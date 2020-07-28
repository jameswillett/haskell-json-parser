module Main where

import Control.Applicative (Alternative, empty, (<|>), many)
import Data.Char (isSpace, isDigit)
import Data.Tuple (swap)
import Text.Read (readMaybe)

data JsonValue
  = JsonNull
  | JsonBool    Bool
  | JsonInteger Integer
  | JsonFloat   Float
  | JsonString  String
  | JsonArray   [JsonValue]
  | JsonObject  [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $
    \input -> do
      (rest, result) <- p input
      Just (rest, f result)

instance Applicative Parser where
  pure x = Parser $ \rest -> Just (rest, x)
  (Parser p1) <*> (Parser p2) = Parser $
    \input -> do
      (rest,  f) <- p1 input
      (rest', x) <- p2 rest
      Just (rest', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ (<|>) <$> p1 <*> p2

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ swap $ span f input

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $
  \input -> do
     (rest, x) <- p input
     if null x
       then Nothing
       else Just (rest, x)

stringP :: String -> Parser String
stringP = sequenceA . map charP

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | x == c = Just (xs, c)
        f _               = Nothing

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:)
  <$> element
  <*> many (sep *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true" = JsonBool True
        f _      = JsonBool False

maybeNegate :: Num a => Char -> (a -> a)
maybeNegate ('-') = negate -- kirby says `negate`
maybeNegate _     = id

constP :: a -> Parser a
constP s = Parser $ \i -> Just (i, s)

signP :: Parser Char
signP = charP '-' <|> constP '+'

notNullDigits :: Parser String
notNullDigits = notNull $ spanP isDigit

getExponent :: Parser String
getExponent = ((charP 'e' <|> charP 'E') *> notNullDigits) <|> constP "0"

jsonInteger :: Parser JsonValue
jsonInteger = JsonInteger <$>
  ((\s int ex ->
      toInteger
      $ round
      $ maybeNegate s
      $ (read (int ++ "e" ++ ex) :: Float))
    <$> signP
    <*> notNullDigits
    <*> getExponent)

jsonFloat :: Parser JsonValue
jsonFloat = JsonFloat <$>
  ((\s big dot little ex ->
      maybeNegate s
      $ read
      $ big ++ [dot] ++ little ++ "e" ++ ex)
    <$> signP
    <*> notNullDigits
    <*> (charP '.')
    <*> notNullDigits
    <*> getExponent)

stringLiteral :: Parser String
stringLiteral =
  charP '"' *>
  ((Parser $ (\input -> f (input, ""))) <|> spanP (/='"'))
  <* charP '"'
  where f (x:xs, parsed) =
          if x == '"' && length parsed > 0 && last parsed /= '\\'
            then Just (x:xs, parsed)
            else f (xs, parsed ++ [x])
        f _ = Nothing

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$>
  (charP '[' *> ws *> els <* ws <* charP ']')
  where sep = ws *> charP ',' <* ws
        els = sepBy sep jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
    (charP '{' *> ws *>
    sepBy (ws *> charP ',' <* ws)
    pair <* ws <* charP '}')
  where pair = (\key _ value -> (key, value))
          <$> stringLiteral
          <*> (ws *> charP ':' *> ws)
          <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull    <|>
  jsonBool    <|>
  jsonFloat   <|>
  jsonInteger <|>
  jsonString  <|>
  jsonArray   <|>
  jsonObject

parseFileWithParser :: Parser a -> FilePath -> IO (Maybe a)
parseFileWithParser parser path = do
  input <- readFile path
  return (snd <$> runParser parser input)

parseFile :: FilePath -> IO (Maybe JsonValue)
parseFile = parseFileWithParser jsonValue

jsonPath :: [String] -> JsonValue -> Maybe JsonValue
jsonPath []     a = Just a
jsonPath (p:ps) j =
  case j of
    JsonArray  a -> findArray  p a >>= returning
    JsonObject o -> findObject p o >>= returning
    _            -> Nothing
  where findObject :: String -> [(String, JsonValue)] -> Maybe JsonValue
        findObject p l
          | length l == 0       = Nothing
          | (fst $ head l) == p = Just (snd $ head l)
          | otherwise           = findObject p $ tail l

        findArray :: String -> [JsonValue] -> Maybe JsonValue
        findArray p l
          | length l == 0 = Nothing
          | otherwise     = (readMaybe p :: Maybe Int) >>= safeIndex l

        safeIndex :: [JsonValue] -> Int -> Maybe JsonValue
        safeIndex a i
          | i >= (length a) = Nothing
          | otherwise       = Just $ a !! i

        returning :: JsonValue -> Maybe JsonValue
        returning
          | length ps == 0 = return
          | otherwise      = jsonPath ps

main :: IO ()
main = undefined

