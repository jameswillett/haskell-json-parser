module Main where

import Control.Applicative
import Data.Char
import Data.Tuple

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Integer
  | JsonFloat Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
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
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true" = JsonBool True
        f _      = JsonBool False

jsonInteger :: Parser JsonValue
jsonInteger = (JsonInteger . read) <$> (notNull $ spanP isDigit)

jsonFloat :: Parser JsonValue
jsonFloat = Parser $ \f -> do
  (rest,   n  ) <- runParser (notNull $ spanP isDigit) f
  (rest',  n' ) <- runParser (charP '.') rest
  (rest'', n'') <- runParser (notNull $ spanP isDigit) rest'
  Just (rest'', JsonFloat $ read $ n ++ [n'] ++ n'')

stringLiteral :: Parser String
stringLiteral = charP '"' *> ((Parser $ (\input -> f (input, ""))) <|> spanP (/='"')) <* charP '"'
  where f (x:xs, parsed) =
          if x == '"' && length parsed > 0 && last parsed /= '\\'
            then Just (x:xs, parsed)
            else f (xs, parsed ++ [x])
        f _ = Nothing

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> els <* ws <* charP ']')
  where sep = ws *> charP ',' <* ws
        els = sepBy sep jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where pair = (\a b c -> (a, c)) <$> stringLiteral <*> (ws *> charP ':' *> ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonFloat <|> jsonInteger <|> jsonString <|> jsonArray <|> jsonObject

parseFileWithParser :: Parser a -> FilePath -> IO (Maybe a)
parseFileWithParser parser path = do
  input <- readFile path
  return (snd <$> runParser parser input)

parseFile :: FilePath -> IO (Maybe JsonValue)
parseFile = parseFileWithParser jsonValue

main :: IO ()
main = undefined
