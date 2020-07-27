# haskell-json-parser

this is an "ideological" fork of [@tsoding](https://github.com/tsoding)'s haskell parser from [this video](https://www.youtube.com/watch?v=N9RUqGYuGfw) (repo [here](https://github.com/tsoding/haskell-json))

i wanted to address the original gotchas of the initial commit without referencing any subsequent PRs on that repo

* escaped double quotes are supported however my initial implementation doesn't support empty strings, so currently there are 2 parsers for strings
* floats are supported. i made a decision to parse them as a separate type from integers because of the difference between the types in haskell. scientific notation is still unsupported
* eventually i want to move `JsonObject` to `Map String JsonValue` because looking up a property via recursion is fucking sloooooow
* there is a `jsonPath` function exposed that takes a list of properties as strings and a `JsonValue` and gives you back a `Maybe JsonValue`. pretty cool!
* also a dump of the coke bust api response to play around with
