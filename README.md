TOML Parser (Haskell)
=====================

This is an example TOML parser implemented in Haskell using parser combinators, 
inspired by the structure of a JSON parser. It demonstrates how the same 
combinator-based approach can be used to parse other configuration or data 
languages with a complexity level similar to JSON.

Features
--------
- Parses a subset of TOML syntax:
  - Key/value pairs: key = value
  - Basic data types: strings, integers, floats, booleans
  - Arrays: key = [ value, value, ... ]
  - Tables: [table] and nested [table.subtable]
  - Comments starting with '#'
  
- Produces a TomlTable data structure as output, storing nested table keys 
  as dot-separated strings (e.g. owner.age).

Limitations
-----------
- Not fully compliant: This parser is not a fully compliant TOML parser. It is 
  intended as a demonstration of parsing techniques.
- Reduced feature set: Some TOML features, such as inline tables, arrays of 
  tables, dates, and times, are not implemented.
- No error recovery: Error messages are basic and no advanced error recovery 
  mechanisms are in place.

Quick Start
-----------
Requirements:
- GHC (Haskell Compiler)
- Optionally, Cabal or Stack for easier builds.

Running the Parser:
1. Save the provided code as Main.hs in a directory.
2. Compile and run using GHC directly:
   ghc Main.hs
   ./Main
   
   Or run without compiling:
   runhaskell Main.hs

3. The parser will process a hard-coded TOML snippet (defined in the source code) 
   and attempt to parse it.

4. On success, it prints the parsed data structure and checks if it matches the 
   expected result.

Example Output
--------------
[INFO] TOML:
# A TOML file
title = "TOML Example"
number = 42
float_val = 3.1415
array = [1, 2, 3, true, "hello"]

[owner]
name = "Alice"
age = 30

[database]
type = "sql"
enabled = true

[INFO] Parsed as: TomlTable [("title",TomlString "TOML Example"),("number",TomlInt 42),("float_val",TomlFloat 3.1415),("array",TomlArray [TomlInt 1,TomlInt 2,TomlInt 3,TomlBool True,TomlString "hello"]),("owner.name",TomlString "Alice"),("owner.age",TomlInt 30),("database.type",TomlString "sql"),("database.enabled",TomlBool True)]
[INFO] Remaining input (codes): []
[SUCCESS] Parser produced expected result.
