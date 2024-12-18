{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Applicative
import           Data.Char
import           Numeric
import           System.Exit

data Input = Input
  { inputLoc :: Int
  , inputStr :: String
  } deriving (Show, Eq)

-- | Pull the first character of the input if there is one
inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ [])       = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

data TomlValue
  = TomlString String
  | TomlInt Integer
  | TomlFloat Double
  | TomlBool Bool
  | TomlArray [TomlValue]
  | TomlTable [(String, TomlValue)]
  deriving (Show, Eq)

data ParserError = ParserError Int String deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
        Left $
        ParserError
          (inputLoc input)
          ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left $
      ParserError
        (inputLoc input)
        ("Expected '" ++ [x] ++ "', but reached end of string")

stringP :: String -> Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected \"" ++ str ++ "\", but found \"" ++ inputStr input ++ "\"")
      result -> result

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputLoc input)
            ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " ++ desc ++ ", but reached end of string")

spanP :: String -> (Char -> Bool) -> Parser String
spanP desc = many . parseIf desc

ws :: Parser String
ws = spanP "whitespace" isSpace

comment :: Parser ()
comment = do
  charP '#'
  _ <- spanP "any character" (/= '\n')
  return ()

-- | Consume whitespace and comments
wsAndComments :: Parser ()
wsAndComments = do
  _ <- many (ws *> optional comment)
  _ <- ws
  return ()

-- | Basic TOML values
tomlBool :: Parser TomlValue
tomlBool = (TomlBool True <$ stringP "true") <|> (TomlBool False <$ stringP "false")

tomlString :: Parser TomlValue
tomlString = TomlString <$> (charP '"' *> many normalChar <* charP '"')
  where
    normalChar = parseIf "non-quote char" (/= '"')

digits :: Parser String
digits = some (parseIf "digit" isDigit)

tomlNumber :: Parser TomlValue
tomlNumber = tryFloat <|> tryInt
  where
    tryFloat = do
      -- Float: integral part, dot, fractional part, optional exponent
      integral <- digits
      charP '.'
      frac <- digits
      expo <- optional exponentPart
      let val = read (integral ++ "." ++ frac ++ maybe "" id expo) :: Double
      return (TomlFloat val)

    tryInt = do
      numStr <- digits
      return (TomlInt (read numStr))

    exponentPart = do
      e <- charP 'e' <|> charP 'E'
      sign <- optional (charP '+' <|> charP '-')
      expoDigits <- digits
      return (e : maybe "" (:[]) sign ++ expoDigits)

-- | Arrays: key = [ value, value, ... ]
tomlArray :: Parser TomlValue
tomlArray = TomlArray <$> (charP '[' *> wsAndComments *> sepBy (charP ',' *> wsAndComments) tomlValue <* wsAndComments <* charP ']')

-- | Tables: [table], [table.subtable]
-- We'll store tables as TomlTable. For simplicity, parse them into a flat structure.
tableHeader :: Parser [String]
tableHeader = charP '[' *> wsAndComments *> sepBy (charP '.' *> wsAndComments) keyName <* wsAndComments <* charP ']'

keyName :: Parser String
keyName = bareKey <|> quotedKey
  where
    bareKey = some (parseIf "bare key char" (\c -> isAlphaNum c || c == '_' || c == '-'))
    quotedKey = charP '"' *> many (parseIf "non-quote char" (/= '"')) <* charP '"'

-- | Key-Value pair: key = value
keyValuePair :: Parser (String, TomlValue)
keyValuePair = do
  k <- keyName
  wsAndComments
  charP '='
  wsAndComments
  v <- tomlValue
  return (k, v)

-- | Parse a list of key-value pairs for a table
tableBody :: Parser [(String, TomlValue)]
tableBody = many (wsAndComments *> keyValuePair <* wsAndComments)

-- | A top-level TOML file has zero or more key-values and tables
tomlFile :: Parser TomlValue
tomlFile = do
  globalKVs <- tableBody
  tables <- many parseTable
  return (TomlTable (globalKVs ++ concat tables))
  where
    parseTable = do
      wsAndComments
      header <- tableHeader
      kvs <- tableBody
      -- For simplicity, just store them directly. A full TOML parser would nest these.
      let prefix = concatMap (++ ".") (init header) ++ last header
      return (map (\(k,v) -> (prefix ++ "." ++ k, v)) kvs)

-- | Try parsing a given parser, but if it fails, restore input (used in number parsing)
tryP :: Parser a -> Parser a
tryP (Parser p) = Parser $ \input ->
  case p input of
    Left _  -> Left (ParserError (inputLoc input) "tryP failed")
    Right r -> Right r

tryFloat :: Parser TomlValue
tryFloat = tryP (do
  integral <- digits
  charP '.'
  frac <- digits
  expo <- optional exponentPart
  return (TomlFloat (read (integral ++ "." ++ frac ++ maybe "" id expo))))

exponentPart :: Parser String
exponentPart = do
  e <- charP 'e' <|> charP 'E'
  sign <- optional (charP '+' <|> charP '-')
  expoDigits <- digits
  return (e : maybe "" (:[]) sign ++ expoDigits)

-- | Combine all value parsers
tomlValue :: Parser TomlValue
tomlValue =
  wsAndComments *>
  (tomlBool <|> tomlString <|> tomlArray <|> tomlNumber)
  <* wsAndComments

-- | sepBy combinator
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- | Main function: parse a sample TOML input
main :: IO ()
main = do
  putStrLn "[INFO] TOML:"
  putStrLn testTomlText
  case runParser tomlFile $ Input 0 testTomlText of
    Right (input, actualTomlAst) -> do
      putStrLn ("[INFO] Parsed as: " ++ show actualTomlAst)
      putStrLn ("[INFO] Remaining input (codes): " ++ show (map ord $ inputStr input))
      if actualTomlAst == expectedTomlAst
        then putStrLn "[SUCCESS] Parser produced expected result."
        else do
          putStrLn
            ("[ERROR] Parser produced unexpected result. Expected result was: " ++
             show expectedTomlAst)
          exitFailure
    Left (ParserError loc msg) -> do
      putStrLn $
        "[ERROR] Parser failed at character " ++ show loc ++ ": " ++ msg
      exitFailure
  where
    testTomlText =
      unlines
        [ "# A TOML file"
        , "title = \"TOML Example\""
        , "number = 42"
        , "float_val = 3.1415"
        , "array = [1, 2, 3, true, \"hello\"]"
        , ""
        , "[owner]"
        , "name = \"Alice\""
        , "age = 30"
        , ""
        , "[database]"
        , "type = \"sql\""
        , "enabled = true"
        ]
    expectedTomlAst =
      TomlTable
        [ ("title", TomlString "TOML Example")
        , ("number", TomlInt 42)
        , ("float_val", TomlFloat 3.1415)
        , ("array", TomlArray [TomlInt 1, TomlInt 2, TomlInt 3, TomlBool True, TomlString "hello"])
        , ("owner.name", TomlString "Alice")
        , ("owner.age", TomlInt 30)
        , ("database.type", TomlString "sql")
        , ("database.enabled", TomlBool True)
        ]

