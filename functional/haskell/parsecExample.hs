import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = cell `sepBy` (char ',')
cell = do
  values <-many (noneOf ",\n")
  return values
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <|> fail "Couldn't find EOL"



parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
