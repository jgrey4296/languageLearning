import Text.Parsec as P
import Data.Map

data Operator = DOT | EX deriving (Eq)

instance Show Operator where
  show DOT = "."
  show EX = "!"

data Fact = Fact [(String, Operator)] deriving (Show, Eq)

bangP :: Stream s m Char => ParsecT s u m Operator
bangP = (string "!" *> return EX)
        <|> (string "."  *> return DOT)

clauseP :: Stream s m Char => ParsecT s u m (String, Operator)
clauseP = do
  a <- P.many P.alphaNum
  b <- bangP
  return (a, b)

factP :: Stream s m Char => ParsecT s u m Fact
factP = do
  clauses <- many clauseP
  return $ Fact clauses
