module Block3.Task3
 ( parsePSP
 , parseInt
 ) where

import Control.Applicative
import Block3.Task1
import Block3.Task2
import Data.Char

parsePSP :: Parser Char ()
parsePSP = eof <|> (parsePSP' *> eof)

parsePSP' :: Parser Char ()
parsePSP' = leftBr *> emptyPSP *> rightBr *> emptyPSP
  where
    leftBr = element '('
    rightBr = element ')'
    emptyPSP = parsePSP' <|> ok

parseInt :: Parser Char Int
parseInt = fmap read $ concatParsers signParser $ concatParsers intParser $ const [] <$> eof
  where
    signParser = stream "-" <|> (stream "+" *> endl) <|> endl
    intParser = some (satisfy isDigit)
    concatParsers l r = fmap (<>) l <*> r
    endl = fmap (const []) ok
