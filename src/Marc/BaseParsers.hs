module Marc.BaseParsers where

import           Marc.Char
import           Text.Parsec
import           Text.Parsec.String

numbers :: Int -> Parser Int
numbers n = read <$> count n digit

asciiGraphic :: Parser Char
asciiGraphic = satisfy isAsciiGraphics

asciiGraphicSymbol :: Parser Char
asciiGraphicSymbol = satisfy isAsciiGraphicSymbol

asciiSpace :: Parser Char
asciiSpace = char ' '

identifier :: Parser Char
identifier = lower  <|> digit <|> asciiGraphicSymbol

indicator :: Parser (Maybe Char)
indicator = do
  c <- lower <|> digit <|> asciiSpace
  case c of
    ' ' -> return Nothing
    x   -> return (Just x)

delimiter :: Parser Char
delimiter = char '\US'

fieldTerminator :: Parser Char
fieldTerminator = char '\RS'

recordTerminator :: Parser Char
recordTerminator = char '\GS'
