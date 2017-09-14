module Marc.BaseParsers where

-- import           Control.Applicative
import           Data.Char
import           Marc.Char
-- import           Text.ParserCombinators.ReadP
import           Text.Parsec
import           Text.Parsec.Prim
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
identifier = lower
         <|> digit
         <|> asciiGraphicSymbol

validIndicator :: Parser Char
validIndicator = lower
             <|> digit
             <|> asciiSpace

indicator :: Parser (Maybe Char)
indicator = do
  c <- validIndicator
  case c of
    ' ' -> return Nothing
    x   -> return (Just x)

delimiter :: Parser Char
delimiter = char '\US'

fieldTerminator :: Parser Char
fieldTerminator = char '\RS'

recordTerminator :: Parser Char
recordTerminator = char '\GS'
