module Marc.BaseParsers where

-- import           Control.Applicative
import           Data.Char
import           Marc.Char
-- import           Text.ParserCombinators.ReadP
import           Text.Parsec
import           Text.Parsec.Prim
import           Text.Parsec.String

-- oneOf :: String -> ReadP Char
-- oneOf str = satisfy $ flip elem str

-- noneOf :: String -> ReadP Char
-- noneOf str = satisfy $ flip notElem str

-- digit :: ReadP Char
-- digit = satisfy isDigit

numbers :: Int -> Parser Int
numbers n = read <$> count n digit

-- asciiGraphic :: ReadP Char
asciiGraphic :: Parser Char
asciiGraphic = satisfy isAsciiGraphics

asciiGraphicSymbol :: Parser Char
asciiGraphicSymbol = satisfy isAsciiGraphicSymbol

asciiSpace :: Parser Char
asciiSpace = char ' ' --satisfy isAsciiSpace

-- identifier :: ReadP Char
identifier :: Parser Char
identifier = lower -- satisfy isAsciiLower
         <|> digit -- satisfy isDigit
         <|> asciiGraphicSymbol


-- validIndicator :: ReadP Char
validIndicator :: Parser Char
validIndicator = lower -- satisfy isAsciiLower
             <|> digit
             <|> asciiSpace

indicator :: Parser (Maybe Char)
indicator = do
  c <- validIndicator
  return $ f c
  where
    f ' ' = Nothing
    f x   = Just x

delimiter :: Parser Char
delimiter = char '\US' --satisfy isAsciiUnitSeparator

fieldTerminator :: Parser Char
fieldTerminator = char '\RS' --satisfy isFieldTerminator

recordTerminator :: Parser Char
recordTerminator = char '\GS' -- satisfy isAsciiGroupSeparator
