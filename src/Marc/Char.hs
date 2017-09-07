module Marc.Char where

import           Data.Char (isAsciiLower, isNumber, ord)


isAsciiGraphicSymbol :: Char -> Bool
isAsciiGraphicSymbol c =
     o `elem` [0x21..0x2F]
  || o `elem` [0x3A..0x40]
  || o `elem` [0x5B..0x60]
  || o `elem` [0x7B..0x7E]
  where
    o = ord c

isAsciiGraphics :: Char -> Bool
isAsciiGraphics c = ord c `elem` [0x20..0X7E]

isAsciiSpace :: Char -> Bool
isAsciiSpace = (== ' ')

isAsciiDelete :: Char -> Bool
isAsciiDelete = (== 0x7F) . ord

isFill :: Char -> Bool
isFill = (== '|')

isFieldTerminator :: Char -> Bool
isFieldTerminator = (== '\RS')

isDataElementId :: Char -> Bool
isDataElementId c = (isAsciiLower c || isNumber c || isAsciiGraphicSymbol c) && not (isAsciiSpace c)

isAsciiUnitSeparator :: Char -> Bool
isAsciiUnitSeparator = (== '\US')

isAsciiGroupSeparator :: Char -> Bool
isAsciiGroupSeparator = (== '\GS')

