module Lib where

-- import           Control.Applicative
-- import           Data.Char                    (isAsciiUpper, isDigit)
-- import           Text.ParserCombinators.ReadP

-- data WindInfo = WindInfo
--     { dir   :: Int
--     , speed :: Int
--     , gusts :: Maybe Int
--     } deriving (Eq, Show)

-- data Report = Report
--     { station :: String
--     , time    :: (Int, Int, Int)
--     , wind    :: WindInfo
--     } deriving (Eq, Show)

-- digit :: ReadP Char
-- digit = satisfy isDigit

-- numbers :: Int -> ReadP Int
-- numbers n = read <$> count n digit

-- numbersInRange :: Int -> Int -> Int -> ReadP Int
-- numbersInRange count minValue maxValue =
--     do
--     x <- numbers count
--     if x < minValue || x > maxValue then
--         pfail
--     else
--         return x

-- hour :: ReadP Int
-- hour = numbersInRange 2 0 23

-- day :: ReadP Int
-- day = numbersInRange 2 0 30

-- minute :: ReadP Int
-- minute = numbersInRange 2 0 59

-- airport :: ReadP String
-- airport = do
--     code <- many1 $ satisfy isAsciiUpper
--     return code

-- timestamp :: ReadP (Int, Int, Int)
-- timestamp = do
--     d <- day
--     h <- hour
--     m <- minute
--     _ <- char 'Z'
--     return (d, h, m)

-- gustParser :: ReadP Int
-- gustParser = do
--     _ <- char 'G'
--     numbers 2 <|> numbers 3

-- windInfo :: ReadP WindInfo
-- windInfo = do
--     direction <- numbers 3
--     speed     <- numbers 2 <|> numbers 3
--     gusts     <- option Nothing $ Just <$> gustParser
--     unit      <- string "KT" <|> string "MPS"
--     return $ WindInfo direction (toMPS unit speed) $ fmap (toMPS unit) gusts

-- toMPS :: String -> Int -> Int
-- toMPS unit speed = case unit of
--     "KT"  -> speed `div` 2
--     "MPS" -> speed

-- metar :: ReadP Report
-- metar = do
--     code <- airport
--     _    <- skipSpaces
--     time <- timestamp
--     _    <- skipSpaces
--     wind <- windInfo
--     _    <- skipSpaces
--     return $ Report code time wind

-- parseMetar :: String -> Maybe Report
-- parseMetar = parseMaybe metar

-- parseMaybe :: ReadP a -> String -> Maybe a
-- parseMaybe parser input =
--     case readP_to_S parser input of
--         []              -> Nothing
--         ((result, _):_) -> Just result
