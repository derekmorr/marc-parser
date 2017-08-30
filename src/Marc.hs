module Marc where

import           Control.Applicative
import           Data.Char                    (isAsciiLower, isAsciiUpper,
                                               isDigit, isNumber, ord)
import           Text.ParserCombinators.ReadP

data Encoding = MARC8 | UTF8 deriving (Eq, Show)

data Status = Increase
            | Corrected
            | Deleted
            | New
            | Obsolete
            | IncreaseFromPrePublication
            | DeletedSplit
            | DeletedReplaced
            deriving (Eq, Show)

data RecordType = Authority
                | Cartographic
                | Classification
                | ComputerFile
                | Kit
                | LanguageMaterial
                | ManuscriptLanguage
                | ManuscriptCartographicMaterial
                | ManuscriptNotatedMusic
                | MixedMaterials
                | Multipart
                | MusicalSoundRecording
                | NonMusicalSoundRecording
                | NotatedMusic
                | ProjectedMedium
                | Singlepart
                | Serial
                | ThreeDimensionalArtifact
                | TwoDimensionalNonProjectableGraphic
                | Unknown
                deriving (Eq, Show)

-- see https://www.loc.gov/marc/bibliographic/bdleader.html

-- skips the entry map field since it's hardcoded
data Leader = Leader
  { recordLength :: Int
  , status       :: Status
  , typeOfRecord :: RecordType
  , implDefined  :: String
  , encoding     :: Encoding
  , baseAddress  :: Int
  , implDefined2 :: String
  } deriving (Eq, Show)

data DirEntry = DirEntry
  { tag           :: Int
  , lengthOfField :: Int
  , startCharPos  :: Int
  } deriving (Eq, Show)

data ControlField = ControlField
  { tagId       :: Int
  , controlData :: String
  } deriving (Eq, Show)

data DataElement = DataElement
  { dataElementId :: Char
  , dataField     :: String
  } deriving (Eq, Show)

data VariableDataField = VariableDataField
  { indicator1   :: Char
  , indicator2   :: Char
  , dataElements :: [DataElement]
  } deriving (Eq, Show)

data Marc21Record = Marc21Record
  { leader        :: Leader
  , dirEntries    :: [DirEntry]
  , controlFields :: [ControlField]
  } deriving (Eq, Show)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers n = read <$> count n digit

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
isFieldTerminator = (== 0x1E) . ord

isDataElementId :: Char -> Bool
isDataElementId c = (isAsciiLower c || isNumber c || isAsciiGraphicSymbol c) && not (isAsciiSpace c)

isAsciiUnitSeparator :: Char -> Bool
isAsciiUnitSeparator = (== 0x1F) . ord

isAsciiGroupSeparator :: Char -> Bool
isAsciiGroupSeparator = (== 0x1D) . ord

-- isAsciiRecordSeparator :: Char -> Bool
-- isAsciiRecordSeparator = (== 0x30) . ord

parseAsciiGraphic :: ReadP Char
parseAsciiGraphic = satisfy isAsciiGraphics

parseEncoding :: ReadP Encoding
parseEncoding = do
  e <- get
  case e of
    ' ' -> return MARC8
    'a' -> return UTF8
    _   -> pfail

parseStatus :: ReadP Status
parseStatus = do
  s <- get
  case s of
    'a' -> return Increase
    'c' -> return Corrected
    'd' -> return Deleted
    'n' -> return New
    'o' -> return Obsolete
    'p' -> return IncreaseFromPrePublication
    's' -> return DeletedSplit
    'x' -> return DeletedReplaced
    _   -> pfail

parseType :: ReadP RecordType
parseType = do
  s <- get
  case s of
    'a' -> return LanguageMaterial
    'c' -> return NotatedMusic
    'd' -> return ManuscriptNotatedMusic
    'e' -> return Cartographic
    'f' -> return ManuscriptCartographicMaterial
    'g' -> return ProjectedMedium
    'i' -> return NonMusicalSoundRecording
    'j' -> return MusicalSoundRecording
    'k' -> return TwoDimensionalNonProjectableGraphic
    'm' -> return ComputerFile
    'o' -> return Kit
    'p' -> return MixedMaterials
    'r' -> return ThreeDimensionalArtifact
    't' -> return ManuscriptLanguage
    'u' -> return Unknown
    'v' -> return Multipart
    'w' -> return Classification
    'x' -> return Singlepart
    'y' -> return Serial
    'z' -> return Authority
    _   -> pfail

parseLeader :: ReadP Leader
parseLeader = do
  l  <- numbers 5
  s  <- parseStatus
  t  <- parseType
  i1 <- count 2 get
  e  <- parseEncoding
  _  <- char '2'
  _  <- char '2'
  b  <- numbers 5
  i2 <- count 3 parseAsciiGraphic
  _  <- char '4'
  _  <- char '5'
  _  <- char '0'
  _  <- char '0'
  return $ Leader l s t i1 e b i2

parseDirEntry :: ReadP DirEntry
parseDirEntry = do
  t <- numbers 3
  l <- numbers 4
  s <- numbers 5
  return $ DirEntry t l s

parseMarc21Record :: ReadP Marc21Record
parseMarc21Record = do
    lead    <- parseLeader
    dirs    <- many1 parseDirEntry
    _       <- satisfy isFieldTerminator
    cfields <- getCF dirs
    -- let cfields = parseControlField <$> getControlFields dirs
    -- f <- cfields
    return $ Marc21Record lead dirs cfields

getCF :: [DirEntry] -> ReadP [ControlField]
getCF dirents = sequence $ parseControlField <$> getControlFields dirents

getControlFields :: [DirEntry] -> [DirEntry]
getControlFields = filter ((< 10) . tag)

testFunc :: String -> IO [(Marc21Record, String)]
testFunc filepath = do
  s <- readFile filepath
  let j = readP_to_S parseMarc21Record s
  return j

parseControlField :: DirEntry -> ReadP ControlField
parseControlField dirEnt = do
  content <- many1 get
  _       <- satisfy isFieldTerminator
  return $ ControlField (tag dirEnt) content

parseDataElement :: ReadP DataElement
parseDataElement = do
  identifier  <- satisfy isAsciiLower <|> satisfy isAsciiLower
  dataElement <- many1 get
  _           <- satisfy isFieldTerminator
  return $ DataElement identifier dataElement

-- parseVariableField :: DirEntry ->ReadP VariableDataField
-- parseVariableField dirEntry = do
--   i1 <- get
--   i2 <- get
--   ?  <- get?
--   _  <- satisfy isFieldTerminator
--   VariableDataField
