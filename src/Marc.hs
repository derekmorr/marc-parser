module Marc where

-- import           Control.Applicative
import           Data.Char          (isAsciiLower, isDigit)
import           Marc.BaseParsers
import           Marc.Char
import           Text.Parsec
import           Text.Parsec.String

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
  { indicator1   :: Maybe Char
  , indicator2   :: Maybe Char
  , dataElements :: [DataElement]
  } deriving (Eq, Show)

data Marc21Record = Marc21Record
  { leader        :: Leader
  , dirEntries    :: [DirEntry]
  , controlFields :: [ControlField]
  , varFields     :: [VariableDataField]
  } deriving (Eq, Show)

parseEncoding :: Parser Encoding
parseEncoding = do
  e <- anyChar
  case e of
    ' ' -> return MARC8
    'a' -> return UTF8
    _   -> fail "Invalid encoding"

parseStatus :: Parser Status
parseStatus = do
  s <- anyChar
  case s of
    'a' -> return Increase
    'c' -> return Corrected
    'd' -> return Deleted
    'n' -> return New
    'o' -> return Obsolete
    'p' -> return IncreaseFromPrePublication
    's' -> return DeletedSplit
    'x' -> return DeletedReplaced
    _   -> fail "Invalid status"

parseType :: Parser RecordType
parseType = do
  s <- anyChar
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
    _   -> fail "Invalid type"

parseLeader :: Parser Leader
parseLeader = do
  l  <- numbers 5
  s  <- parseStatus
  t  <- parseType
  i1 <- count 2 anyChar
  e  <- parseEncoding
  _  <- char '2'
  _  <- char '2'
  b  <- numbers 5
  i2 <- count 3 asciiGraphic
  _  <- char '4'
  _  <- char '5'
  _  <- char '0'
  _  <- char '0'
  return $ Leader l s t i1 e b i2

parseDirEntry :: Parser DirEntry
parseDirEntry = do
  t <- numbers 3
  l <- numbers 4
  s <- numbers 5
  return $ DirEntry t l s

-- body :: Parser [VariableDataField]
-- body = endBy vfields (satisfy isAsciiGroupSeparator)

-- vfields :: Parser [VariableDataField]
-- vfields = sepBy parseVariableField parseFieldTerminator

parseMarc21Record :: Parser Marc21Record
parseMarc21Record = do
  lead    <- parseLeader
  dirs    <- many1 parseDirEntry
  _       <- fieldTerminator
  cfields <- getCF dirs
  -- vfields <- p5
  -- body    <- endBy (satisfy isAsciiGroupSeparator)
  -- elems   <-
  -- vfields <- sepBy parseVariableField fieldTerminator --many1 parseVariableField
  vfields <- endBy parseVariableField fieldTerminator
  _       <- recordTerminator
    -- let cfields = parseControlField <$> getControlFields dirs
    -- f <- cfields
  return $ Marc21Record lead dirs cfields vfields

getCF :: [DirEntry] -> Parser [ControlField]
getCF dirents = sequence $ parseControlField <$> getControlFields dirents

getControlFields :: [DirEntry] -> [DirEntry]
getControlFields = filter ((< 10) . tag)

testFunc :: String -> IO (Either ParseError Marc21Record)
testFunc filepath = do
  s <- readFile filepath
  return $ runParser parseMarc21Record () filepath s

parseControlField :: DirEntry -> Parser ControlField
parseControlField dirEnt = do
  content <- many1 $ noneOf "\RS"
  _       <- fieldTerminator
  return $ ControlField (tag dirEnt) content

parseDataElement :: Parser DataElement
parseDataElement = do
  i           <- identifier
  dataElement <- many1 $ noneOf "\US\RS\GS"
  return $ DataElement i dataElement

parseVariableField :: Parser VariableDataField
parseVariableField = do
  i1    <- indicator
  i2    <- indicator
  _     <- delimiter
  elems <- sepBy1 parseDataElement delimiter
  -- _     <- fieldTerminator
  return $ VariableDataField i1 i2 elems


-- p4 :: Parser [String]
-- p4 = do
--   str <- sepBy1 (many1 $ noneOf "\GS") recordTerminator
--   _   <- recordTerminator
--   return str

-- p5 :: Parser [VariableDataField]
-- p5 = do
--   elems <- sepBy1 parseVariableField recordTerminator
--   -- _     <- recordTerminator
--   return elems

p6 :: Parser [String]
p6 = endBy marcrec recordTerminator
  where
    marcrec = many1 $ noneOf "\GS"

-- ft :: Parser Char
-- ft = char '\RS'

-- rt :: Parser Char
-- rt = char '\GS'

-- marcFile :: Parser [[String]]
-- marcFile = endBy marc rt

-- marc :: Parser [String]
-- marc = sepBy cell delimiter

-- cell :: Parser String
-- cell = many (noneOf "\GS\RS")
