import           Data.Maybe
import           Marc
import           System.IO
import           Test.Hspec
import           Text.ParserCombinators.ReadP

expectedMarc1b :: Marc21Record
expectedMarc1b = Marc21Record
  { leader = Leader {recordLength = 2666, status = New, typeOfRecord = LanguageMaterial, implDefined = "m ", encoding = UTF8, baseAddress = 469, implDefined2 = " a "}, dirEntries = [DirEntry {tag = 1, lengthOfField = 12, startCharPos = 0},DirEntry {tag = 5, lengthOfField = 17, startCharPos = 12},DirEntry {tag = 6, lengthOfField = 19, startCharPos = 29},DirEntry {tag = 7, lengthOfField = 7, startCharPos = 48},DirEntry {tag = 8, lengthOfField = 41, startCharPos = 55},DirEntry {tag = 37, lengthOfField = 84, startCharPos = 96},DirEntry {tag = 40, lengthOfField = 23, startCharPos = 180},DirEntry {tag = 42, lengthOfField = 8, startCharPos = 203},DirEntry {tag = 43, lengthOfField = 12, startCharPos = 211},DirEntry {tag = 74, lengthOfField = 20, startCharPos = 223},DirEntry {tag = 86, lengthOfField = 23, startCharPos = 243},DirEntry {tag = 88, lengthOfField = 15, startCharPos = 266},DirEntry {tag = 110, lengthOfField = 54, startCharPos = 281},DirEntry {tag = 245, lengthOfField = 291, startCharPos = 335},DirEntry {tag = 246, lengthOfField = 177, startCharPos = 626},DirEntry {tag = 246, lengthOfField = 122, startCharPos = 803},DirEntry {tag = 260, lengthOfField = 73, startCharPos = 925},DirEntry {tag = 336, lengthOfField = 22, startCharPos = 998},DirEntry {tag = 337, lengthOfField = 24, startCharPos = 1020},DirEntry {tag = 338, lengthOfField = 33, startCharPos = 1044},DirEntry {tag = 500, lengthOfField = 53, startCharPos = 1077},DirEntry {tag = 500, lengthOfField = 21, startCharPos = 1130},DirEntry {tag = 500, lengthOfField = 120, startCharPos = 1151},DirEntry {tag = 500, lengthOfField = 18, startCharPos = 1271},DirEntry {tag = 504, lengthOfField = 41, startCharPos = 1289},DirEntry {tag = 538, lengthOfField = 196, startCharPos = 1330},DirEntry {tag = 610, lengthOfField = 92, startCharPos = 1526},DirEntry {tag = 650, lengthOfField = 75, startCharPos = 1618},DirEntry {tag = 650, lengthOfField = 69, startCharPos = 1693},DirEntry {tag = 710, lengthOfField = 60, startCharPos = 1762},DirEntry {tag = 710, lengthOfField = 114, startCharPos = 1822},DirEntry {tag = 856, lengthOfField = 65, startCharPos = 1936},DirEntry {tag = 907, lengthOfField = 35, startCharPos = 2001},DirEntry {tag = 998, lengthOfField = 45, startCharPos = 2036},DirEntry {tag = 910, lengthOfField = 12, startCharPos = 2081},DirEntry {tag = 910, lengthOfField = 28, startCharPos = 2093},DirEntry {tag = 945, lengthOfField = 75, startCharPos = 2121}], controlFields = [ControlField {tagId = 1, controlData = "ocm57177924"},ControlField {tagId = 5, controlData = "20041207065359.0"},ControlField {tagId = 6, controlData = "m        d f      "},ControlField {tagId = 7, controlData = "cr mn-"},ControlField {tagId = 8, controlData = "041207s2004    dcu     s    f000 0 eng c"}]}

parseMarc :: String -> Maybe Marc21Record
parseMarc = parseMaybe parseMarc21Record

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        []              -> Nothing
        ((result, _):_) -> Just result

withFileSpec :: FilePath -> (String -> IO r) -> IO r
withFileSpec filename spec =
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    spec contents

main :: IO ()
main = hspec $ do
  describe "MARC parser should" $ do

    it "parse valid input 1a" $ do
      withFileSpec "data/marc1a.mrc" $ \s ->
        isJust (parseMarc s) `shouldBe` True

    it "parse valid input 1b" $ do
      withFileSpec "data/marc1b.mrc" $ \s ->
        parseMarc s `shouldBe` Just expectedMarc1b
