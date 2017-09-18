import           Data.Maybe
import           Marc
import           Marc.BaseParsers
import           Marc.Char
import           System.IO
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.String

expectedMarc1b :: Marc21Record
expectedMarc1b = Marc21Record
  { leader = Leader
        { recordLength = 2666
        , status = New
        , typeOfRecord = LanguageMaterial
        , implDefined = "m "
        , encoding = UTF8
        , baseAddress = 469
        , implDefined2 = " a "
        }
  , dirEntries =
    [ DirEntry {tag = 1, lengthOfField = 12, startCharPos = 0}
    , DirEntry {tag = 5, lengthOfField = 17, startCharPos = 12}
    , DirEntry {tag = 6, lengthOfField = 19, startCharPos = 29}
    , DirEntry {tag = 7, lengthOfField = 7, startCharPos = 48}
    , DirEntry {tag = 8, lengthOfField = 41, startCharPos = 55}
    , DirEntry {tag = 37, lengthOfField = 84, startCharPos = 96}
    , DirEntry {tag = 40, lengthOfField = 23, startCharPos = 180}
    , DirEntry {tag = 42, lengthOfField = 8, startCharPos = 203}
    , DirEntry {tag = 43, lengthOfField = 12, startCharPos = 211}
    , DirEntry {tag = 74, lengthOfField = 20, startCharPos = 223}
    , DirEntry {tag = 86, lengthOfField = 23, startCharPos = 243}
    , DirEntry {tag = 88, lengthOfField = 15, startCharPos = 266}
    , DirEntry {tag = 110, lengthOfField = 54, startCharPos = 281}
    , DirEntry {tag = 245, lengthOfField = 291, startCharPos = 335}
    , DirEntry {tag = 246, lengthOfField = 177, startCharPos = 626}
    , DirEntry {tag = 246, lengthOfField = 122, startCharPos = 803}
    , DirEntry {tag = 260, lengthOfField = 73, startCharPos = 925}
    , DirEntry {tag = 336, lengthOfField = 22, startCharPos = 998}
    , DirEntry {tag = 337, lengthOfField = 24, startCharPos = 1020}
    , DirEntry {tag = 338, lengthOfField = 33, startCharPos = 1044}
    , DirEntry {tag = 500, lengthOfField = 53, startCharPos = 1077}
    , DirEntry {tag = 500, lengthOfField = 21, startCharPos = 1130}
    , DirEntry {tag = 500, lengthOfField = 120, startCharPos = 1151}
    , DirEntry {tag = 500, lengthOfField = 18, startCharPos = 1271}
    , DirEntry {tag = 504, lengthOfField = 41, startCharPos = 1289}
    , DirEntry {tag = 538, lengthOfField = 196, startCharPos = 1330}
    , DirEntry {tag = 610, lengthOfField = 92, startCharPos = 1526}
    , DirEntry {tag = 650, lengthOfField = 75, startCharPos = 1618}
    , DirEntry {tag = 650, lengthOfField = 69, startCharPos = 1693}
    , DirEntry {tag = 710, lengthOfField = 60, startCharPos = 1762}
    , DirEntry {tag = 710, lengthOfField = 114, startCharPos = 1822}
    , DirEntry {tag = 856, lengthOfField = 65, startCharPos = 1936}
    , DirEntry {tag = 907, lengthOfField = 35, startCharPos = 2001}
    , DirEntry {tag = 998, lengthOfField = 45, startCharPos = 2036}
    , DirEntry {tag = 910, lengthOfField = 12, startCharPos = 2081}
    , DirEntry {tag = 910, lengthOfField = 28, startCharPos = 2093}
    , DirEntry {tag = 945, lengthOfField = 75, startCharPos = 2121}
    ]
  , controlFields =
    [ ControlField {tagId = 1, controlData = "ocm57177924"}
    , ControlField {tagId = 5, controlData = "20041207065359.0"}
    , ControlField {tagId = 6, controlData = "m        d f      "}
    , ControlField {tagId = 7, controlData = "cr mn-"}
    , ControlField {tagId = 8, controlData = "041207s2004    dcu     s    f000 0 eng c"}
    ]
  , varFields =
    [ VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'b', dataField = "GAO (202)512-6000 (voice); (202)512-6061 (Fax); (202)512-2537 (TDD)"},DataElement {dataElementId = 'f', dataField = "paper copy"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "GPO"},DataElement {dataElementId = 'c', dataField = "GPO"},DataElement {dataElementId = 'd', dataField = "MvI"},DataElement {dataElementId = 'd', dataField = "MvI"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "pcc"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "n-us---"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "0546-D (online)"}]},VariableDataField {indicator1 = Just '0', indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "GA 1.13:GAO-05-126"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "GAO-05-126"}]},VariableDataField {indicator1 = Just '1', indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "United States."},DataElement {dataElementId = 'b', dataField = "Government Accountability Office."}]},VariableDataField {indicator1 = Just '1', indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'a', dataField = "Aviation security"},DataElement {dataElementId = 'h', dataField = "[electronic resource] :"},DataElement {dataElementId = 'b', dataField = "preliminary observations on TSA's progress to allow airports to use private passenger and baggage screening services : report to the Chairman, Subcommittee on Aviation, Committee on Transportation and Infrastructure, House of Representatives."}]},VariableDataField {indicator1 = Just '3', indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Aviation security :"},DataElement {dataElementId = 'b', dataField = "preliminary observations on Tranportation Security Administration's progress to allow airports to use private passenger and baggage screening services."}]},VariableDataField {indicator1 = Just '3', indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'a', dataField = "Preliminary observations on TSA's progress to allow airports to use private passenger and baggage screening services."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "[Washington, D.C.] :"},DataElement {dataElementId = 'b', dataField = "U.S. Government Accountability Office,"},DataElement {dataElementId = 'c', dataField = "[2004]"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "text"},DataElement {dataElementId = '2', dataField = "rdacontent."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "computer"},DataElement {dataElementId = '2', dataField = "rdamedia."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "online resource"},DataElement {dataElementId = '2', dataField = "rdacarrier."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Title from title screen (viewed on Dec. 2, 2004)"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "\"November 2004.\""}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Paper version available from: U.S. Government Accountability Office, 441 G St., NW, Rm. LM, Washington, D.C. 20548."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "\"GAO-05-126.\""}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Includes bibliographical references."}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Mode of access: Internet from GPO Access web site. Address as of 12/02/04: http://frwebgate.access.gpo.gov/cgi-bin/getdoc.cgi?dbname=gao&docid=f:d05126.pdf; current access available via PURL."}]},VariableDataField {indicator1 = Just '1', indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'a', dataField = "United States."},DataElement {dataElementId = 'b', dataField = "Transportation Security Administration"},DataElement {dataElementId = 'v', dataField = "Rules and practice"},DataElement {dataElementId = 'x', dataField = "Evaluation."}]},VariableDataField {indicator1 = Nothing, indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'a', dataField = "Aeronautics, Commercial"},DataElement {dataElementId = 'x', dataField = "Security measures"},DataElement {dataElementId = 'z', dataField = "United States"},DataElement {dataElementId = 'x', dataField = "Evaluation."}]},VariableDataField {indicator1 = Nothing, indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'a', dataField = "Airline passenger security screening"},DataElement {dataElementId = 'z', dataField = "United States"},DataElement {dataElementId = 'x', dataField = "Evaluation."}]},VariableDataField {indicator1 = Just '1', indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "United States."},DataElement {dataElementId = 'b', dataField = "Transportation Security Administration."}]},VariableDataField {indicator1 = Just '1', indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "United States."},DataElement {dataElementId = 'b', dataField = "Congress."},DataElement {dataElementId = 'b', dataField = "House."},DataElement {dataElementId = 'b', dataField = "Committee on Transportation and Infrastructure."},DataElement {dataElementId = 'b', dataField = "Subcommittee on Aviation."}]},VariableDataField {indicator1 = Just '4', indicator2 = Just '0', dataElements = [DataElement {dataElementId = 'u', dataField = "http://purl.access.gpo.gov/GPO/LPS56006"},DataElement {dataElementId = 'z', dataField = "View online version"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = ".b37991772"},DataElement {dataElementId = 'b', dataField = "06-10-15"},DataElement {dataElementId = 'c', dataField = "07-26-05"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "es001"},DataElement {dataElementId = 'b', dataField = "07-26-05"},DataElement {dataElementId = 'c', dataField = "m"},DataElement {dataElementId = 'd', dataField = "a"},DataElement {dataElementId = 'e', dataField = "-"},DataElement {dataElementId = 'f', dataField = "eng"},DataElement {dataElementId = 'g', dataField = "dcu"},DataElement {dataElementId = 'h', dataField = "0"},DataElement {dataElementId = 'i', dataField = "1"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "MARCIVE"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'a', dataField = "Hathi Trust report None"}]},VariableDataField {indicator1 = Nothing, indicator2 = Nothing, dataElements = [DataElement {dataElementId = 'g', dataField = "0"},DataElement {dataElementId = 'j', dataField = "0"},DataElement {dataElementId = 'l', dataField = "esb  "},DataElement {dataElementId = 'o', dataField = "n"},DataElement {dataElementId = 'p', dataField = "$0.00"},DataElement {dataElementId = 'q', dataField = " "},DataElement {dataElementId = 'r', dataField = " "},DataElement {dataElementId = 's', dataField = "-"},DataElement {dataElementId = 't', dataField = "255"},DataElement {dataElementId = 'u', dataField = "0"},DataElement {dataElementId = 'v', dataField = "0"},DataElement {dataElementId = 'w', dataField = "0"},DataElement {dataElementId = 'x', dataField = "0"},DataElement {dataElementId = 'y', dataField = ".i138993580"},DataElement {dataElementId = 'z', dataField = "07-26-05"}]}]}

expectedMarc1Leader :: Leader
expectedMarc1Leader = Leader
  { recordLength = 1805
  , status = New
  , typeOfRecord = LanguageMaterial
  , implDefined = "m "
  , encoding = UTF8
  , baseAddress = 385
  , implDefined2 = " i "
  }

parseMarc :: String -> Maybe Marc21Record
parseMarc = parseMaybe parseMarc21Record

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser input =
    case runParser parser () "test data" input of
        Left _       -> Nothing
        Right result -> Just result

withFileSpec :: FilePath -> (String -> IO r) -> IO r
withFileSpec filename spec =
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    spec contents

varFieldsStr :: String
varFieldsStr = "  \USbGAO (202)512-6000 (voice); (202)512-6061 (Fax); (202)512-2537 (TDD)\USfpaper copy\RS  \USaGPO\UScGPO\USdMvI\USdMvI\RS  \USapcc\RS  \USan-us---\RS  \USa0546-D (online)\RS0 \USaGA 1.13:GAO-05-126\RS  \USaGAO-05-126\RS1 \USaUnited States.\USbGovernment Accountability Office.\RS10\USaAviation security\USh[electronic resource] :\USbpreliminary observations on TSA's progress to allow airports to use private passenger and baggage screening services : report to the Chairman, Subcommittee on Aviation, Committee on Transportation and Infrastructure, House of Representatives.\RS3 \USaAviation security :\USbpreliminary observations on Tranportation Security Administration's progress to allow airports to use private passenger and baggage screening services.\RS30\USaPreliminary observations on TSA's progress to allow airports to use private passenger and baggage screening services.\RS  \USa[Washington, D.C.] :\USbU.S. Government Accountability Office,\USc[2004]\RS  \USatext\US2rdacontent.\RS  \USacomputer\US2rdamedia.\RS  \USaonline resource\US2rdacarrier.\RS  \USaTitle from title screen (viewed on Dec. 2, 2004)\RS  \USa\"November 2004.\"\RS  \USaPaper version available from: U.S. Government Accountability Office, 441 G St., NW, Rm. LM, Washington, D.C. 20548.\RS  \USa\"GAO-05-126.\"\RS  \USaIncludes bibliographical references.\RS  \USaMode of access: Internet from GPO Access web site. Address as of 12/02/04: http://frwebgate.access.gpo.gov/cgi-bin/getdoc.cgi?dbname=gao&docid=f:d05126.pdf; current access available via PURL.\RS10\USaUnited States.\USbTransportation Security Administration\USvRules and practice\USxEvaluation.\RS 0\USaAeronautics, Commercial\USxSecurity measures\USzUnited States\USxEvaluation.\RS 0\USaAirline passenger security screening\USzUnited States\USxEvaluation.\RS1 \USaUnited States.\USbTransportation Security Administration.\RS1 \USaUnited States.\USbCongress.\USbHouse.\USbCommittee on Transportation and Infrastructure.\USbSubcommittee on Aviation.\RS40\USuhttp://purl.access.gpo.gov/GPO/LPS56006\USzView online version\RS  \USa.b37991772\USb06-10-15\USc07-26-05\RS  \USaes001\USb07-26-05\UScm\USda\USe-\USfeng\USgdcu\USh0\USi1\RS  \USaMARCIVE\RS  \USaHathi Trust report None\RS  \USg0\USj0\USlesb  \USon\USp$0.00\USq \USr \USs-\USt255\USu0\USv0\USw0\USx0\USy.i138993580\USz07-26-05\RS"


expectedVarField1 :: VariableDataField
expectedVarField1 = VariableDataField
  { indicator1 = Nothing
  , indicator2 = Nothing
  , dataElements =
    [ DataElement 'b' "GAO (202)512-6000 (voice); (202)512-6061 (Fax); (202)512-2537 (TDD)"
    , DataElement 'f' "paper copy"
    ]
  }



main :: IO ()
main = hspec $
  describe "MARC parser should" $ do

  it "parse valid input 1a - leader" $
    withFileSpec "data/marc1a.mrc" $ \s ->
    leader <$> parseMarc s `shouldBe` Just expectedMarc1Leader

  it "parse valid input 1b" $
    withFileSpec "data/marc1b.mrc" $ \s ->
    parseMarc s `shouldBe` Just expectedMarc1b

  it "parse the correct number of records from a multi-record file" $
    withFileSpec "data/test_10.mrc" $ \s ->
    length <$> parseMaybe parseMarc21Records s `shouldBe` Just 10

  it "parse the correct number of records from a single record file" $
    withFileSpec "data/marc1a.mrc" $ \s ->
    length <$> parseMaybe parseMarc21Records s `shouldBe` Just 1

  it "parses a variable field" $ do
    let s = "  \USbGAO (202)512-6000 (voice); (202)512-6061 (Fax); (202)512-2537 (TDD)\USfpaper copy"
    runParser parseVariableField () "test data" s `shouldBe` Right expectedVarField1

  it "extract the correct number of varfields from the varfields portion of a record" $ do
    let p = endBy parseVariableField fieldTerminator
    let expectedCount = length $ filter isFieldTerminator varFieldsStr
    length <$> runParser p () "test data" varFieldsStr `shouldBe` Right expectedCount

    -- it "the number of extracted fields should match the number of directory entries"

