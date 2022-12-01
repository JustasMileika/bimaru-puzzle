import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib1 (State(..))
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)


golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [ 
    --  testCase "goldenInput" $
    --     parseDocument (friendlyEncode (goldenDoc1)) @?= Left (friendlyEncode (goldenDoc1))
    -- ,testCase "goldenInput1" $
    --     parseDocument (friendlyEncode (DList [DInteger (-6),DMap [("y",DList [DString "onHrx3",DInteger (-5)]),("hilTiLyK",DString "  E K "),("wzKUk",DInteger (-6)),("MSXAnF",DInteger (-4))],DString ""])) @?= Left (friendlyEncode (DList [DInteger (-6),DMap [("y",DList [DString "onHrx3",DInteger (-5)]),("hilTiLyK",DString "  E K "),("wzKUk",DInteger (-6)),("MSXAnF",DInteger (-4))],DString ""]))
    -- , testCase "dogfood" $
    --     parseDocument (renderDocument dogFood1) @?= Left (renderDocument dogFood1)  
     testCase "null" $
        parseDocument "null" @?= Right DNull
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]


kazkas = unlines[
  "dmUXglho: 12",
  "JYslPBdkZ:",
  "-",
  "  - {}",
  "  -",
  "    - \"lXadBcv8\"",
  "    - []",
  "    - YKRRhn: ''",
  "{}  - \"PIwf 2U1   \"\n"]

goldenDoc1 = DMap [("nC",DInteger (-55)),("lkR",DMap [("PvumUHu",DMap []),("NkDUfqo",DMap [("OqdueV",DMap [("FtfIvGdCy",DInteger 74),("SnBAQ",DMap [("KLrtQOsNg",DString "O1 ")]),("tRmebF",DMap [("PVY",DString "   y  ")]),("SQ",DList [DInteger (-41),DMap [("btBoROY",DList [DList [DMap [("kcrglhRI",DList [DString "K 2y  7cHjWZoM S",DMap [("ajWjFXDY",DInteger 42),("ps",DInteger (-39)),("csdkRM",DMap [("nn",DString "X  m8"),("DS",DString "T"),("RYiDe",DString " T0RdMBf  B0cy ")]),("lf",DMap [("GBkLmaHz",DInteger 5),("n",DString "3  6 2S3b1h "),("bjIgejaTE",DInteger (-28)),("Pf",DString "2b H dD  ZY631E")])],DString "8 2Cl5lJoR z",DList [DMap [("s",DMap [("LugYj",DList [DInteger (-36)]),("TbrMXm",DInteger 73),("cHmXGCMJ",DString "8"),("poDd",DInteger 43)])]]]),("QQIWjL",DInteger 55),("SQu",DString "")]],DList [DMap [],DInteger 75,DList [DString "1 n  ",DList []],DString "  p vJy jn123eD"],DString "We Pl7qM k8 Ru"]),("kKivmJhMWQ",DList [DInteger (-54),DList [DInteger 79,DList [DList [DMap [("sFH",DMap [("PFQWvTsj",DInteger 19),("T",DList [DInteger (-3)]),("nAgcp",DInteger 34)]),("JNVhzlBEIW",DString "867 iMA0 mu"),("nUi",DList []),("Q",DList [DMap [("zs",DString "Cn")],DString "28"])],DString "1",DInteger (-77)],DList [DInteger 34,DString "bLH  Wfy"]],DString "J  GP7  m6Vx",DString ""]])]])])]),("t",DMap [])]),("cZBd",DMap [("sYoCld",DList [DMap [("trY",DList [DInteger 18,DInteger (-39),DList [DString "1t7FUK",DMap [("nAbTck",DString ""),("iSod",DInteger 52)],DMap [("GDJM",DString "9uJ e z6"),("ZLU",DList [DMap [("xJRhFEydSw",DList [])]]),("vKPXruMW",DMap [("raL",DList []),("WO",DInteger (-40)),("j",DList []),("HF",DList [DMap [("GGAZfFxA",DString "8h01  r4 tVE4s2"),("zuDMJ",DInteger 35)],DInteger (-9)])])]],DList [DString "F ",DMap [("RJ",DMap [("FAJP",DString "MKyXs j7"),("nfF",DMap [("UDU",DString "  1 vf"),("VLK",DInteger (-23)),("uy",DInteger 81)]),("Yvu",DMap [("CbN",DInteger 0),("jCixXil",DInteger 73)])]),("dDjOjfXf",DList [DString "p0 q",DMap [("XB",DInteger 70),("nvGSs",DString "7zj8j G ")]])],DString " 92  68r ghs  ",DMap [("nEVZClbI",DInteger (-49)),("lRP",DString " nL9eU0"),("K",DMap [("Do",DList [DMap [("oqONOaQ",DList [DString "J4 "]),("PstXrybQo",DList [DMap [],DString "Y8T JB4 X"])],DInteger 54,DList [DList [DMap [("d",DList [DString ""]),("cFhejrmcH",DString "")],DInteger 12]],DInteger 37]),("Dvc",DList []),("AWNrDsj",DList [DInteger (-60),DMap [("ksAAosSR",DList [DInteger 31,DString " 8u6 ",DList [DString "rs mxPqzgfn",DMap [("ucgpLHP",DString "l16p37NBK Q qI"),("wIAsE",DList [DList [DInteger (-47),DInteger 30,DInteger 73],DInteger (-67),DString "h D2x4xQ  c",DString ""]),("npm",DString "Ps sEIW1 qKu6  "),("fDDSbHjD",DString "s4b v")]],DInteger 8])]]),("ZIC",DMap [("K",DMap [])])])]]]),("sgwhBVfbg",DString "m  x8")]]),("BBYoTJEJax",DList [DMap [("XZctdh",DInteger (-48)),("yfblnFBKk",DMap [("zK",DString "2  4 p K r q2 ")])],DList [],DString "f4 7 ",DList [DList [DList []],DMap [("CupXQYIL",DString "2 tw u"),("HbOKVUJuAd",DInteger (-34))]]]),("L",DMap [("MqTy",DInteger 26)])]),("orXiYWlAi",DInteger (-59))]

ziurim = unlines[
  "JYslPBdkZ:",
  "-",
  "  - {}",
  "  -",
  "    - \"lXadBcv8\"",
  "    - []",
  "    - YKRRhn: ''",
  "{}  - \"PIwf 2U1   \"",
  "  - \"x6w3C 78 sU \"",
  "- -6",
  "- ''"
 ]

testukas = DMap [
  ("JYslPBdkZ",DList [
  DList [
    DMap [],
    DList [
      DString "lXadBcv8",
      DList [],
      DMap [("YKRRhn",DString "")]
      ],
    DString "PIwf 2U1   ",
    DString "x6w3C 78 sU "],
  DInteger (-6),
  DString ""])]


dogfoodt = unlines[
  "dmUXglho: 12",
  "JYslPBdkZ:",
  "-",
  "  - {}",
  "  -",
  "    - \"lXadBcv8\"\n    - []",
  "    - YKRRhn: ''",
  "  - \"PIwf 2U1   \"",
  "  - \"x6w3C 78 sU \"",
  "- -6",
  "- ''",
  "FOtYC: \"9HeaWS4 r NM8j \"",
  "zXTGGzqvxQ: \"  H n a6U\""]

dogFood1 = DMap [
  ("dmUXglho",DInteger 12),
  ("JYslPBdkZ", DList [
    DList [
      DMap [],
      DList [
        DString "lXadBcv8",
        DList [],
        DMap [("YKRRhn",DString "")]
        ],
      DString "PIwf 2U1   ",
      DString "x6w3C 78 sU "],
    DInteger (-6),
    DString ""]),
  ("FOtYC",DString "9HeaWS4 r NM8j "),
  ("zXTGGzqvxQ",DString "  H n a6U")]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "string" $
        renderDocument (DString "word") @?= "\"word\""
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "list of strings" $
        renderDocument (DList [DString "first", DString "second"]) @?= listOfStrings
    , testCase "list of nulls" $
        renderDocument (DList [DNull, DNull]) @?= listOfNulls
    , testCase "list of lists" $
        renderDocument (DList [DList[DNull, DNull], DList[DNull, DNull]]) @?= listOfLists    
    , testCase "map of mixed lists" $
        renderDocument (DMap[("DList1",DList[DInteger 1, DString "hello", DNull]),("DList2",DList[DInteger 2, DString "world", DNull])]) @?= mapOfLists
    , testCase "map of mixed lists" $
        renderDocument (DMap[("DNull", DNull), ("DString", DString "word"), ("DInteger", DInteger 5)]) @?= mapOfMixedLiterals
    , testCase "map of mixed maps" $
        renderDocument (DMap[("DMap1", DMap[("InnerDMap1", DInteger 1), ("InnerMap2", DString "word")])] ) @?= mapOfMaps
    , testCase "map of lists of maps" $
        renderDocument (DMap [("coords", DList[DMap[("col", DInteger 1),("row", DInteger 6)], DMap[("col", DInteger 1),("row", DInteger 9)]])]) @?= mapOfListsOfMaps
    , testCase "tricky - nested maps and lists, edge cases" $
        renderDocument (DMap [("key1", DMap [("key2", DList [DInteger 1, DMap [("key3", DList [DInteger 1, DInteger 3, DNull, DMap [("", DNull)], DMap []]), ("key4", DString "")], DNull])]), ("key5", DList [])]) @?= tricky  
       
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

--same as trickyOriginal just changed indentation and
--slightly modified format (DMap in DList has "-" before map key in the same line, eg. "- key3:") 


tricky :: String
tricky = unlines [
    "key1:",
    "  key2:",
    "  - 1",
    "  - key3:",
    "    - 1",
    "    - 3",
    "    - null",
    "    - '': null",
    "    - {}",
    "    key4: ''",
    "  - null",
    "key5: []"
  ]
{-
trickyOriginal :: String
trickyOriginal = unlines [
    "---",
    "key1: ",
    " key2: ",
    " - 1",
    " - ",
    " key3: ",
    " - 1",
    " - 3",
    " - null",
    " - ",
    " '': null",
    " - []",
    " key4: ''",
    " - null",
    "key5: []"
  ]
  -}

mapOfMixedLiterals :: String
mapOfMixedLiterals = unlines [
      "DNull: null"
    , "DString: \"word\""
    , "DInteger: 5"
 ]

listOfNulls :: String
listOfNulls = unlines [
      "- null"
    , "- null"
 ]
listOfLists :: String
listOfLists = unlines [
      "-"
    , "  - null"
    , "  - null"
    , "-"
    , "  - null"
    , "  - null"
  ]

listOfInts :: String
listOfInts = unlines [
      "- 5"
    , "- 6"
  ]
listOfStrings :: String
listOfStrings = unlines [
      "- \"first\""
    , "- \"second\""
  ]

mapOfLists :: String
mapOfLists = unlines [
  "DList1:",
  "- 1",
  "- \"hello\"",
  "- null",
  "DList2:",
  "- 2",
  "- \"world\"",
  "- null"
 ]

mapOfMaps :: String
mapOfMaps = unlines [
  "DMap1:",
  "  InnerDMap1: 1",
  "  InnerMap2: \"word\""
 ]
mapOfListsOfMaps :: String 
mapOfListsOfMaps = unlines [
  "coords:",
  "- col: 1",
  "  row: 6",
  "- col: 1",
  "  row: 9"
  ]

emptyState :: State
emptyState = State [] [] [] 0

invalidInput :: Either String State
invalidInput = Left "Invalid input"


gmValidStateExmpl :: Either String State
gmValidStateExmpl = Right (State {occupied = [], rows = [1,1,2,3,1,4,2,4,2,0], columns = [3,0,6,0,2,2,2,0,2,0], hintAmount = 10})

--shortened gameStart since State is irrelevant
gameStart' :: Document -> Either String State
gameStart' = gameStart emptyState

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" [
  testCase "Correct document" $ gameStart' gmCorrectDoc @?= gmValidStateExmpl,
  testCase "No DMap" $ gameStart' gmDocNoDMap @?= invalidInput,
  testCase "Mismatching or missing key name" $ gameStart' gmDocCorrectKeyMissing @?= invalidInput,
  testCase "Missing 1 element in DList" $ gameStart' gmDocMissingElmnts @?= invalidInput,
  testCase "Incorrect type elements in DList" $ gameStart' gmDocBadType @?= invalidInput,
  testCase "Mismatching key name" $ gameStart' gmDocCorrectKeyMissing @?= invalidInput,
  testCase "Incorrect type after key 'number_of_hints'" $ gameStart' gmDocBadTypeNumOfHints @?= invalidInput,
  testCase "Incorrect type after key 'occupied_rows'" $ gameStart' gmDocBadTypeRows @?= invalidInput,
  testCase "Incorrect integers in 'occupied_rows'" $ gameStart' gmDocBadIntRows @?= invalidInput
 ]
--gm ~ game start
gmCorrectDoc :: Document
gmCorrectDoc = DMap [
    ("number_of_hints",DInteger 10),
    ("occupied_cols",DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows",DList [DInteger 1, DInteger 1, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0])]

gmDocNoDMap :: Document
gmDocNoDMap = DNull

gmDocMissingElmnts :: Document
gmDocMissingElmnts = DMap [
    ("number_of_hints",DInteger 10),
    ("occupied_cols",DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2]),
    ("occupied_rows",DList [DInteger 1, DInteger 1, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0])]

gmDocBadType :: Document
gmDocBadType = DMap [
    ("number_of_hints",DInteger 10),
    ("occupied_cols",DList [DString "ERROR", DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows",DList [DInteger 1, DInteger 1, DString "ERROR", DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0])]

gmDocCorrectKeyMissing :: Document
gmDocCorrectKeyMissing = DMap [
    ("MISMATCH",DInteger 10),
    ("occupied_cols",DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows",DList [DInteger 1, DInteger 1, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0])]

gmDocBadTypeNumOfHints :: Document
gmDocBadTypeNumOfHints = DMap [
    ("number_of_hints",DString "ERROR1"),
    ("occupied_cols",DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows",DList [DInteger 1, DInteger 1, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0])]

gmDocBadTypeRows :: Document
gmDocBadTypeRows = DMap [
    ("number_of_hints", DInteger 10),
    ("occupied_cols", DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows", DNull)]

gmDocBadIntRows :: Document
gmDocBadIntRows = DMap [
    ("number_of_hints", DInteger 10),
    ("occupied_cols", DList [DInteger 3, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0]),
    ("occupied_rows", DList [DInteger 999, DInteger 0, DInteger 6, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 2, DInteger 0])]


validHintState :: Either String State
validHintState = Right (State {occupied = [(8,3),(7,3),(6,3),(5,3),(7,5),(7,6),(7,7),(4,0),(5,0),(6,0)], rows = [], columns = [], hintAmount = 0})

validAddedHintState :: Either String State
validAddedHintState = Right (State {occupied = [(8,3),(7,3),(0,3),(6,3),(5,3),(7,5),(7,6),(7,7),(4,0),(5,0),(6,0)], rows = [], columns = [], hintAmount = 0})

nonEmptyState :: State
nonEmptyState =  State {occupied = [(8,3),(7,3)], rows = [], columns = [], hintAmount = 0}

hintTests :: TestTree
hintTests = testGroup "Test hint document" [
    testCase "Correct document" $ hint emptyState docAll @?= validHintState,
    testCase "No DMap" $ hint emptyState docNoDMap @?= invalidInput,
    testCase "Mismatch for key 'coords'" $ hint emptyState docNoCoords @?= invalidInput,
    testCase "No tail" $ hint emptyState docNoTail @?= invalidInput,
    testCase "Empty" $ hint emptyState docEmpty @?= invalidInput,
    testCase "Incorrect integers" $ hint emptyState docIncorrectNums @?= invalidInput,
    testCase "No head" $ hint emptyState docNoHead @?= invalidInput,
    testCase "Not DMap after 'coords'" $ hint emptyState docNoDMapAfterCoords @?= invalidInput,
    testCase "Mismatch for key 'row'" $ hint emptyState docNoRow @?= invalidInput,
    testCase "Not DInteger after 'row" $ hint emptyState docNoDInt @?= invalidInput,
    testCase "Adding hints to non empty state" $ hint nonEmptyState docCorrectNotFull @?= validAddedHintState
 ]

docAll :: Document
docAll = DMap [("coords",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 8)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 6)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 5)]),
        ("tail",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 7),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 4)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 5)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 6)]),
        ("tail", DNull)])])])])])])])])])])]

docCorrectNotFull :: Document
docCorrectNotFull = DMap [("coords",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 0)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 6)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 5)]),
        ("tail",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 7),("row",DInteger 7)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 4)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 5)]),
        ("tail",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 6)]),
        ("tail", DNull)])])])])])])])])])])]

docEmpty :: Document
docEmpty = DMap []

docIncorrectNums :: Document
docIncorrectNums = DMap [("coords",DMap [("head",DMap [("col",DInteger 999),("row",DInteger 8)]),
        ("tail", DNull)])]

docNoHead :: Document
docNoHead = DMap [("coords",DMap [("NOT HEAD",DMap [("col",DInteger 2),("row",DInteger 8)]),
        ("tail", DNull)])]

docNoCoords :: Document
docNoCoords = DMap [("NOT COORDS",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 8)]),
        ("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 7)]),
        ("tail",DNull)])])]

docNoDMapAfterCoords :: Document
docNoDMapAfterCoords = DMap [("coords",DList[DNull])]       

docNoTail :: Document
docNoTail = DMap [("coords",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 8)]),
        ("ERROR",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 7)]),
        ("tail",DNull)])])]



docNoDMap :: Document
docNoDMap = DList [DNull, DNull]

docNoRow :: Document
docNoRow = DMap [("coords",DMap [("head",DMap [("col",DInteger 2),("NOT row",DInteger 8)]),
        ("tail", DNull)])]

docNoDInt :: Document
docNoDInt = DMap [("coords",DMap [("head",DMap [("col",DInteger 2),("row",DString "8")]),
        ("tail", DNull)])]


