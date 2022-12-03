import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)


import qualified Data.List as L



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

testukas1 = L.sort ([("lSjEqofj",DList [DString "RRfP",DMap [],DList [DString "hO48f 4YQ7 6",DList [DString " Lp6"]],DInteger (-1)]),("QuYxoCNpW",DInteger (-72)),("u",DString "4 L  73ZyN1O  ")]
 )
testukas2 = L.sort ([("u",DString " z74 "),("lSjEqofj",DList [DString "RRfP",DMap [],DList [DString "hO48f 4YQ7 6",DList [DString " Lp6"]],DInteger (-1)]),("QuYxoCNpW",DInteger (-72)),("u",DString "4 L  73ZyN1O  ")]
 )

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

superGiga = DList [DInteger 21,DList [DString "  7t o ",DInteger 56,DInteger (-24),DMap [("EYaLqK",DList [DList [DMap [("xf",DString "Be77p  dvk")],DInteger (-24)]]),("fmpUIhOZp",DInteger 47),("mJz",DInteger 7),("NtUi",DMap [("AfXVGbsBj",DList [DMap [("mLRyFeXLR",DMap []),("fwXVNHCWtv",DList [DInteger 49,DList [DString "Mp 2VT 7d5J 85B",DInteger 10,DList [DMap [("a",DInteger 41),("dRGF",DMap [("BgXMFsQw",DMap [("rOqgFSM",DList [DString "fhB x3x ",DInteger 68,DInteger (-40)])])]),("xlT",DString "  DLjw J"),("SN",DMap [("H",DMap [("dptHV",DString ""),("Gs",DList [DList [DList [],DMap [],DString "V 2b 5kaa  2c",DMap [("xaNLdcnT",DList [DInteger 65,DList [DMap [],DString "j3514 9 K4V",DString "0 a Pm59o  ",DString "  u  ja2 2X7Xg"],DMap [],DInteger 63])]],DMap []]),("pyVyo",DString "")]),("vIFiByj",DString "")])],DList [DInteger (-50),DMap [("alwrz",DString "  u "),("Fr",DInteger 19),("yKSCoj",DMap [("Df",DList [DMap [("YovmQUZktM",DInteger 47),("qyc",DList [DMap [("jXdPWNev",DString " I")],DInteger (-66),DInteger 25,DString "33t"]),("Q",DMap [("yjSP",DList [DInteger 25,DString " Uk   AtK  ",DString "",DList [DMap [("dmCNySpn",DString "Y  "),("HJJiNl",DString "8 "),("gqeqylwBzS",DList [DInteger (-24),DInteger 57,DString "3 1i9I 1Z5",DInteger (-55)])],DMap [("yGNvclI",DString "0702SB"),("uJvBSuw",DList [DMap [("qdcH",DString "Y p R1    ez9J6"),("FI",DString "P  v am4 V "),("GOgSUjoYQ",DList [DList [DMap [("vCjUU",DMap [("YJxibaHLh",DInteger 8),("JFRdyJx",DString "2W9  JLAqc2"),("fmDiBJrpQ",DInteger 62),("VOtPPqPtz",DInteger (-7))]),("ZEPEtUJX",DList [DList [DInteger (-63),DInteger (-4),DString "g778"],DList [],DMap [("yifpqyue",DMap [("smCpo",DMap [("OxDOLic",DMap [("BXtV",DMap [("F",DMap [("ASidFNwGt",DMap [("sdoG",DInteger 48),("Dpf",DList []),("HKnG",DMap [("rwWO",DInteger (-45))]),("nQ",DMap [("ZwQtYZN",DMap []),("HBJwKmn",DString "9yZ 3V5X  j B8NI"),("RonQ",DList [DMap [("oVhIKYuk",DString "h 4P"),("tQTsvydee",DList [DMap [("wqNmMglaz",DMap [("KL",DInteger 7),("kXwpq",DInteger (-21)),("PoX",DString "h15Cg DnxlBUr9 ")]),("MaBp",DMap [("ovPkl",DList [DMap [("dzbF",DList [DMap [("ysrk",DMap [("aKIuTeqYoc",DInteger (-48)),("lFrxvFZzr",DList [])]),("vylWh",DList []),("eqpRI",DInteger 35),("rQVYIPn",DList [DMap [("JxH",DInteger 58),("R",DList [DMap [("GFagod",DMap [("KlgolrZofA",DString "04Gl kx6sy")])]]),("THFGT",DInteger 24)],DMap [("wMNwUDnytA",DMap [("TN",DString " rJ8 "),("llaKnaK",DMap [("Ygpe",DMap [("dhQey",DInteger 46)]),("syfKePQ",DList [DString " i 819   d",DMap [("bXIMuXTCbs",DMap [("Y",DString " 6  qn2 52Hr "),("GYKVXMnPCc",DList []),("tpkCuMUT",DInteger (-41))]),("wbfOeykq",DInteger 25),("tFjoxETrmJ",DString "rT18Cwr tB"),("INtr",DList [DString " Y3YN9157V8h w4E",DList [DString "P1 r047Lo"],DInteger (-38)])],DList []]),("y",DString "")]),("nzDBJ",DList [DMap [("twS",DList []),("xBkbvEvD",DMap []),("xyHyhXBNY",DMap [("BEP",DString "537"),("qu",DList [DList [],DList [DInteger (-38),DList [DMap [("DlucusCQe",DMap [("kkcOSZ",DInteger 30),("F",DString " q cP76Omh xjEn7"),("Naqpfdp",DMap [])]),("Hl",DString "99R1  6750 1"),("trIx",DString "C 8b"),("UIB",DString " wU2 B2  WlY")],DList [DMap []],DString "1 T  iaBO",DMap []],DString " 8h2E"]]),("shIjZlFwk",DMap [("QutpeP",DString "l91Z"),("QbakscHebz",DString "bLj "),("kGLrIdgI",DList [DMap [("YSVOKtZK",DList [DList [DString "mfJT7J95 rP  E",DString "iK5 Ri ",DInteger 52,DMap []]]),("OZNvvvv",DMap [])],DList [DInteger (-50),DList [DMap [("A",DInteger 39)],DMap [("v",DList [DMap [("X",DString "  "),("fgbxijjA",DMap [("oDhnjrLTZC",DInteger 30),("hlhaha",DInteger 20)])]]),("NAP",DInteger 23),("qD",DMap [("CFJzA",DInteger 47)])],DInteger 12]],DMap []])]),("ii",DInteger (-6))]),("hfgE",DList [DInteger (-60),DString "i2 3 b"])],DList [DInteger (-46),DList [DString "   27Pk   b 4y",DInteger 60,DString " 56 R Ba",DList [DMap [("hJm",DList [DMap [("zkNqLYee",DMap []),("A",DInteger 16),("HVdqtGK",DString "3ygy6H "),("lgfhGCv",DString "p0nyM wk3 riu")]]),("mKUwieFD",DInteger (-31)),("OfRE",DString "1X0Wa"),("KUXRl",DList [DMap [("dgEMPecu",DList [DMap [("Zsf",DString "69"),("pNrPTITR",DString "  D  o41q0z 2 "),("FGIrjzExCq",DInteger 50),("oVnr",DList [DMap [("wXiD",DInteger 35),("QLb",DMap []),("KQh",DMap [("iUBSYjWdu",DList [DList [DInteger (-66),DMap [("wtOZ",DList [DString " 7a ",DMap [("QcS",DString "t cxYp0 4E 7"),("wW",DList [DList [DString "P   ",DList [],DMap [],DInteger (-24)],DList [DList [DInteger (-65)]],DInteger 64]),("suMJ",DMap [("wVsTruPlA",DString "  AWh"),("q",DString " ep"),("DRnYP",DInteger 29),("iN",DString "A yt  8z  g13TFW")]),("fiegbK",DInteger 66)],DString " qS5t9",DList [DMap [("m",DInteger 27),("tTwM",DString "ZB as JY")],DString "S 84 r FjGr ",DString " 5T03j 6v0 827pO",DMap [("PQlZbOMyK",DMap [("SX",DList [DString "Q   lx7",DMap [("Jl",DString "fk 2ZcI "),("hPtindgE",DMap [("CFFKuFK",DInteger (-12))]),("WF",DString "51TM ")]])]),("TGk",DString "4I7 "),("RC",DInteger (-38))]]]),("LJxrOZXQIc",DList [DString " t",DString "1S  "]),("bisVuZgBc",DMap [("kGni",DString "9t97   13rO3a JO")]),("WqxylRUIp",DString "2czOaUM5C9  1")],DList [],DString "G u3Z6 9P  fk5 "],DList [DList [DString "",DString "",DMap [("f",DString "l RM7b1"),("HuvNGnU",DMap [("Avz",DMap [("IPK",DMap [("rYVZDGPQda",DMap [("qeAyc",DList [DMap [("B",DMap [("zeb",DMap [("tRIDOCR",DInteger 59),("aeQ",DString "7l  qthyQ N  t")])])],DList [DInteger 11,DInteger 43,DString "Hl7ZE2",DList [DMap [],DList [DInteger (-11),DInteger 53],DList [],DMap [("cXcjWv",DList [DList [],DMap [("MAtDBjXUA",DString "7   "),("mRyGNt",DList [DList [DInteger 7],DString "lHN3 "]),("E",DInteger 13),("yfiYtJE",DMap [])],DList [DList [],DList [DInteger 25,DInteger 54,DString "yiG 4iJ 069 b"]],DInteger 14]),("uVVf",DList [DList [DInteger (-13)],DString "4 0V vAo1  D8YA"]),("rHp",DMap [("EFs",DMap [("kmKLL",DList [DString " eQAE9 9"]),("LNjLgzL",DInteger (-11)),("V",DString "9y37  u H")]),("NbYH",DMap [("WREfpWZ",DMap [])])])]]],DInteger 11,DList [DInteger (-57),DInteger (-8),DString " ic1K   3l"]]),("IyBsxv",DMap [("OfJqITJtw",DList [DString "vV CM4B1 I9F9a ",DMap [("nFgJo",DString " u 4bcTEe   "),("xzfzjxmH",DList [DString "E768mOmMB a7 2U",DInteger (-7),DInteger 32,DList [DList [DList [DList [DList [DMap [("EIWiZBNqNo",DInteger 65),("DkiZZIIoO",DString "R  FIzcI O"),("BdjEG",DList [DInteger (-13),DList []])],DString "Z03ao 6W6r1",DList [DString "",DList [DMap [("uqqU",DMap [("puvtdbyRSY",DString " t 2iR DKbW "),("Cbi",DMap [("ILoiiHkGeG",DString "9 2 G9 L6 Y Q532"),("wtMVBBbWLN",DInteger (-14))])]),("MwpN",DList [DMap [("pkJeYeRId",DString " g9 tzh"),("cV",DMap [("bO",DString " Ar 9  c72")]),("SCQlGriw",DMap []),("zwSB",DInteger 68)]])],DMap [("bsVvHeBpB",DInteger (-42))],DInteger 6,DString "4C yNN"],DMap [("BEAz",DMap [("GvvTVqxN",DList [DString "f",DString "B3sg hegGE ITJv5",DString "4 nff 9",DMap [("rOVolmmI",DInteger (-35)),("RhKemOl",DMap [("nxiQfgNIx",DString " 31 c1Q 25"),("sqshtdjK",DString "hn")]),("AlahWCoEo",DMap [("LvXBLTQAk",DInteger 31),("qWmrRaCb",DMap [("pFyrsDj",DString "j  fokB V")]),("eAvzekgl",DString "L")]),("frPbv",DString "6 2 j0keopTB")]]),("EFUBIFp",DList [DList [DList [DList [DString "1k",DString ""]],DString "",DInteger 55]]),("NhmkubkpcU",DList []),("P",DMap [("WTduMNpt",DInteger (-14))])])],DInteger 40],DInteger (-33)],DList [DInteger (-42),DList [],DString "87k2N vq6dpJq1f"]]]],DInteger 35]])],DList [],DList [DInteger 2,DMap [("cHxCsmvb",DInteger (-52)),("umRFNBGwu",DMap []),("nmwg",DList [])],DString "  C 52  5"]]),("v",DString "33"),("y",DString "0 1k  EGo2R0"),("rcTvZG",DMap [("lt",DString "V4N75 I"),("PXMXbCyjsW",DString "rr G2")])]),("u",DMap [("CiELzGrxJ",DMap [("rvEfeV",DMap []),("shD",DString " djQvdECwQUy4L3")]),("a",DList [DMap [("hq",DInteger 15),("Qfcusr",DInteger 48),("vN",DList [DInteger (-15),DList [DInteger (-9),DList [DString "ua  l0m 1Q1GY  C",DString "74O7Id4  2",DInteger (-64)],DList []],DInteger (-59)]),("ioxBcvBygp",DList [DMap [("xZog",DMap [("kWRrfxrS",DList [DList [DString "577rfHEf O P",DMap [("uHc",DMap []),("QKJG",DList [DInteger 18,DList [DString "C",DInteger (-63),DString "4D0v 9",DMap []],DList [DList [DString " 6"],DInteger (-30),DList [DInteger (-7),DList [DList [DString " ",DMap [("khqQU",DString "h45Yp4"),("F",DInteger (-12))],DInteger (-36),DMap [("YfZUvL",DList [DMap [("tKMmiG",DInteger 62),("jPeF",DMap [("X",DInteger 59)]),("unneZPHtq",DList [DString "S La4",DString "  Jn z  1Za18 ",DList []]),("pLUZ",DInteger 4)]]),("Uu",DInteger (-14))]]],DString " fi G yq  pHr"]]]),("TcM",DInteger (-1))]],DInteger (-52),DList [DString "KfSz  dn "],DString "IyAm "]),("IqYAZ",DMap [("g",DString " g P0AaKg4o3a t"),("jfpUf",DInteger 14),("zidj",DMap [])])]),("JnASqWUa",DMap [("n",DString "  B3e8YHp1tk "),("EnuTGX",DInteger (-42))]),("TzHwYBEHU",DList [DMap [("tmBziNY",DList [DString "",DString " 1R O  VB P",DString "hfS"]),("zzkmd",DInteger 4),("VnK",DList [DMap [],DInteger 32,DInteger (-60)])]]),("OK",DInteger 39)],DString "AE El On",DString "R69t"])]])]),("sYdwwI",DList [DString "isTS401 M  K",DInteger (-10)])]),("w",DMap [("HJnRpSY",DMap [("rZMA",DList [DMap [("tDaCil",DString "VpcEr er6c  ST"),("DvMsFTGhvw",DList [DString "3GbW5Xe",DInteger (-4),DMap [("ncmr",DList [DMap [("ZJcPqcTvGZ",DMap [("klsgJGRp",DInteger (-10))]),("HwjLreefBf",DString " "),("mkPmV",DList []),("Ljk",DString "ePX76 xg 3")],DString "j3 od 4 sqLL3w",DMap [("J",DMap [("mvuRM",DInteger (-26)),("OCGWvriyk",DString " a   6a i qv 0")]),("XgPrNk",DString "wMtSL5Ud9Ut")]])]]),("nGAcHtHJa",DList [DString "Mo"]),("IjYc",DMap [("QOljXVBRzN",DMap [("iOZJ",DInteger 39)])])],DString "S L5203 V",DString " 3Wz  u   9"])])]),("HylTwHXf",DString "5dp"),("xnTNZdYg",DList [DString " ",DInteger 13])])]),("QuBti",DString " P3 M "),("ArJnUBBav",DMap [("xv",DInteger (-12)),("crW",DMap [("eXjhKFhS",DInteger (-17)),("Fx",DString " W1qC v   m2k "),("Sbws",DString " Hp iT Bq moh1 "),("qzzbMWLPm",DInteger (-53))])]),("HIIeAbmJA",DList [DInteger 21,DString "izSw",DInteger 48])]),("CGboo",DList [DList [DList [DMap [("RfBsSCwTEw",DList [DMap [("WNcXHLTjf",DInteger (-13))],DInteger (-64),DString "900xIV DF1  "])],DInteger 59,DString "zWI7HF"],DList [DInteger 33,DMap [],DList [DString "9",DList [],DMap [("ePWjYkhbxT",DList [DString "p6q6X",DMap [("ISsJYXtWN",DList [DMap [],DMap [("yDSjXMPTWo",DList [DString "0  ",DList [DMap [],DMap [("LobGZdY",DString "3fm T8cwWqu8"),("ryxXu",DInteger 2)]],DList [DInteger 66,DMap [("lCfpW",DMap [("GwwpvRCDos",DInteger (-15)),("uXYBnHS",DString " 55M7 0BkqX ")]),("uftynDEGCv",DInteger 12)]]]),("jSI",DString "bBQ"),("N",DInteger (-17)),("uvCuYCK",DInteger (-44))]]),("ELpiS",DInteger 42)]]),("ljPDv",DInteger (-3)),("mfrfOYSUR",DString "d WHXrt yb  PYS")]]],DString "N 13Ziy 6 o",DInteger (-42)],DString ""]),("hoTs",DInteger 56)]],DMap [("OskXIuQJuM",DInteger 59),("vFoNnzI",DString "206vH  I 4")]]]),("NkazpG",DMap [("SqbBo",DString "JMx883 "),("Pdt",DList [DString "t"]),("tZdjfHDPbW",DString "dD9")]),("GkkBHT",DList []),("GCy",DMap [("ueyV",DList [DList [DMap [("ByacrYjE",DMap [("WQnxkWcg",DString " J9B0  1 ")]),("YabpIaTkma",DInteger 35)],DList [],DList [DMap [("rw",DMap [("kkzCC",DInteger (-13)),("JwWEMRYN",DInteger 43),("teWLtMuiq",DString "")]),("CpfhKHpHf",DList [DMap [],DList [DMap [("lHG",DList [DList [DList [DList [DList [DList [],DInteger (-18),DString " pq  ESqhK 9   "],DString " CS eF82"]],DInteger (-48)]]),("dQhs",DMap [("N",DList []),("IvfRyJuPYp",DString "zmG SkcjhR1cK"),("xlfSzkEujI",DMap [("dVcEnDFaA",DList [DString " PSo If  9",DString "rSC YZ1Hu F",DInteger (-67)])])]),("RhWriL",DList [DString "  1j6  6Y ",DList [DInteger (-43),DInteger 14,DList [DString "rh8   9aa"]]]),("Do",DInteger 41)],DInteger 68,DList [DMap [],DInteger (-60)],DString "  8OzlLZiud  vq"],DMap [("j",DString "XHc")],DList []])],DInteger 50,DString "yHcDOV4ZQ7S",DMap [("mLm",DList [DInteger 44]),("oH",DString "l2Xm")]],DString "eW1 T tD84em5O 8"],DMap [("Pw",DMap [("zPF",DString "1J 1  hvs1oZ"),("X",DList [])])],DList [],DList [DMap [("pwT",DInteger (-57))],DInteger (-52),DMap [("mQoI",DString "8 w6ZJjotk2Q 3kD")],DString "9yrc9xc2q4"]]),("eW",DInteger (-13)),("CS",DList [DInteger 16,DMap [("OjOOjLj",DMap [("eJCNZQaRS",DInteger 10),("ADbTEMtZkV",DList [DString " Lnof "]),("ZMBoi",DList [])]),("aGeXJw",DList [DMap [("RoeTmluao",DList []),("oVsDjxrK",DString "eB LwkH Y FKd"),("N",DString "i TN L "),("uHRt",DInteger 61)],DString "8fl9kWR"]),("mDWlZOVT",DMap [("T",DString "3   p8 12 G2")])],DMap [("hzOReJqLDY",DString "X "),("KNwViVVzS",DString "o32RUp  62")],DMap []])])]),("h",DInteger 13)],DList [DList [DList [DInteger 45,DString "nG K86Dd x",DInteger 25,DMap [("CSkAubk",DMap [("yIcI",DList [])]),("sPrGO",DMap [("UlwWHFlr",DMap [("MlYiD",DString "60P1vs 21W"),("h",DMap [("fKgvKINzaz",DString "Yd6SyZ L32pc6zR "),("pvOYkNeniQ",DList [DString "0xh9 V 9WNLuu2Dh",DInteger (-43),DMap [("rAusIB",DList []),("UmRvj",DList [])]])]),("zl",DList [DInteger (-63),DList [DMap []],DString "Gp8b9C"]),("T",DInteger 2)])]),("MSsLQHg",DString "420Q y5v tV "),("OiX",DList [DMap [("R",DString "c  m83X3")],DMap [("LA",DInteger (-12))],DString "Qcq",DList [DMap [],DMap [("oQEirw",DInteger 16),("GX",DString " FWs265"),("BrBOSbxD",DString "uUh   Sc0t0T0")],DInteger 4]])]],DInteger 67,DString "3x"],DInteger 38,DInteger (-66),DInteger 35],DMap [("vvnpobK",DString " 9a 3kt 4VE51"),("DGgFY",DMap [("EqYStcf",DInteger 26),("gnggXtf",DInteger 55),("zTLqrt",DMap [("tnYMkN",DMap [("ykhtqaoRw",DInteger (-7)),("SLec",DMap []),("vdk",DString "1s7R    0z9qQ"),("sZ",DList [DMap [],DString " 024",DString "KD C16",DList [DList [DInteger (-10),DList [DInteger 10,DInteger (-14),DList [DInteger 51,DMap [("LRFooZFwu",DList [DString " 97Jx2zGQJ9HMit",DString "jh LP30g3K2",DMap [],DMap [("Rtgq",DList [DMap [("oKH",DList [DMap [],DList [DInteger (-50)],DMap [("SVn",DInteger 11),("kWk",DMap [("qQmAa",DList [DString " 4goF"]),("cZXRUuzd",DMap [("ARoDy",DString "r l  o z d0 Z  ")]),("aH",DInteger 7)]),("yGUIKi",DInteger (-4))]]),("vksWbjg",DMap [("DEOGAR",DMap [])]),("IlcVLR",DMap [("PBF",DInteger 35),("Pfy",DString " ART4z um"),("IbYDhZf",DMap [("eDcqQ",DInteger 24)]),("kmfOr",DMap [("NMTTApAuGh",DString "4Dr 2Vv"),("RTxhmbjs",DList [DString "44rb c57N02av",DString "8 kW  K A ",DInteger (-53),DInteger 4]),("oHOyC",DInteger 22),("bTRj",DMap [("jq",DMap [("ucGtyDmyHd",DMap [("a",DInteger 28),("MMD",DString " FperL 8A vX"),("mBJv",DMap [("uQ",DInteger (-43))])]),("tE",DInteger 18),("fGq",DString "1rf 2  0  "),("SpnKEtb",DString "Czv o 0RM18K")]),("VQGDxNBrDR",DInteger 13),("mwFoSLac",DList [])])])]),("uHCj",DList [DString "  BG8",DString " by",DInteger 43])],DList [DInteger 53,DMap [("cWzQuVGz",DInteger (-15))],DMap [],DString "F "]]),("J",DString "s "),("WlLA",DInteger 37),("opuyQiuR",DMap [("e",DString "z   w")])]]),("cbVzRzgSA",DInteger 16)],DMap [("sIU",DList [DInteger (-58),DString "U  ",DMap [("NYSQ",DList []),("JZXARLOr",DString "1 PiC2l5aK"),("Y",DList [DInteger (-37),DMap [],DString ""])],DInteger (-63)]),("wCXMO",DMap [("bNIAXgg",DString "aRWE"),("dgbez",DString "9d7a5Ncn D")]),("EucJ",DList []),("BmKBeHF",DString "1Y q")],DList [DInteger (-47)]]]],DString "q0Z  57x oUI0 00"]])]),("zgQukXYFVw",DString "c     5 7 S9QAQ"),("URODIapd",DList [DMap [("XxP",DMap [("CPDyuY",DMap [("akmBn",DString ""),("YbRQHPA",DString "Wfh")]),("iZdSREot",DString "1v")]),("IwKZoVtPMt",DMap [("TdCZT",DInteger 61)]),("YikUFDR",DInteger 22),("WfcIzMy",DString "4 3U2j kjNs")],DString " 3MJ 3 1",DMap [("MYAOorjN",DString " 0 Ad"),("p",DMap []),("fU",DInteger (-68))],DString "U1 074  V"])]),("mzRwcoDAjS",DMap [])])]])],DString "Cj0fX FR D",DString " geb2"]),("o",DMap [("SWaLJw",DList [DInteger (-1)]),("vKTZorzLX",DList [DString "DW  EBe5",DMap [("pECYwcxOTC",DInteger 14),("fOx",DMap [("ee",DList [DMap [("Q",DInteger (-43)),("YjTi",DInteger 68),("Gizsh",DInteger (-18))]]),("FEP",DMap []),("kc",DList [DMap [("jstFWye",DString "5v BGeqMksq D9"),("yafUCyfeJ",DMap [("B",DInteger 10),("fedArz",DList [DString "9dH3 bo k"]),("hSnsYJg",DInteger 55)]),("qWHoQbU",DInteger 47)],DInteger (-18),DInteger 17,DList [DMap [],DMap [("qwVq",DInteger (-4))],DString "1 0X16e5v",DInteger (-23)]])]),("HftTyVN",DList []),("cabwjWECQ",DString "a 9l  620VR0")],DString "S  o 7g",DMap []]),("veUPlGLW",DList []),("khaWf",DMap [("gaq",DString "X3 I 9AT08"),("tsfgwWzsL",DInteger (-35)),("bViY",DList [])])]),("vbdrFGMZfr",DMap [("iz",DList [DInteger 41,DInteger 12]),("dyJbkaTA",DInteger 45)])]])]]],DInteger (-24),DMap [("RiFybeC",DInteger 38)]]]),("MghdHR",DString "ny5V s27GB29")]),("gXvXFHpPF",DInteger (-64))],DList [DMap [("lHeWqyFP",DList [DInteger 38,DMap [("Fj",DString "0 q 1  6JE2")],DString "wyR  ",DString ""]),("Wrd",DInteger (-6)),("zZ",DString "uqiYv")],DInteger 64,DList [DMap [("mXzz",DString "E1l6E  06 7")],DString "5e  4Z MTDy",DMap [("WrXfFMQsQB",DInteger 37)]]]])],DString "1"])]]),("EqgNSe",DList [DString "1J1JV NVx  X"]),("J",DString " id QNrj8oI")]),("sREe",DList [])],DInteger (-34)]),("eiGqRRUI",DList [DMap [("jt",DList [DInteger (-64)])],DList [],DInteger 45,DString "Z"]),("kXCBjc",DList [DString "dM6qr0z8 7  D",DList [DString " P8SQW1   X",DString "6 I g9 h3o 0 a",DString "3 s"],DInteger 45,DInteger 5])]])])]),("U",DMap []),("LSaMOFO",DList [DList [],DInteger (-43)]),("UhkExJB",DString "bs   f0M   4")])]),("zmn",DString "E9F 8j  2KyF OE8")]),("huusG",DMap [("P",DString "N"),("W",DList [DInteger 32,DString "l88 j"])])]),("ZlHUCb",DString "M  NMn2U93 "),("perLO",DInteger (-43)),("NbEnU",DString "q0ayM0ZrbiaTa")])],DString "3h 8 "]),("IIfHB",DMap [("KM",DList []),("DF",DInteger (-22))])]]])]]),("L",DString "Ila 6F 7j "),("SpPIj",DList [DList [DMap [("vwtRbXI",DMap [("sGuSgtw",DInteger 48)]),("nCIMPMOxL",DInteger (-10)),("pKrFLY",DList []),("qSdwFUD",DInteger (-36))],DMap [("gNmRP",DMap [("m",DList [DString "g",DString "dosEQWk 0O",DString "8 1JQuQRb3 XELY"]),("NLEeBGLPwb",DMap [("GMZbsjp",DInteger 23),("KmKUADGU",DString "H4A3D53a 97R8")]),("P",DString "25SwN8XOc9e5")])],DList [],DMap [("e",DString "si70 P"),("bdssWQwh",DInteger 40),("f",DList [])]],DInteger 48,DString "2S"])],DInteger 53,DList [DMap [("RgnuyaO",DString " b  0X j33v"),("iSaUrTv",DInteger 15)],DString "Lc11rE"]]]),("lF",DList [DMap [("nTdTOMW",DList [DList []]),("VAjOm",DInteger (-3)),("eKcPHRFS",DString "gu 9X z7")]]),("cWVR",DMap [("UKbszSDG",DList [DInteger 29,DInteger (-49),DString "w   "]),("vLavxB",DList [DString "B   "])]),("jLAeKlNE",DList [DInteger (-50),DString "15 2 H6 2R 6wl ",DString " 2Fd6 h7R 9I",DMap [("wfAiqexP",DMap [("gVypJGJvq",DMap [("LFnhIram",DString "J"),("LGH",DString "bN4sy1M S 6 N4Hu")]),("gtO",DMap [("fqd",DString ""),("mluLDy",DString "7e JX"),("eElKEnbkBg",DMap [("BQF",DInteger 53),("FC",DString "6 2a"),("mkcNyF",DMap [])]),("YLc",DList [DMap [("LJNHP",DList []),("KodZ",DMap []),("sO",DString "eu17  Z u4pT H"),("GVhjgWJX",DList [DInteger 36,DMap [],DList [DList [DInteger 22,DInteger (-32),DList [DList [DInteger (-12),DList [DInteger (-19),DMap []],DList [DString "   d8 3 rMI 1",DInteger 1]],DString " q5 1 Za xj6"],DMap [("X",DInteger (-30)),("HFhZa",DMap [])]],DString "9   Ikm ",DInteger 62]])],DMap [("HIebeAxZ",DInteger (-41)),("TrATaL",DInteger (-39))],DList [DList [DInteger (-38),DString "N5YFZ07  ZA"],DList [DMap [],DList [],DInteger (-27),DString "  ME 57"]]])])]),("JxmHIS",DList [DList [DMap [("lMilZPS",DMap [("Hb",DString "Nf G4V "),("PDfrJPG",DString " 8Ap9k7  B"),("zG",DMap []),("QjFmJx",DList [])])]],DMap [("bkS",DString " 5  XcbVk"),("gEaDppRZ",DInteger (-13))],DMap []]),("Im",DMap [])]])])]]),("m",DList [DString "4d Me J02P"]),("Lheeyn",DString "r7S7 q0"),("GPmKYlHpG",DString "rs ml Z")]),("rDNRMFGE",DString "0 hO94RGD 9u  HJ")],DMap [("VgkqhrYZM",DString "y  "),("BrCQpDwb",DList [DList [DMap [],DInteger 20,DMap []],DString "L 5472IL Wh",DString "u4 rAH1o92 Eb"])],DString "ez2CT  g0HcVZ4 0"],DInteger 25],DMap [("MmivSExv",DList [DMap [("sfqE",DMap [("wHKFY",DMap [("MurTqPpO",DInteger 19),("tGlZJIwq",DMap []),("gi",DString "pY30 MoG "),("wMlOxJF",DMap [])])])]]),("SBrwB",DString "LoH")]],DInteger 45,DMap []]),("lFdNfeXTG",DMap [("u",DString "1d4k3de W Fpd9"),("RwSJjm",DInteger 49),("dHuwrPj",DInteger 16),("rb",DString "KjB  8t bR9pG")]),("xtuvLGYx",DString "")],DList [DList [DString "LFIm 2 J",DMap [("NDPTlcwyfR",DInteger (-14)),("NLZwmI",DMap [("UqS",DInteger 49),("b",DString " 07T4"),("QlopKcuFW",DMap [])]),("WLHOTE",DMap [("Wt",DList [DMap [],DMap [("ndJarOGv",DString " k  "),("DFRzPg",DInteger 19),("veWDwsWt",DString "U27B")]]),("RxPCERVMWr",DMap [("JnvDivph",DInteger (-11)),("nQWNxKqtX",DString "ng b33zK64Bli")]),("xX",DMap [("ikToS",DList []),("DylLxz",DString " v"),("rQU",DInteger 14)]),("A",DList [DString "XSuF  D95QV0 Hk8",DMap [("K",DMap [("LyABHCS",DList []),("UZH",DMap [("qNKg",DInteger 68),("nOUqB",DInteger 11),("OWNDDhX",DMap [("EZKixbD",DInteger 9),("RznHOZKk",DMap [("yzpgTDP",DList [DMap [("XmdbKs",DInteger (-2))],DInteger (-11)])]),("qvhms",DString "  8lRjD"),("SdMhMfRw",DMap [])])])]),("UV",DString "0Aut6 ncH "),("qRHm",DString "4p 0b9r   38")],DMap []])]),("mSthZojV",DString "z QUS  INKb29 ")]],DMap [("Abqhf",DMap [("WkxGx",DList [DMap [("kq",DInteger 51)],DInteger (-6)]),("GitfWDa",DList [DList [DInteger 3,DString "d 5T4Tb0k16Z6",DString "C8 S4yS  23o Z",DString "298 s  5 Ya6K "],DInteger 20])]),("gke",DMap [("pisuKP",DString "w 16661 1N66iTf "),("oRySkItbM",DInteger 28),("tLAHzhae",DString " "),("cAf",DString "xbWK5u  ")])],DString " 81L  P  2cJDi"],DInteger 25]),("JqIz",DList [DString "53y9Oo",DList [DString "FiuSq"]]),("TfOG",DList [DList [DList [DList [DString "hm  N V  KCw m",DList [DInteger 1,DList [DString "l3OB pZB1 m1  ",DList [],DList [DString " G  41tm",DMap [("ytYF",DList []),("jChdxhqfn",DString "K "),("waxVeOGP",DMap [("xQjfXwfk",DString "X30 Z4Nj uiE"),("dcedqNA",DString "0xUyP 3 W1K afB "),("Uaggk",DList [DMap [("VnVQ",DString "a9"),("ZgUHy",DList [DInteger 34]),("gdZ",DString "J 5")]]),("z",DMap [("clnj",DList [DList [DInteger (-4),DInteger (-46)],DString "   80Q94Pq",DList [DList [DString " 8u T8w13K",DInteger 52,DInteger (-13)],DInteger (-25),DList [DInteger 2,DString "w M0s2LGc xQ ",DList []]],DInteger (-4)])])])],DList [DInteger 11]]],DMap [("m",DString "o5SRGW A"),("No",DString "F")]]],DString "8 jVR       j",DList [DList [DList [DMap []],DString "tp J9j "],DList [],DList [],DList [DString "TRGi",DMap [("pLU",DMap [("QDDsPQKsj",DString "hJNpye oj"),("CKKDOvA",DList [DList [],DList [DList []]]),("WeMO",DString "25e 91")]),("XYlO",DInteger 34),("B",DList [])],DList [],DMap [("KZv",DList [DList [DString "o k13oHW97 8",DMap [("BlMclITg",DList [DList [DList []]]),("gYggZxN",DMap [("qCVLGLGI",DList []),("dMJ",DList [DMap [("wTPdoSukL",DInteger (-66))],DList [DMap [("aNHbybot",DString " v5vP8FMu HM0f"),("mzVyLdb",DString "u3 a9 BUF")],DList [DInteger (-45),DInteger 27,DList [DList [DInteger (-7),DInteger 14,DMap [("ZbosjY",DMap []),("OypQnO",DList [DList [DMap [("ivJsjynP",DList [])],DString "  0HwsrR1RN  x",DMap []],DList [],DList [DInteger 30,DInteger 62,DString " 4P n7 ",DInteger 41],DList [DList [],DMap [("va",DMap [("Nu",DString "MrMlk f   d0 "),("xxMGVdVFMn",DMap [("cSTL",DInteger 60),("Qh",DString " E")]),("RQnPdyOMvm",DMap [("omknk",DInteger 66),("KLRYEm",DString " 2 U  ")]),("soNEeQ",DMap [("ZoWnRu",DString " "),("dxmZyl",DInteger 57),("Cxp",DList []),("KjAhECXoTI",DList [DList [DInteger 32,DList [DInteger 33,DInteger 25,DList [DString "1Wn9 e034 5mNc",DString "L E",DInteger (-37)]],DList [DList [DMap [("KVWjmySlSm",DInteger 35),("Zsg",DString "Re5N3 "),("OEczDO",DMap [("b",DInteger 12),("bAVWAUzN",DMap [("ErZpuSnhc",DMap [("ZZfe",DString " f t"),("kmAWu",DInteger 2),("vWY",DInteger 18)]),("astfYYDF",DInteger 3),("XQrKwc",DMap [("pj",DString " Mb 4W E"),("HhpHK",DList [DList [DInteger (-27),DString "da3y",DMap [("shyOtsGPCJ",DString "2TYpw6MVl5 uD"),("bitTYt",DString "14u"),("t",DMap [("vVQwvGVR",DList [DMap [("kZB",DMap [("NQJ",DString "5W5L 7x2IVH0 "),("pbdP",DList [DString " bPIL1TI89C"])]),("WAznW",DString "DN1149 3mEXnBC4"),("P",DString "Z6k8zU")],DString " M014  7T",DMap [("UxtUIiWs",DInteger (-56))],DString " 9t6940k7 a  "]),("SeCh",DString "Xf4iGmdfe1"),("D",DList [DString "MFNMaJ",DInteger (-27),DInteger (-58),DString "03 FU 1"]),("XlHGedu",DString "12u2YilQ pZ3   ")]),("zBpoxUITg",DString "6 e eP e a")],DList []]]),("Kayojml",DMap [])]),("HTtyxfLYe",DInteger 51)]),("xwKjh",DMap [("RRSeIqbJZN",DString " 905 khvu45i9V ")])]),("WdpmYQ",DString " Vfx kx3W  V8")],DList [DList [DList [DList [DList [],DMap [("yFNbzqV",DString "J"),("z",DInteger (-62))]],DMap [],DList [DInteger (-6)],DList []],DMap [],DInteger (-45),DMap [("teWPm",DInteger 26),("WnR",DInteger 50),("kR",DMap []),("qO",DInteger 12)]]],DString "Nj 47",DString "32X 075   Z d"],DString "3P 8  b0y rk"],DList []],DMap [("mg",DInteger 25),("nCWRlw",DString "216ceA "),("CwiM",DString " 9D t3 gh")],DString ""])])]),("Q",DInteger (-29)),("thwO",DString "25NAuCR")],DMap [("yfKyDLk",DList [DInteger 2]),("eQfYfXA",DString "9w r"),("mXCSEJ",DList [DMap [("Ghn",DMap [("EZLby",DMap []),("NS",DList [DInteger 50,DString "A"]),("H",DList [DInteger (-23),DInteger 60,DInteger 11]),("MeWr",DMap [("QrwbthYp",DList [DList [DMap [("lKynqTC",DInteger 23),("cYAckdz",DInteger (-36)),("aHZETfE",DInteger 39)],DList [DString "6E0",DMap [("ujnLI",DMap [("npap",DList [DString "51k6O JS"]),("WwVIjRyUP",DString "  H  nTcO lwReP"),("lUkrIznF",DInteger (-15)),("nhNxP",DString "mxlO  5")]),("Lesqgvwh",DMap [("kmnqyv",DInteger (-7)),("FJu",DMap []),("j",DInteger 21)]),("EhLVoUHi",DInteger 52),("UWztx",DList [DString "    Zu3Z",DList [DInteger 46,DMap [("fIupvf",DInteger (-46)),("sWPzZY",DMap [("wO",DString "1 L5 G  68"),("yMDtpWft",DMap [("bm",DList [DInteger 45]),("eypnx",DInteger (-19)),("zIC",DString "pp8i 1dfbE V "),("fjOrRQQJzd",DString "M  5sf1")]),("DbCvHL",DString "R  E 5"),("GL",DList [])]),("ynGZ",DList [])],DMap [("mK",DString " t"),("Ylj",DMap [("lVcBZ",DString "P0g")]),("QTvyerCpcX",DMap [("EpZysg",DMap [("JTAEjlVdpn",DMap []),("MQwaimr",DString "dBp2"),("eKSmkTeZo",DInteger 63)]),("qKtquVd",DInteger 17),("qJYdGnSmYF",DString " "),("VlhkKbKEg",DInteger (-20))])],DList [DMap [("cxzwNddzC",DInteger (-63))],DInteger (-61),DInteger 37,DInteger 31]],DMap [],DMap [("pBrzRNUaHZ",DString "v05004 Kv8m  0 7"),("OcBexYqUtB",DMap [("qVdAaVORKC",DInteger (-45)),("POuUr",DInteger (-48)),("vAT",DMap [("G",DString " v x9sc88aQ s"),("RujirI",DInteger (-24)),("i",DInteger (-21))]),("EZbbufmCVQ",DString " 7E X")]),("zxkDf",DMap [("IG",DInteger (-4))]),("WwiJOdv",DList [DString " 299J J5"])]])]]],DString " M18y AD "]),("jOuavAG",DMap [("F",DString "cO330BO uI062t"),("SvnZKT",DString "xs7"),("MGHUFu",DList [DMap [("WlyPpc",DInteger (-32))]]),("mphbjAj",DMap [("FYzLbk",DMap [("dGobNWL",DMap [])]),("CgVaUROCTE",DMap [("bzMzyrno",DInteger 22),("vf",DString "O"),("w",DMap [("sAXRcI",DList [DMap [],DInteger 55,DMap [("PzOPAJC",DInteger 44),("svhvbEfq",DList [DInteger 25,DMap [("p",DList [DString "EDW ",DMap [("Pi",DString "4 L4 pz rMr  XM")],DString "43kl Z  "]),("qxpxMe",DString "kn w"),("Y",DInteger 60),("iGK",DMap [("RIsfjirbOx",DInteger 37),("yovAmIFko",DList [DInteger 65,DMap [("hbERiVwga",DString " apUg  64 ")]]),("w",DMap [("RfnWc",DInteger (-1)),("LuE",DString "7L"),("Lz",DString "MH3")])])],DMap [("cEe",DList []),("rsAsjwQ",DMap []),("GZUc",DInteger 51)],DInteger (-52)]),("XoULICTxo",DString "e k16s 2n skh"),("sVDdZf",DString "Tj1 ")]]),("FrkAfqEag",DString " ux")]),("A",DString "M 5 5l4 dByATBl")]),("echv",DList [DInteger (-14),DMap [("EAAvZgRj",DString " 6p6 0s l H"),("SUngkLl",DString "k "),("MKWdSobV",DInteger 36)],DInteger (-5),DString "dm"])])]),("bk",DMap [("adOMlXtj",DString "6QbA5 0KH5p kotd"),("M",DList [DMap [("UJ",DString "23 a  BjG  "),("XVFUOS",DString "38Qpli"),("gjZkP",DMap [("PtWXKzc",DList [DString " 4 MwQj3X",DInteger 21,DMap [("ItNRN",DInteger 15),("KxqzU",DList [DList [DList [DString "v 78X 3O5b4 40eq",DMap [],DInteger (-59)],DMap [("kiJFgcuwCR",DString "941Nw"),("PdjPI",DInteger (-6)),("H",DMap [("b",DMap [("Wq",DList [DMap [],DMap [("oEIEzSc",DMap [("dRFKpaX",DInteger 62)]),("CcTIQBLsZs",DString "N A  lb3 MJD N")],DString " ",DList []])]),("d",DMap [("BASxZaCX",DMap [("f",DMap [("szda",DInteger (-60)),("zlDZLiSkx",DInteger 12),("ohuT",DInteger (-9)),("s",DMap [("pyxqiBcZv",DString " e0")])]),("BPEoPiTCPj",DList [DMap [("psPDMQTe",DInteger 48),("RorDFjxB",DList [DInteger (-67)]),("IPqos",DList [DList [DString "7xg2ONe"],DInteger (-18),DInteger (-60)])],DMap []])]),("iZryH",DMap [])])]),("v",DMap [("qXvsUIHwv",DString " 9gt yH37 wTNn  "),("JOOsV",DList [DMap []]),("dVoC",DString "   N 4")])],DInteger (-53)],DInteger (-35)]),("RFOHY",DInteger (-40)),("aKFbPMFZvA",DMap [("KjbK",DString " yNu9k  x"),("AnSN",DInteger (-17))])],DString "aaGsRr0y "]),("zW",DString "28 Y 6AV3 4u2IB")])]]),("KmLukU",DString "  A"),("E",DMap [("qDFmovUWkp",DList [DMap [],DInteger (-34),DInteger 9,DString "ZM1 Z  32cJOg 5l"]),("JhoUHyF",DMap []),("JO",DList [DMap [],DMap [("bKnxsF",DMap [("Sul",DInteger 54)]),("ZEBwRyUBgW",DInteger 40)],DInteger (-45)])])]),("Hn",DMap [("faW",DMap [("BYf",DMap [("IVbv",DInteger (-31)),("BvEYcTpPJd",DList [DList [DString "  FJP aY6Ii",DInteger (-65)]])])])])])]),("xH",DMap [])],DMap [("G",DString " 3"),("Qork",DMap [("CreEL",DString "q6YeK We")])],DString "B4Y"]),("WSKOKW",DMap [("NRFYdkApkS",DInteger (-8)),("b",DList []),("O",DMap [("OY",DString "qnwYh W275 3tSy "),("szd",DList [DInteger (-14),DInteger 51]),("E",DString "   qY2 3E2P "),("DQJWUVb",DInteger (-32))])])],DList [DInteger 30,DString "USUuXgLJ8",DList [DList [DList [DInteger 23,DMap [("dFuxvpUFOJ",DString "fLOT 3g"),("JexPlNna",DList [DInteger (-10),DString " 46b    1 ",DList [],DInteger (-40)]),("xNjpZ",DList [])]],DList [DList [DString "2AF 4 l15KbdZ ",DList [DMap [("vZf",DList [DString "0q Gl3i",DInteger 35,DString "a n 5r 7j ",DInteger (-47)]),("s",DList [DList [DInteger (-10),DInteger (-60)],DList []]),("pHkQ",DMap [("CGW",DString "clSve"),("bzzchMvl",DInteger (-33)),("ELEQObV",DMap [("DJzirQj",DInteger (-12)),("DJneiiIhk",DString "jK ")]),("LhIrd",DInteger 41)]),("BAl",DList [DMap [("Wl",DString " 4"),("wdURKbQ",DInteger (-38)),("dwc",DString "7BX"),("xZkp",DMap [("TTbU",DInteger 13),("zZXwSBeYa",DList []),("tFGSedmzO",DInteger 12)])]])]],DInteger (-17),DInteger (-62)],DString "Ut  9W Hh4K 6l9 ",DMap [("yGZdNu",DList [DString "7  97k AeP2JH"]),("UYBcM",DList []),("hUkhoqb",DInteger (-13))]],DInteger 13],DInteger (-34),DInteger 43,DString " pDMQM5"]]]])],DInteger (-38)],DInteger 10,DList []],DString "82oTq48    7"],DList [DString "  4   JzJ q3h   ",DMap [("xRcJha",DMap [("Xb",DList [DString "23V4dX e3d i  4P",DString "2nqEwoC  "])]),("FluPEvg",DList [DString "0   vdiTib 1",DList [DMap []]]),("bVgBVpwz",DInteger (-50))]],DString "7Zt "],DInteger (-66)]),("wBXzYEYJ",DString " "),("KQAzVTV",DString "i j 28")]),("WrjPpO",DList []),("JXfXPudx",DInteger (-43))],DString " 21cr 80   "]])]]]],DMap [("WM",DList [])],DString "W "],DInteger 51,DMap [("mY",DMap []),("BR",DMap [("TMQija",DList [DString "YzSY"])]),("IyRbqrs",DList [DList [DString "m9q A JUhZh  9d"]])]]),("LhUGGTTrSc",DList [DInteger (-26),DMap [("x",DString "8 W")]])])]],DList [DMap [("CRFQKzLcZ",DList [DInteger 50,DList [DInteger 17,DMap [("WMU",DMap [("aiGEJtvQs",DList [DMap [("lgIC",DInteger 15)],DMap [("ZxOjaTVdip",DMap [("Cu",DInteger 60)]),("LGKgIONZs",DInteger (-3))],DMap [("gLku",DMap [("jOm",DInteger 30),("tWVYsnsOac",DMap [("SzYdCnot",DInteger 38),("FaI",DInteger 12)]),("zcEDpCUxNW",DString "21 R "),("CBkd",DList [DList [DMap []],DMap [("c",DMap [("ziNnlJNS",DString " 1 97M3Q")]),("jRORtBzXR",DMap [("uQfCVJJPWH",DList []),("JMXHuBB",DInteger 15)]),("QoWbC",DList [DList [],DList [DInteger 12]]),("c",DString " n hy16Ou zo")],DInteger 66])]),("Fmiqx",DList [DList [DMap [("LLEX",DList [DMap [("E",DString "R0pfaM5E7qO H0A"),("rbGE",DString "5w"),("TLCxI",DInteger (-51)),("vGvD",DMap [("esksj",DString "71 2ZuG bT EJt"),("zSMI",DMap []),("yak",DInteger 0),("Ndlpp",DInteger (-11))])],DInteger 3,DMap [("gC",DString "")]]),("iGhKCUeVm",DString "5S36CMBe")]]]),("shXrFr",DString "e    2D3")],DMap [("johA",DMap [("dI",DMap [])]),("nZPEKfscA",DMap [("InhnVJYV",DString " X0l  s m2"),("GZAEtIRQX",DMap [("R",DString "aO"),("KWpqOzD",DString "y5 RB"),("TKCEJdtVb",DList [DString " 968",DString "9Ft My"]),("TJt",DMap [])]),("vzCfo",DList [DInteger (-58),DList [DString "x F Ofr0F133",DString "a i VxwpwSL2",DString "9 b"],DString " L NNR 1Dy  "]),("TqqLHpfxD",DString "45 b2m6a0eoccH")])]]),("XxBJvJz",DList [DString "Jzb4UFFn4Ws",DInteger 29,DString "H0",DString "DVMJED zFhK"]),("OpbMvbJlTm",DInteger 38)]),("znCf",DInteger 4),("z",DMap [("DRYGFmLVda",DString "z  3 ")]),("ujtA",DString "ih8G")]]]),("MeYbHZfeS",DInteger 29),("ZniYj",DString " 912"),("nACbQSKKOE",DMap [("FkapwwMtJ",DList [DInteger (-21),DMap [],DString " pUG  0x7 "]),("NZOtL",DMap [("TfnVJFkE",DList []),("vqVPX",DMap [("aWkaNct",DList [DMap [("mrSmXBb",DList [DMap [("K",DInteger 21),("B",DString "Nh1608CF1ZaX"),("CuFmbv",DMap []),("YOEEUisZOU",DString "  0 fx x9  V7d ")],DString "m5 7 ",DMap [("pFfSpId",DList [DString " ueh R n",DList [],DInteger (-55)]),("Li",DMap []),("e",DInteger (-18)),("IazhwJh",DMap [("yMdVMjF",DInteger (-16)),("LJagRSR",DList [])])],DString "63"]),("dgZsAGsw",DMap [("yirPad",DInteger 49),("y",DList [DInteger 49,DInteger 19,DInteger (-10)]),("QeUXbSHgM",DInteger 3)])],DString "1 5XrqLL518Q1"])])]),("V",DInteger 23),("gKzUsFYCfh",DInteger (-64))])],DString "k19Ib PY1  0 ",DList [DList [DMap [],DList [DString "PJN27 16 F  N5X"],DList [DInteger 57,DMap [("OrJgoZJq",DMap [("TcRgSra",DList [DMap [("gQQtS",DMap [("TyvtZtDIg",DMap [("CpEZPOtIf",DString "72mYy S h")]),("l",DMap [("vBiG",DInteger (-26)),("WTOEyL",DList [DMap [("Tcxt",DString ""),("O",DInteger 13)]]),("egNMd",DInteger 38)]),("pmlmItF",DInteger (-50))])],DString " T    j  ",DList [DString "E41gqo B",DList [DMap [("AdwyXO",DList [DInteger 54,DString "6iHx42 I9N1L1",DMap [("JHJIy",DMap [("ZohFb",DMap [("M",DList [DList [],DString "LWlg7Q044"]),("UTPhh",DString "8  0Dt49W"),("YzEqVFEs",DInteger 31)]),("g",DList [DString "  uh"]),("CFsYqPD",DMap [("awbJPHtJQV",DMap [("EwNvR",DInteger 3)]),("GObg",DMap []),("yOD",DString " i5 "),("UjYINCdpnD",DInteger 31)])]),("VUcdnygN",DList [DMap [("tBYjKQxbY",DInteger 1),("XKN",DMap [("pcNUePSVzk",DString " X n3  A "),("rVMg",DList [DInteger 65,DInteger 6])])],DMap [("y",DMap []),("Oi",DInteger 7),("LFG",DMap [("irERQ",DMap []),("GZbYzH",DList [])])]])],DList []]),("xt",DList [DMap [("mHz",DList [DList [DString "b n TmOl T",DMap [("oRGFRW",DInteger 17),("Mmu",DInteger 35),("jFK",DMap [("eRSZtgOEqc",DString " SI1t39 2"),("sTFr",DList [DList [DInteger 47,DInteger 42,DString "Jwj  SSDc 1 n3  "],DString "MuwCX1iQ4H67RH",DString "4 ",DString "J "]),("ntSjaFk",DList [DList [DInteger (-59),DString "oQ3p  9w  l7B",DString "   T 3mc ",DList [DInteger (-17)]],DMap [("uspoaAe",DList [DString "KFZK br"])],DString "  V7"])]),("Q",DInteger (-65))],DMap [("GLhafQroVZ",DList [DList []]),("FTNrDt",DMap [("C",DMap [("XKBFVrF",DString " vVD2 qo  Uq   "),("tiOkCJXF",DString " Jy 9 "),("FfLszV",DInteger (-19)),("JJEVuoC",DMap [])]),("LkPULGzRQh",DList [DInteger (-19)]),("bsINwv",DList [DInteger (-54)]),("Vw",DMap [])]),("tUsivtoh",DMap [("mGDtxpVvq",DList [DMap [("BBBYIQCeqp",DList []),("PLA",DString "   1 5vv G 0"),("ShnB",DInteger 13)],DMap []]),("YqoKObPq",DString "FI "),("qZUoqdLUHq",DInteger (-33))]),("lJOh",DMap [("lq",DMap [("qzjhOSrvix",DString " t QV JEi46   37"),("qRd",DInteger (-54))]),("tnhdmIZFV",DMap [("u",DString "9ZL"),("wGdBvx",DInteger 17),("uuPJnHtut",DMap [("cB",DMap [("ntDDz",DString "q 19  5 9 "),("MSVrWil",DInteger 64),("Dc",DString " 3x23  x9"),("ouR",DMap [("QSKgzCjSm",DString "6z"),("uvATIEIcyB",DString "J LTm "),("WFTevqS",DString ""),("iZweq",DList [DList [],DInteger 0,DInteger 64,DMap [("oMLbKA",DMap [("PIigRRRO",DMap []),("wS",DMap [("QLqvlifchv",DString "BF R8Gp k 16d")]),("iIbHttIa",DList [DList [DString " yxc0OTM7 uH QbZ"],DMap [("CGDiV",DString " 5  Gq3 9a idjr"),("viZCJJr",DMap [("mCXfcJ",DMap [("b",DString "w9NUu p f")])])],DInteger (-9)]),("qVUttAMhSn",DMap [("NERqMMmF",DList [DInteger 2,DMap [("nf",DString " c"),("dnV",DInteger (-33)),("gCPOZUNik",DString "  bM")]])])]),("E",DMap [("YTEET",DMap [("OK",DInteger 49),("eJIKQPlGLp",DString "7  H6"),("yA",DMap [("DcCSXEdWHz",DString "Z 1NDPmfz  k"),("lNJpYfN",DMap [])]),("VnuxybZkW",DMap [("Ub",DInteger 7),("XSrX",DString "h 3uxv A9ve"),("WnDN",DList [DString "LCxn5 e s7EL5q g"]),("fYo",DList [DList [DInteger 20,DInteger (-2),DInteger 44],DString "9 ZsM MWM 5OItr"])])]),("UOcyqfVT",DMap [("HuCSDD",DList [DMap [("FlSkU",DString "0 9 kMo9p  0Xw")],DMap [("pDMmcHDLo",DMap [("EyanXtek",DList [DInteger 6,DString "F  JN1 M",DString "mI",DMap [("ylnu",DInteger 4),("IelFPHb",DString " ufPe17 944i"),("UZYPKfR",DString "BZHA9q9 s")]]),("uIbruxEld",DMap [("Ejg",DList [DString "Xs  iCyZc8w6 4C2",DList [DList [DInteger (-28),DString "5qIR   m1os"],DInteger (-9)],DMap [("tgmC",DString ""),("waCWMUnF",DList [])],DMap [("FQRy",DInteger (-10)),("rshi",DString "GhoLftp J"),("UMC",DString "6Uy8V24j  3J0zx7")]]),("DRwlLQo",DString "9Tc zd"),("AXSKCVDMh",DString "P g 8   3 "),("IG",DInteger 40)]),("gBoGuAelE",DList [])]),("uhJwOttqex",DMap [("xdj",DMap [("QnBzphDoM",DList [DInteger 49,DMap [("Aks",DInteger (-11)),("GVYiBlDUb",DMap [("Ffi",DInteger 16),("toXvGbq",DList [DMap [("dSKhop",DString "  GCZ2a9 q Q"),("HUH",DMap [("QFCOK",DString "71"),("NBfiPrXfQy",DList [DInteger 48,DString "E43  zvxkrqE"]),("w",DString "E 6I 03xQY2MjHu"),("iy",DString "X")])]]),("x",DMap [])]),("sPGMPYI",DInteger 59),("ZcomogFyMa",DList [DList [DList [DMap [("Th",DList [DMap [("oI",DInteger (-53)),("NHaBeEN",DString "vmtKk 5")],DMap [("bcut",DInteger 65),("vnlGYe",DInteger (-40)),("EgiSBUM",DString "Mpo y   R3  ")],DMap [("spPzaM",DMap [("bNB",DInteger (-3)),("bRsXl",DInteger 21),("nidZ",DList [DMap [("hcHL",DInteger (-17)),("wlSXXTHlme",DList []),("BDCgi",DList [DList [DInteger 38,DInteger (-16),DInteger 17,DInteger (-42)]])],DString "3NXb 7"]),("oKe",DList [DString "r  hm 6 1SwJwt",DInteger 13])]),("nbVl",DMap [])],DList [DString "",DString "  yh 08d",DInteger 8,DMap [("ExZgTuMRO",DMap [("yKhZ",DMap [("ysffWY",DInteger 30),("wIYFNo",DString "")]),("gVvwcM",DMap []),("PBhnx",DInteger 16)]),("GRRDeXtuWZ",DMap [("qPnwfSf",DMap [("EyiTwh",DMap [("XLH",DMap [("UYOXyHEFSk",DMap [("jhVWjXHR",DInteger 60),("zlsLk",DInteger 59),("CJIe",DString " 07bD FS3pv")]),("x",DInteger (-49)),("AppWf",DMap [("nZfbNVKHHW",DList []),("RQufN",DMap [("Mdv",DMap []),("M",DInteger 44),("ecKIvUhJ",DString " Ih0 c hkL")])])])]),("gQnGOEg",DMap []),("KPv",DMap [("BefjCCI",DList [])]),("jlRCF",DString "2z3Yym9 7")]),("TerrczVndF",DList [DInteger 36,DInteger (-44),DList [DString ""]]),("PwUmhCGlPe",DList [])]),("YCSHDYZ",DString " F2mf91 5L2"),("cwscRtn",DString "VV ")]]]),("Nc",DMap [("zjfbCqj",DList [DInteger (-34),DInteger (-2),DList [DString " "],DInteger 36])]),("X",DList [DList []]),("w",DInteger (-66))],DString "C cH j  8  U",DMap []]]])],DList [DInteger (-60),DMap [("P",DString "snfAsA7WGDs"),("GchRrCQZ",DString "e3 5947 B 8 ln")],DInteger 30],DList [DString ""]]),("ERDWaC",DMap [("QwfGJIn",DInteger (-48))]),("Iv",DMap [])])])],DString " RfS 0gf0M1",DString "s KE 043 S6OQ39w"]),("pJzXxtd",DString " 66F ml Q 01")]),("XJK",DInteger (-58))]),("xvjRYb",DMap [("TiPhw",DString "us 2M t6Z  a2 L")])]])])]),("oMsm",DMap []),("kdpTMOCYda",DList [DMap [("RTsmKXdag",DList [DInteger 47,DInteger 66,DString "y0 aIg1 98F5 ",DString "YX 56g KG6"]),("jLf",DInteger (-18)),("rXBMvagG",DInteger (-24)),("VMWpGAlsmf",DMap [("xBBEeyrB",DString "S 2 V ac"),("Q",DList [DInteger (-55),DMap [("wHGgQd",DInteger 7),("SkQBzswv",DList [DMap [],DString " 3L 4QJKpvmqfb O"]),("IeCO",DInteger (-67)),("M",DList [DMap [("reLsO",DString " zp")],DList [DList [DInteger (-32),DInteger (-58),DInteger (-5)],DList [DMap [("JUlXKD",DString ""),("fZCdVlnkZz",DInteger (-65)),("OJqNFX",DInteger (-5))],DString "m5lN  1Lw",DInteger (-36)],DString "4Z8W48 S"],DString "EHM6 9j qC 6 t 9",DString "Zk gN 4i8 53"])],DMap [("ZUHMEZ",DMap [("GEbJMLRhhq",DMap [("CiIcUCj",DMap [("Uv",DInteger (-4)),("wnzQJTI",DInteger (-33))]),("KaNhMPzEj",DList []),("FalTAUBmRE",DString "pPMMasjCgKPZn")]),("bNYOQDfi",DInteger 60),("XkAoJG",DList [DMap [("yLIVZTrrnE",DInteger 18),("lpRX",DInteger (-3))],DList [DMap [("WruIVZVTdi",DInteger (-66)),("TFd",DInteger (-43)),("TFLGyg",DString "0z PGry9  NO4y")],DMap [],DInteger (-31),DString "5  05 7xPAh 1 9"],DMap [("Tl",DString "S0 7 8T Q uF8T Y")]]),("JenT",DInteger 28)]),("Sgk",DInteger (-17))]])])],DMap [("gHGLy",DMap [("colioBiLYT",DString "5hf 2Bd VY1P")]),("BkaUu",DList []),("F",DMap [("J",DInteger 8),("pljWOrlGz",DInteger 26),("lB",DList [DMap [("R",DMap [("Rsvv",DInteger (-58)),("YhynsOlyh",DInteger (-54)),("nLIkSw",DInteger (-30)),("Lc",DString "9Ime i 5Cz ")]),("QFQjEYqIyR",DInteger 44),("XJjpOkhZfw",DMap [("yOoxf",DMap [("UeuFsKD",DList [DList [DMap [("fNmzdPkB",DMap [("eDcHw",DInteger (-56)),("Gx",DInteger 54)]),("ALcYCLAB",DInteger (-29)),("forsFBJ",DList [DList [DInteger (-21),DInteger 28],DInteger (-51),DInteger (-31),DInteger 5]),("Gme",DString "h  2QO   1Bqp78 ")],DList [DList [DInteger (-7)],DList [DString "1Bxp b 5 p4xSdq",DList [DMap [("pRoPQv",DString "I 8Qp55N"),("HJ",DMap [("PKTMuYDhAz",DInteger (-29)),("utrpn",DString "")]),("NX",DString " M4 FNCnQKg0"),("tPSsVWEnVn",DInteger (-8))],DList [],DList [DInteger (-34)]],DString "gkS7",DList [DMap [("wktXDTz",DMap [("fqI",DList [DList [],DMap [("FfuwnR",DList []),("QKhzw",DMap [("BRsxHzPh",DString "S")]),("Te",DString "4v"),("f",DInteger 27)]]),("nXTR",DList [DMap [("STZR",DString "6z8 X M gZ n4l L")],DList []]),("fmDjLyUvlC",DString "3 2c  ZNMfVYj Qb")]),("AONBLM",DMap [("OQCUPq",DList [])]),("hcWKrv",DString "Hi vAi3R 9")],DInteger 37,DMap [],DList [DList [DString " 68j w9S"]]]]],DList [],DString "X5yzYVdy5    T"],DString "pCl3oQ cfS3  d"]),("UbuAbitpQG",DString "32KJ "),("dM",DList [DList [DList [DList [DInteger (-15),DList [DMap [("FGEe",DList [DInteger 3,DInteger (-48)]),("sHCMRw",DInteger 8),("vRh",DInteger 23),("gXsKCavOFq",DMap [("xEZVw",DString "HYxrK 0nB"),("KFtMwNCbly",DString "  3i64 "),("hKef",DInteger 44)])],DInteger 41],DInteger (-38),DList [DList [DString "3 dh875Sfb Ad"],DInteger 0,DMap []]],DInteger 45],DList [DMap [("Q",DList [DString "4Icl"]),("kMENnnyTTI",DInteger (-56)),("BjWwtVwPl",DInteger (-11)),("kRiXHFgEK",DInteger (-60))],DMap [("UfT",DList []),("VQdJK",DMap [])],DString "x tfW"],DString "32p1YHwv1  E4uO",DMap [("x",DString "  FJXq"),("dpgxZ",DInteger (-36)),("SfTF",DString "1 2M3t qX146 8fs"),("vyB",DInteger 42)]]])]),("LNCMMHq",DInteger (-5))]),("T",DList [DString "0 o  y89 7 ",DList [DList [],DList [DMap []],DList [DList [DList [DMap [],DString " qj",DMap [("zDpWuHK",DString "k2 8"),("SVnrGDvgE",DString " vAfy9T5  y"),("b",DString "hF mno5PU692 Q97")]],DInteger (-12),DInteger 46,DInteger 19],DString " c",DList [DString "th 64un 5D21",DString "G 8BoZyzSW   08w",DMap [("NF",DString "n30 HybV0v 8d"),("AeC",DInteger 44),("qYT",DString "B "),("yA",DInteger (-14))],DMap [("HgOzd",DList [])]]]],DInteger 48])],DMap [("OmyKydfolE",DInteger 35),("SVF",DInteger (-33)),("XL",DString "W1 Kr4hE"),("jKjJuGnjDk",DInteger 18)],DList [DString "U90 94H  2",DString "3E 0Q  k0M",DString "3nUE2e9x"]]),("X",DList [DInteger 20,DMap [("grK",DList []),("i",DInteger (-68)),("M",DMap [("lirbo",DString " Pb  n36k0 W"),("FbuR",DString ""),("UIHa",DInteger (-49))]),("BcryAIpCVu",DInteger (-60))]])])],DString "  7aOGScEZ"])])]),("h",DList [DInteger 6,DList [DMap [("cxdTFnEPgg",DInteger (-9)),("TqWvUCGJto",DInteger 44),("ySZnnnlR",DList [])]],DMap [("weMvE",DMap [("RXnGceYd",DList [DList []]),("g",DInteger 11),("SZqNc",DList [DList [DMap [("uNiCVQS",DString "vnc ee t"),("DnPjVpA",DString " 18gf739"),("c",DMap [("JjPUJE",DMap [("ljZHBEbrTZ",DInteger 18),("wgrgYPrypP",DList [DInteger 25,DList [],DList [DInteger 0,DMap [("eO",DMap [("ckDQ",DInteger (-50)),("I",DMap [("cCbFbC",DString "U 3y 5 2J")]),("VhEQRMJ",DString "")]),("TSdmRXnrMO",DString "9 Q t A40 W"),("gMRU",DString "vQBJ"),("MWvN",DMap [("svQ",DMap [("EsdP",DList [DMap [],DList [],DMap [("qzPr",DString "qn iio  IW bI V"),("cuLB",DString "mZ6 6Od 5 6 6")],DMap [("xVm",DInteger 25),("sNvsYBRh",DList [])]]),("KrRAi",DString "G0")]),("rcjvBf",DInteger (-2)),("AmNHrLP",DInteger 48),("kHnsPxRak",DString "23bdEy")])]]]),("CEvx",DMap [("SWpkaJQf",DString "s")]),("sguBMoiJTi",DInteger 58)])])],DInteger 18,DString "t e9J  SCO",DMap []],DInteger 62,DInteger (-29),DInteger (-20)]),("WcjJ",DInteger 5)])]]),("R",DString " Hl hc9z Gs")])]],DList [DMap [("dqCyAqU",DString "J1 0T8 ou8"),("EM",DString " D"),("PA",DString "z")],DInteger 64]]),("yAs",DInteger 17)]]),("pYz",DString "66rm")]],DMap [("EyqX",DMap [("NMKhLvaKe",DInteger 1),("HrsauK",DList [DInteger 48,DInteger 68]),("pvsB",DList [DString " Weh8V p 2fB ",DList [DMap [("CR",DInteger 53),("lYC",DString "")]]])]),("femjnmyEyq",DList [DString "DP I4 0 Br"]),("zOoKbIl",DString "1XK  "),("DiGMDvpUr",DInteger (-6))]]]),("Mqq",DMap [("nM",DInteger 54),("YgEae",DString "D   4F324eS"),("OEHRWAQk",DInteger 23)]),("BF",DMap [("SXvQ",DInteger 32),("FukIwqfs",DInteger 62),("tNxKpdJc",DMap [("wyqzBj",DMap [("OT",DInteger 2),("WoDpVn",DMap [("Ow",DInteger 41),("Mp",DMap [("L",DMap []),("IIDkIKmQ",DInteger 14),("AfYsnOY",DString "g "),("jYqDQt",DInteger 2)]),("WCRXik",DMap [("JoPvMhoJZ",DString " aT1 X"),("SFEJa",DList [DInteger 68]),("LuI",DList [DMap [("sAqQjwfqA",DMap [("FMBrz",DMap [("MEWy",DMap [("lvsPdCDduk",DList [DInteger (-68)]),("Nr",DList [DString "ACP ",DInteger 45,DInteger (-49)]),("wZTfD",DString "1PO42G91D8 "),("lS",DList [DMap [("jzbdeY",DList [DInteger 60])],DString "6",DString "",DMap [("jhilHU",DInteger (-16))]])]),("hlkyPDIoPr",DString "hxA0   p8ku")]),("nuAn",DString ""),("aFmMP",DList [DMap [],DMap [("ya",DMap [("ch",DInteger 38)]),("aRfYBpg",DString "n71CiX Oq Zg  S"),("GxVMY",DInteger 18)],DString "HVQRM3nc5",DString "g26 59 67 W3 "]),("lBmTq",DInteger (-61))])],DInteger 52])])]),("LAcfDsStDF",DInteger (-63)),("hbLQ",DMap [("gcxTvtA",DString "Ja84u y  "),("vfKgYEkuJN",DList [DInteger 56,DList []])])]),("itcewK",DMap [])]),("elCRbVUMP",DString "")]),("IsgI",DList [DList [DInteger 57,DInteger 63,DMap [("vtZKv",DString " g  Yy4s "),("CcgtHQJJ",DInteger 20)],DMap [("oYNC",DMap [("RBLK",DString "i b "),("Jvy",DString " M89 16 B4Va"),("gdin",DMap [("ZaOxoS",DMap [("rXbD",DInteger 41),("pOfcdxoNh",DInteger 66)]),("PyakL",DString "G 6 258s15djT  "),("uHazh",DInteger 26)]),("hkHbnkL",DInteger (-40))])]],DMap [("b",DString "7  "),("jl",DList [])]])])],DMap [("DtwHs",DInteger 2),("MC",DString "Eo6ZupV0hj146S3J"),("vSe",DString "a n d m c 5  G"),("AlDNEwa",DString "m9  r0 ")]],DList [DInteger (-14),DInteger (-6),DInteger 68]],DString "Aq Hx 427"],DInteger 54]]

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


