{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument )
import Lib1 (State(..))
import Data.List(nub)
import Data.Either
import Text.Read (readEither, readMaybe)
import Data.Char (isDigit, isSpace)
import Data.Maybe (isNothing)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument "" = Left "Empty string is invalid"
--parseDocument a = Left ("bybys man " ++ show a)
parseDocument str = case getValue str of
            Left msg -> Left ("Could not parse line:" ++ msg)
            Right doc -> Right doc
--parseDocument str =  Left ( "va cia " ++ show str)
parseDocument _ = Left "Some error occured"

getValue :: String -> Either String Document
getValue str
     | isRight a = a
     | isRight b = b
     | isRight c = c
     | isRight d = convertToDList d
     | isRight e = convertToDMap e
     | otherwise = Left (
        if last' (getMessage d) == Just '$' then getMessage d
        else if last' (getMessage e) == Just '$' then getMessage e
        else getMessage c) -- Error message
     where
        a = parseInt str
        b = parseString str
        c = parseNull str
        d = parseList str
        e = parseMap str


parseString :: String -> Either String Document
parseString str
           | length c == 2 && (((head' c == Just '[') && (last' c == Just ']'))
            || ((head' c == Just '{') && (last' c == Just '}'))) = Left (b ++ "$")
           | length c == 2 && (head' c == Just '\'') && (last' c == Just '\'') = Right (DString "") 
           | (head' c == Just '\"') && (last' c == Just '\"') && null e = Right (DString (removeLast $ removeFirst c))
           | (head' c == Just '\'') && (last' c == Just '\'') && null e = Right (DString (removeLast $ removeFirst c))
           | c /= trim "null" && isNothing (readMaybe c :: Maybe Int) && head' c /= Just '-'
             && last' (trim c) /= Just ':' && null e = Right (DString c)
           | otherwise = Left (b ++ "$")     
            where (b, _) = breakOn '\n' str
                  c = trim b
                  (d, e) = breakOn ':' c


parseInt :: String -> Either String Document
parseInt str = case  readMaybe (replaceSpaceWithTrash (trim b))  :: Maybe Int of
    Nothing -> Left (b ++ "$")
    Just num -> Right (DInteger num)
    where (b, _) = breakOn '\n'  str
          replaceSpaceWithTrash = map (\c -> if c==' ' then '@'; else c)


parseNull :: String -> Either String Document
parseNull str =
    let 
        (b, c) = breakOn '\n' str
    in 
        case trim b of 
            "null" -> Right DNull
            _ -> Left (b ++ "$")


parseMap :: String -> Either String [(String, Document)]
parseMap str
      | length (trim b) == 2 && head' (trim b) == Just '{' && last' (trim b) == Just '}' = Right []
      | last' (trim b) /= Just ':' &&  possibleValue /= [] && head'(trim key) /= Just '-' =  
        case getValue possibleValue of 
            Left msg -> Left msg
            Right value -> do
                a <- Right (modifiedKey, value)
                (a:) <$> continueRec
      | last' (trim b) == Just ':' &&  null possibleValue && head' (modifiedKey) /= Just '-'=
        case getValue c of 
            Left msg -> Left msg
            Right value -> do
                a <- Right (modifiedKey, value)
                (a:) <$> continueRec        
      | otherwise = Left b
      where (b, c) = breakOn '\n' str
            (key, possibleValue) = breakOn ':' b
            rest = findSameLevelMap (str, str)
            modifiedKey = if ((head' (trim key) == Just '\'' && last' (trim key) == Just '\'')
               || (head' (trim key) == Just '\"' && last' (trim key) == Just '\"')) then (removeLast $ removeFirst (trim key)) else (trim key)
            continueRec = if null rest then Right [] else parseMap rest


parseList :: String -> Either String [Document]
parseList str
        | length (trim b) == 2 && head' (trim b) == Just '[' && last' (trim b) == Just ']' = Right []
        | head' (trim b) == Just '-' =
          if lastChar == Just '-'
          then if countSpacesFront b < countSpacesFront c
               then (:) <$>  getValue c <*> if null rest then Right [] else parseList rest
               else Left b
          else (:) <$>  getValue (replaceFirstDash '-' str) <*> if null rest then Right [] else parseList rest
        | otherwise = Left b
        where (b, c) = breakOn '\n' str
              rest = findSameLevelList (str, str)
              lastChar = last' b


removeFirst :: [a] -> [a]
removeFirst myList =
    case myList of
      [] -> []
      x : xs -> xs

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [h] = []
removeLast (h:t) = h : removeLast t

breakOn :: Char -> String -> (String,String)
breakOn s str = 
    let 
        (b,c) = break (== s) str
    in 
        (b, removeFirst c)


head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x


countSpacesFront :: String -> Int
countSpacesFront str = length (takeWhile isSpace str)


convertToDMap :: Either a [(String, Document)] -> Either a Document
convertToDMap (Right doc) = Right (DMap doc)
convertToDMap (Left msg) = Left msg


convertToDList :: Either a [Document] -> Either a Document
convertToDList (Right doc) = Right (DList doc)
convertToDList (Left msg) = Left msg


getMessage :: Either a b -> a
getMessage (Left msg) = msg


last' :: [a] -> Maybe a
last' [x] = Just x --base case is when there's just one element remaining
last' (_:xs) = last' xs --if there's anything in the head, continue until there's one element left
last' [] = Nothing


findSameLevelMap :: (String, String) -> String
findSameLevelMap (initial, temp)
   | null (snd (breakOn '\n' initial)) || null (snd (breakOn '\n' temp))= []
   | countSpacesFront initial == countSpacesFront (snd (breakOn '\n' temp)) && getCharAfterSpaces = snd (breakOn '\n' temp)
   | countSpacesFront initial > countSpacesFront (snd (breakOn '\n' temp)) = []
   | otherwise = findSameLevelMap (initial, snd (breakOn '\n' temp))
   where getCharAfterSpaces = head  (trim (snd (breakOn '\n' temp))) /= '-'
   

findSameLevelList :: (String, String) -> String
findSameLevelList (initial, temp)
   | null (snd (breakOn '\n' initial)) || null (snd (breakOn '\n' temp))= []
   | countSpacesFront initial == countSpacesFront (snd (breakOn '\n' temp))
     && head' (trim initial) /= head' (trim (snd (breakOn '\n' temp))) = []
   | countSpacesFront initial == countSpacesFront (snd (breakOn '\n' temp)) 
     && head' (trim initial) == head' (trim (snd (breakOn '\n' temp))) = snd (breakOn '\n' temp)
   | countSpacesFront initial > countSpacesFront (snd (breakOn '\n' temp)) = []
   | otherwise = findSameLevelList (initial, snd (breakOn '\n' temp))


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

replaceFirstDash :: Char -> [Char] -> [Char]
replaceFirstDash _ [] = [] 
replaceFirstDash a (x:xs) | a == x    = " " ++ xs 
                     | x /= ' '  = x:xs
                     | otherwise = x : replaceFirstDash a xs   

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart { numberOfHints :: Int  
                           , occupiedCols :: [Int]
                           , occupiedRows :: [Int]
                           } deriving (Show)

instance FromDocument GameStart where
    --fromDocument :: Document -> Either String GameStart
    --fromDocument doc = Left (show doc)
    fromDocument (DMap initState) = extractInitState
        where extractInitState
                | numberOfHints == -1 = Left "1"
                | (length occupiedCols /= 10) || (find (-1) occupiedCols) = Left "2"
                | (length occupiedRows /= 10) || (find (-1) occupiedRows) = Left "3"
                | otherwise = Right  (GameStart numberOfHints occupiedCols occupiedRows)
            
              getDocByStringFromMap initStateList string = foldl (\acc (s, d) -> if s == string then d else acc) DNull initStateList 
            
              numberOfHintsDInt = getDocByStringFromMap initState "number_of_hints"
              occupiedColsDList = getDocByStringFromMap initState "occupied_cols"
              occupiedRowsDList = getDocByStringFromMap initState "occupied_rows"
            
              getIntFromDInt (DInteger i) = if (i < 0 || i > 10) then -1 else i
              getIntFromDInt _ = -1
            
              getListFromDList (DList l) = l
              getListFromDList _ = []
            
              occupiedColsDInts = getListFromDList occupiedColsDList
              occupiedRowsDInts = getListFromDList occupiedRowsDList
            
              getNumberOfHints (DInteger dint) = getIntFromDInt (DInteger dint)
              getNumberOfHints _ = -1
              numberOfHints = getNumberOfHints numberOfHintsDInt
            
              transformList list = map getIntFromDInt list
            
              occupiedCols = transformList occupiedColsDInts
              occupiedRows = transformList occupiedRowsDInts
    fromDocument initState = Left  (show initState)



-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart _ (GameStart numOfHints occCols occRows) = State [] occRows occCols numOfHints

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint { listOfCoords :: [(Int, Int)] } deriving (Show)

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

instance FromDocument Hint where
    --fromDocument :: Document -> Either String GameStart
    --fromDocument doc = Left (show doc)
    fromDocument (DMap h) = extractHints
        where extractHints
                | find (-1,-1) listOfCoords = Left "4"
                | otherwise = Right (Hint listOfCoords)
              hintCoordsDMap = getDocByStringFromMap (DMap h) "coords"

              getDocByStringFromMap (DMap hintList) string = foldl (\acc (s, d) -> if s == string then d else acc) DNull hintList
              getDocByStringFromMap _ _ = DNull
           
              listOfDMaps = parseHintDMap hintCoordsDMap
           
              getListFromDList (DList l) = l
              getListFromDList _ = []
           
              parseHintDMap (DMap dMapToParse) = DList (foldl (parseRec) [] dMapToParse)
              parseHintDMap _ = DNull
           
              parseRec acc ("head", (DMap m)) = acc ++ [DMap m]
              parseRec acc ("tail", DNull) = acc
              parseRec acc ("tail", (DMap m)) = acc ++ (getListFromDList (parseHintDMap (DMap m)))
              parseRec acc (_, _) = acc ++ [DNull]
      
              getListOfCoords (DList dlist) = map extractCoords dlist
              getListOfCoords _ = [(-1, -1)]
              listOfCoords = getListOfCoords listOfDMaps

              extractCoords (DMap dMapOfCoordTuples)
                 | rowDInt == DNull || colDInt == DNull  = (-1, -1)
                 | (getIntFromDInt rowDInt) < 0 || (getIntFromDInt rowDInt) > 9 || (getIntFromDInt colDInt) < 0 || ( getIntFromDInt colDInt > 9) = (-1, -1)
                 | otherwise = ( (getIntFromDInt rowDInt) , (getIntFromDInt colDInt) )
                       where
                             rowDInt = getDocByStringFromMap (DMap dMapOfCoordTuples) "row"
                     
                             colDInt = getDocByStringFromMap (DMap dMapOfCoordTuples) "col"

                             getIntFromDInt (DInteger i) = i
                             getIntFromDInt _ = -1
              extractCoords _ = (-1, -1)
    fromDocument h = Left  (show h)

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State occupied rows cols hints) (Hint listOfCoords) = State (nub (occupied ++ listOfCoords)) rows cols hints
