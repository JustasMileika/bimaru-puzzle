{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument, Parser )
import Lib1 (State(..))
import Data.List(nub)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except (throwE, runExceptT)
import Control.Monad.Trans.State.Strict (get, put, runState)
import Control.Monad.Trans.Class(lift)
import Control.Applicative((<|>))

-- IMPLEMENT
-- Parses a document from yaml

{-
parseDocument :: String -> Either String Document
parseDocument "" = Left "Empty string is invalid"
parseDocument str = case getValue str of
            Left (msg, _) -> Left ("Could not parse from line: " ++ msg)
            Right doc -> Right doc

getValue :: String -> Either (String, Bool) Document
getValue str
     | isRight dint = dint
     | isRight dstring = dstring
     | isRight dnull = dnull
     | isRight dlist = convertToDList dlist
     | isRight dmap = convertToDMap dmap
     | otherwise = Left (
                    if statusDlist == True then (messageDList, statusDlist)
                    else if statusDmap == True then (messageDmap, statusDmap)
                    else (messageDnull, statusDnull))                                                                        
     where
        dint = parseInt str
        dstring = parseString str
        dnull = parseNull str
        dlist = parseList str
        dmap = parseMap str
        (messageDList, statusDlist) = fromLeft dlist
        (messageDmap, statusDmap) = fromLeft dmap
        (messageDnull, statusDnull) = fromLeft dnull


parseString :: String -> Either (String, Bool) Document
parseString str
           | length trimmedPart == 2 && (((head' trimmedPart == Just '[') && (last' trimmedPart == Just ']'))
            || ((head' trimmedPart == Just '{') && (last' trimmedPart == Just '}'))) = Left (firstPart, True)
           | length trimmedPart == 2 && (head' trimmedPart == Just '\'') && (last' trimmedPart == Just '\'') = Right (DString "") 
           | (head' trimmedPart == Just '\"') && (last' trimmedPart == Just '\"') && null textAfterColon = Right (DString (removeLast $ removeFirst trimmedPart))
           | (head' trimmedPart == Just '\'') && (last' trimmedPart == Just '\'') && null textAfterColon = Right (DString (removeLast $ removeFirst trimmedPart))
           | trimmedPart /= trim "null" && isNothing (readMaybe trimmedPart :: Maybe Int)
             && (head' trimmedPart /= Just '-' || (head' trimmedPart == Just '-' && length trimmedPart > 1 && trimmedPart !! 1 /= ' '))
             && last' trimmedPart /= Just ':' && null textAfterColon = Right (DString trimmedPart)
           | otherwise = Left (firstPart, True)     
            where (firstPart, _) = breakOn '\n' str
                  trimmedPart = trim firstPart
                  (_, textAfterColon) = breakOn ':' trimmedPart


parseInt :: String -> Either (String, Bool) Document
parseInt str = case  readMaybe (replaceSpaceWithTrash (trim firstPart))  :: Maybe Int of
    Nothing -> Left (firstPart, True)
    Just num -> Right (DInteger num)
    where (firstPart, _) = breakOn '\n'  str
          replaceSpaceWithTrash = map (\c -> if c==' ' then '@'; else c)


parseNull :: String -> Either (String, Bool) Document
parseNull str =
    let 
        (firstPart, _) = breakOn '\n' str
    in 
        case trim firstPart of 
            "null" -> Right DNull
            _ -> Left (firstPart, True)   



parseMapKey :: String -> Either String (String, String)
parseMapKey str =
    let 
        prefix = takeWhile (':' /=) str
    in
        case trim prefix of
            [] -> Left (fst (breakOn '\n' str))
            _ -> Right (prefix, drop (length prefix) str)

parseColon :: String -> String -> Either String String
parseColon str [] = Left str
parseColon _ (x:xs) 
                  | ':' == x = Right xs
                  | otherwise = Left (x:xs)

parseMap :: String -> Either (String, Bool) [(String, Document)]
parseMap str
       | length (trim firstPart) == 2 && head' (trim firstPart) == Just '{' && last' (trim firstPart) == Just '}' = Right []
       | otherwise = 
         case getMap of
            Left msg -> Left (msg, False)
            Right (key, stringValue) 
              | stringValue /= [] && head'(trim key) /= Just '-' ->
                                 case getValue stringValue of 
                                      Left msg -> Left msg
                                      Right value -> do
                                       a <- Right (key, value)
                                       (a:) <$> continueRec                      
            Right (key, [])                           
              | head' (trim key) /= Just '-' &&
                                 if head' (trim secondPart) == Just '-'
                                 then(countSpacesFront firstPart == countSpacesFront secondPart)
                                 else (countSpacesFront firstPart == countSpacesFront secondPart - 2) ->
                                 case getValue secondPart of 
                                      Left msg -> Left msg
                                      Right value -> do
                                        a <- Right (key, value)
                                        (a:) <$> continueRec
            _ -> Left (firstPart, False)
       where (firstPart, secondPart) = breakOn '\n' str
             rest = findSameLevelMap (str, str)      
             continueRec = if null rest then Right [] else parseMap rest
             getMap = do
                      (key, colonAndValue) <- parseMapKey (fst (breakOn '\n' str))
                      (value) <- parseColon key colonAndValue
                      return (if ((head' (trim key) == Just '\'' && last' (trim key) == Just '\'')
                                           || (head' (trim key) == Just '\"' && last' (trim key) == Just '\"'))
                                           then (removeLast $ removeFirst (trim key)) else (trim key), value)                               



parseList :: String -> Either (String, Bool) [Document]
parseList str
        | length (trim firstPart) == 2 && head' (trim firstPart) == Just '[' && last' (trim firstPart) == Just ']' = Right []
        | length (trim firstPart) > 1 && (trim firstPart) !! 0 == '-' && (trim firstPart) !! 1 /= ' ' = Left (firstPart, False)
        | head' (trim firstPart) == Just '-' =
          if last' firstPart == Just '-'
          then if countSpacesFront firstPart < countSpacesFront secondPart
               then (:) <$>  getValue secondPart <*> if null rest then Right [] else parseList rest
               else Left (firstPart, False)
          else (:) <$>  getValue (replaceFirstDash '-' str) <*> if null rest then Right [] else parseList rest
        | otherwise = Left (firstPart, False)
        where (firstPart, secondPart) = breakOn '\n' str
              rest = findSameLevelList (str, str)
            

removeFirst :: [a] -> [a]
removeFirst myList =
    case myList of
      [] -> []
      _ : xs -> xs

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [_] = []
removeLast (h:t) = h : removeLast t

breakOn :: Char -> String -> (String,String)
breakOn s str = 
    let 
        (b,c) = break (== s) str
    in 
        (b, removeFirst c)

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:_) = Just x


countSpacesFront :: String -> Int
countSpacesFront str = length (takeWhile isSpace str)


convertToDMap :: Either a [(String, Document)] -> Either a Document
convertToDMap (Right doc) = Right (DMap doc)
convertToDMap (Left msg) = Left msg


convertToDList :: Either a [Document] -> Either a Document
convertToDList (Right doc) = Right (DList doc)
convertToDList (Left msg) = Left msg

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

fromLeft :: Either (String, Bool) b -> (String, Bool)
fromLeft (Left a) = a
fromLeft _ = ("", False)


replaceFirstDash :: Char -> [Char] -> [Char]
replaceFirstDash _ [] = [] 
replaceFirstDash a (x:xs) | a == x    = " " ++ xs 
                     | x /= ' '  = x:xs
                     | otherwise = x : replaceFirstDash a xs   
-}

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document 
--parseDocument = fst . runState . ( runExceptT getValue)
parseDocument str  = fst $ runState ( runExceptT getValue) str

parseDocument' :: String -> (Either String Document, String) 
parseDocument' = runState ( runExceptT getValue)

getValue :: Parser Document
getValue =  parseInt <|> parseNull <|> parseString <|> parseList <|> parseMap


parseString :: Parser Document
parseString = do
  str <- lift get
  (firstPart, secondPart) <- splitOn '\n'
  let  trimmedPart = trim firstPart
  lift $ put trimmedPart
  (_, textAfterColon) <- splitOn ':'
  plainString <- parseFirstLastChar '\"' '\"' <|> parseFirstLastChar '\'' '\'' <|> return trimmedPart
  lift $ put str
  case plainString of
    "[]" -> throwE "Not a DString. "
    "{}" -> throwE "Not a DString. "
    "null" -> throwE "Not a DString. "
    '-' : _ -> throwE "Not a DString. "
    plainString'   | null textAfterColon && last' plainString' /= Just ':' && isNothing (readMaybe trimmedPart :: Maybe Int) -> do
                         lift $ put secondPart
                         return $ DString plainString'
    _ -> throwE "Could not parse DString. "

parseInt :: Parser Document
parseInt = do
    str <- lift get
    let (firstPart, secondPart) = breakOn '\n'  str
        replaceSpaceWithTrash = map (\c -> if c==' ' then '@'; else c)
    case  readMaybe (replaceSpaceWithTrash (trim firstPart))  :: Maybe Int of
      Nothing -> throwE "Could not parse DInteger. "
      Just _ -> do
            lift $ put secondPart 
            return $ DInteger $ read firstPart

parseNull :: Parser Document
parseNull = do
    str <- lift get
    let (firstPart, secondPart) = breakOn '\n' str
    case trim firstPart of 
        "null" -> do
                lift $ put secondPart 
                return DNull
        _ -> throwE  "Could not parse DNull. "

parseMap :: Parser Document
parseMap = do
  str <- lift get
  (firstLine, rest) <- splitOn '\n'
  case trim firstLine of
    "{}" -> do
      lift $ put rest
      return $ DMap[]
    _ -> do
         value <- parseMapValue
         key <- parseFirstLastChar '\"' '\"' <|> parseFirstLastChar '\'' '\'' <|> lift get >>= \state -> return state
         lift $ put value
         case value == rest of
          False
            | head' key /= Just '-' -> do
              parsedValue <- getValue
              lift $ put sameLevel          
              continueRec <- if null sameLevel then do
                lift $ put rest             
                return $ DMap [] else parseMap
              return $ DMap $ (key, parsedValue) : unwrapDocumentMap continueRec
          True
            | head' key /= Just '-' && 
              if head' (trim rest) == Just '-'
              then countSpacesFront firstLine == countSpacesFront rest
              else countSpacesFront firstLine == countSpacesFront rest - 2 -> do              
                parsedValue <- getValue
                lift $ put sameLevel                              
                continueRec <- if null sameLevel then do
                  lift $ put rest
                  return $ DMap [] else parseMap       
                return $ DMap $ (key, parsedValue) : unwrapDocumentMap continueRec
          _ -> do
            lift $ put str
            throwE "Could not parse DMap. "
          where           
              unwrapDocumentMap (DMap xs) = xs
              sameLevel = findSameLevelMap (str, str)

parseList :: Parser Document
parseList = do
  str <- lift get
  (firstPart, secondPart) <- splitOn '\n'
  let rest = findSameLevelList (str, str)
      trimmedFirstPart = trim firstPart
      unwrapDocumentList (DList xs) = xs
  case trimmedFirstPart of
    [] -> do
      lift $ put str
      throwE "Empty is not a DList. "
    "[]" -> do
         lift $ put secondPart
         return $ DList []
    '-' : value
        | head' value /= Just ' ' && value /= [] -> do
          lift $ put str
          throwE "Negative integer is not a DList"
        | null value && countSpacesFront firstPart < countSpacesFront secondPart -> do
             lift $ put rest
             parsedRest <- if null rest then lift $ put secondPart >> return (DList []) else parseList
             lift $ put secondPart
             parsedValue <- getValue
             return $ DList $ parsedValue : unwrapDocumentList parsedRest    
        | otherwise -> do
             lift $ put rest
             parsedRest <- if null rest then lift $ put secondPart >> return (DList []) else parseList
             lift $ put (replaceFirstDash '-' str)
             parsedValue <- getValue
             return $ DList $ parsedValue : unwrapDocumentList parsedRest
    _ -> do lift $ put str
            throwE "Could not parse DList. "          


parseFirstLastChar :: Char -> Char -> Parser String
parseFirstLastChar ch1 ch2 = do
  initialState <- lift get
  _ <- parseChar ch1
  str <- parseUntil ch2
  _ <- parseChar ch2
  lift $ put initialState
  return str

parseUntil :: Char -> Parser String
parseUntil ch = do
    str <- lift get
    let prefix = takeWhile (/= ch) str
    lift $ put $ drop (length prefix) str
    return prefix


parseChar :: Char -> Parser Char
parseChar ch = do
    str <- lift get
    case str of
        [] -> throwE $ "Empty input: '" ++ [ch] ++ "' expected"
        (x:xs) | ch == x -> lift $ put xs >> return x
               | otherwise -> throwE $ ch :" expected"

splitOn :: Char -> Parser (String, String)
splitOn ch = do
  str <- lift get
  let (a, b) = break (== ch) str
  return  (a, removeFirst b)


parseMapValue :: Parser String
parseMapValue = do
  str <- lift get
  let key = takeWhile (':' /=) str
  (prefix, rest) <- splitOn '\n'
  if length key < length prefix && key /= [] then
     do lift $ put $ trim key
        return $ if length (trim prefix) - length (trim key) > 1
                 then drop (length key + 1 ) str
                 else rest
  else
     throwE "Could not parse DMap value"        

removeFirst :: [a] -> [a]
removeFirst myList =
    case myList of
      [] -> []
      _ : xs -> xs

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [_] = []
removeLast (h:t) = h : removeLast t

breakOn :: Char -> String -> (String,String)
breakOn s str = 
    let 
        (b,c) = break (== s) str
    in 
        (b, removeFirst c)

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:_) = Just x


countSpacesFront :: String -> Int
countSpacesFront str = length (takeWhile isSpace str)


convertToDMap :: Either a [(String, Document)] -> Either a Document
convertToDMap (Right doc) = Right (DMap doc)
convertToDMap (Left msg) = Left msg


convertToDList :: Either a [Document] -> Either a Document
convertToDList (Right doc) = Right (DList doc)
convertToDList (Left msg) = Left msg

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

fromLeft :: Either (String, Bool) b -> (String, Bool)
fromLeft (Left a) = a
fromLeft _ = ("", False)


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
                | numOfHints == -1 = Left "Invalid document"
                | (length occupCols /= 10) || (find (-1) occupCols) = Left "Invalid document"
                | (length occupRows /= 10) || (find (-1) occupRows) = Left "Invalid document"
                | otherwise = Right  (GameStart numOfHints occupCols occupRows)
            
              getDocByStringFromMap initStateList string = foldl (\acc (s, d) -> if s == string then d else acc) DNull initStateList 
            
              numberOfHintsDInt = getDocByStringFromMap initState "number_of_hints"
              occupiedColsDList = getDocByStringFromMap initState "occupied_cols"
              occupiedRowsDList = getDocByStringFromMap initState "occupied_rows"
            
              getIntFromDInt (DInteger i) = if (i < 0 || i > 10) then -1 else i
              getIntFromDInt _ = -1
            
              getListFromDList (DList l) = l
              getListFromDList _ = []
            
              occupColsDInts = getListFromDList occupiedColsDList
              occupRowsDInts = getListFromDList occupiedRowsDList
            
              getNumberOfHints (DInteger dint) = getIntFromDInt (DInteger dint)
              getNumberOfHints _ = -1
              numOfHints = getNumberOfHints numberOfHintsDInt
            
              transformList list = map getIntFromDInt list
            
              occupCols = transformList occupColsDInts
              occupRows = transformList occupRowsDInts
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
                | find (-1,-1) listOfCoordinates = Left "Invalid document"
                | otherwise = Right (Hint listOfCoordinates)
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
              listOfCoordinates = getListOfCoords listOfDMaps

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
hint (State occ r c h) (Hint hints) = State (nub (occ ++ fmap addOne hints)) r c h
    where addOne (a, b) = (a+1, b+1)


