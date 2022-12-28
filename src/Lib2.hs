{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types (Document(..))
import Lib1 (State(..))
import Data.List(nub)

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = convert' doc 0 True


convert' :: Document -> Int -> Bool -> String

convert' (DInteger intas) _ _ = show intas


convert' (DNull) _ _ = "null"
convert' (DString "") _ _ = "''"
convert' (DList []) _ _ = "[]"
convert' (DMap []) _ _ = "{}"
convert' (DString str) _ _ = show str

convert' (DMap dmap) indent needToIndent = (foldl mapHelper' "" dmap) 
    where mapHelper' acc (key, (DMap [])) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ (if key == "" then "''" else key) ++ ": " ++ "{}\n"
          mapHelper' acc (key, (DMap mapas)) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ (if key == "" then "''" else key) ++ ":\n" ++ (convert' (DMap mapas) (indent + 2) True) 
          mapHelper' acc (key, (DList [])) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ (if key == "" then "''" else key) ++ ": " ++ "[]\n"
          mapHelper' acc (key, (DList list)) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ (if key == "" then "''" else key) ++ ":\n" ++ (convert' (DList list) (indent) True)
          mapHelper' acc (key, (value)) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ (if key == "" then "''" else key) ++ ": " ++ (convert' value indent True) ++ "\n"
          
          
convert' (DList dlist) indent needToIndent = (foldl listHelper' "" dlist)
    where listHelper' acc (DMap []) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ "- " ++ "{}\n"
          listHelper' acc (DMap mapas) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ "- " ++ (convert' (DMap ([(head mapas)])) (indent + 2) False) ++ (if length mapas == 1 then "" else convert' (DMap (tail mapas)) (indent + 2) True)
          listHelper' acc (DList []) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ "- " ++ "[]\n"
          listHelper' acc (DList list) = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ "-\n" ++ (convert' (DList list) (indent + 2) True)
          listHelper' acc doc = acc ++ (if needToIndent then (replicate indent ' ') else "") ++ "- " ++ (convert' doc indent True) ++ "\n"

find :: Eq t => t -> [t] -> Bool
find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs


-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart _ (DMap initState) = extractInitState
    where extractInitState
            | numberOfHints == -1 = Left "Invalid input"
            | (length occupiedCols /= 10) || (find (-1) occupiedCols) = Left "Invalid input"
            | (length occupiedRows /= 10) || (find (-1) occupiedRows) = Left "Invalid input"
            | otherwise = Right  (State [] occupiedRows occupiedCols numberOfHints)
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
 
gameStart _ _ = Left "Invalid input"

-- IMPLEMENThl
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State o r c hl) (DMap h) = extractHints
    where extractHints
            | find (-1,-1) listOfCoords = Left "Invalid input"
            | otherwise = Right (State (nub (o ++ listOfCoords)) r c hl)
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
                         
hint _ _ = Left "Invalid input"

