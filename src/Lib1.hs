{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.List(nub)
import Data.Char(isDigit, digitToInt)

{- solved puzzle: list of coordinates (row, column)-}
          
{-solved1 = [(2, 1), (2, 10), (3, 3), (3, 4), (3, 5), (3, 7), (4, 7), (5, 3), (5, 4), (5, 5), (5, 7), (6, 7), (6, 10), (7, 10), (9, 8), (9, 10), (8, 2), (8, 3), (9, 5), (9, 6) ] -}
{-solved2 = [(2, 1), (2, 10), (3, 3), (3, 4), (3, 5), (3, 7), (4, 7), (5, 3), (5, 4), (5, 5), (5, 7), (6, 7), (6, 10), (7, 10), (9, 8), (9, 10), (8, 5), (8, 6), (9, 2), (9, 3) ] -}

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
     occupied :: [(Int, Int)],
     rows :: [Int],
     columns :: [Int],
     hintAmount :: Int
}
    deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] [] 0

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart _ initState = State [] occupiedRows occupiedCols numberOfHints
    where getDocByStringFromMap (DMap initStateList) string = foldl (\acc (s, d) -> if s == string then d else acc) DNull initStateList 
          
          numberOfHintsDInt = getDocByStringFromMap initState "number_of_hints"
          occupiedColsDList = getDocByStringFromMap initState "occupied_cols"
          occupiedRowsDList = getDocByStringFromMap initState "occupied_rows"
          
          getIntFromDInt (DInteger i) = i
          getListFromDList (DList l) = l
          
          occupiedColsDInts = getListFromDList occupiedColsDList
          occupiedRowsDInts = getListFromDList occupiedRowsDList
          
          numberOfHints = getIntFromDInt numberOfHintsDInt
          occupiedCols = map getIntFromDInt occupiedColsDInts
          occupiedRows = map getIntFromDInt occupiedRowsDInts

-- IMPLEMENT
-- renders your game board
render :: State -> String

render (State o r c _) = firstRow ++ "\n" ++ gridWithShips
       where 
             firstRow = "\n" ++ "  " ++ (map (intToDigit) c)
             
             intToDigit num = (show num) !! 0
             
             emptyGrid = ( concat (zipWith (\x y -> x ++ " " ++ y) (map show r) (replicate 10 ((replicate 10 'O') ++ "\n")))) ++ "\n"
             
             gridWithShips = foldl (\acc x -> addShip acc x) emptyGrid o
             addShip accum coor = (firstPartOfArray accum (indexToSplit coor)) ++ "x" ++ (tail (secondPartOfArray accum (indexToSplit coor)))
             
             firstPartOfArray gridToSplit indexToSplitAt = fst (splitAt indexToSplitAt gridToSplit)
             secondPartOfArray gridToSplit indexToSplitAt = snd (splitAt indexToSplitAt gridToSplit)
             
             indexToSplit (x, y) = (x-1) * 11 + x * 2 + (y - 1)

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State o _ _ _) = Check coordin
        where coordin = map func o
              func (x, y) = Coord (y-1) (x-1)

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State o r c hl) t = State withToggle r c hl
        where coordPair = if length t /= 2 then [] else (if rowCoord /= 0 && colCoord /= 0 then [(rowCoord, colCoord)] else [])
                  where getIntFromString str
                            | length str == 1 && isDigit (str !! 0) && str /= "0" = digitToInt (str !! 0)
                            | str == "10" = 10
                            | otherwise = 0
                        rowCoord = getIntFromString (t !! 0)
                        colCoord = getIntFromString (t !! 1)
              
              withToggle :: [(Int, Int)]
              withToggle = if coordPair == [] then o else (if (coordPair !! 0) `elem` o then removeItem (coordPair !! 0) o else o ++ coordPair)
              
              removeItem _ []                 = []
              removeItem x (y:ys) | x == y    = ys
                                  | otherwise = y : removeItem x ys

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State o r c hl) (DMap h) = State withHints r c hl
      where hintCoordsDMap = getDocByStringFromMap (DMap h) "coords"

            getDocByStringFromMap (DMap hintList) string = foldl (\acc (s, d) -> if s == string then d else acc) DNull hintList 
            
            listOfDMaps = parseHintDMap hintCoordsDMap
            
            parseHintDMap (DMap dMapToParse) = foldl (parseRec) [] dMapToParse
            parseRec acc ("head", (DMap m)) = acc ++ [DMap m]
            parseRec acc ("tail", DNull) = acc
            parseRec acc (_, (DMap m)) = acc ++ (parseHintDMap (DMap m))
            
            listOfCoords = map extractCoords listOfDMaps

            extractCoords dMapOfCoordTuples = (rowCoord, colCoord)
                          where rowCoord = getIntFromDInt rowDInt + 1
                                rowDInt = getDocByStringFromMap dMapOfCoordTuples "row"
                                
                                colCoord = getIntFromDInt colDInt + 1
                                colDInt = getDocByStringFromMap dMapOfCoordTuples "col"
                                
                                getIntFromDInt (DInteger i) = i
                                
            withHints = nub (o ++ listOfCoords)