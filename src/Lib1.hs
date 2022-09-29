{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.List

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
     occupied :: [(Int, Int)],
     rows :: [Int],
     columns :: [Int],
     hintsLeft :: Int
}
    deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] [] 0

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State o r c hl) (DMap x) = State [(2, 5), (9, 9), (4, 6), (10, 10), (7, 1)] b a (c c')
    where c' = snd (x !! 0)
          c (DInteger x) = x
          te = show c'
          a'' = snd (x !! 1)
          a' (DList list) = list
          a = map (f) (a' a'')
          f (DInteger intas) = intas
          b'' = snd (x !! 2)
          b = map (f) (a' b'')
          



-- IMPLEMENT
-- renders your game board
render :: State -> String


render (State o r c hl) = res (State o r c hl) ++ "\n" ++ grid'
       where res s= firstRow ++ "\n"
             firstRow = "  " ++ (map (intToDigit) c)
             intToDigit num = (show num) !! 0
             grid = ( concat (zipWith (\x y -> x ++ " " ++ y) (map show c) (replicate 10 ((replicate 10 'O') ++ "\n")))) ++ "\n"
             grid' = foldl (\acc x -> addShip acc x) grid o
             addShip accum coor = (first accum (indexToSplit coor)) ++ "." ++ (tail (second accum (indexToSplit coor)))
             first gridToSplit indexToSplitAt = fst (splitAt indexToSplitAt gridToSplit)
             second gridToSplit indexToSplitAt = snd (splitAt indexToSplitAt gridToSplit)
             indexToSplit (x, y) = (x-1) * 11 + x * 2 + (y - 1)

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State o r c hl) t = emptyState

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State o r c hl) (DMap h) = State withHints r c hl
      where h' = snd (h !! 0)
            h'' (DMap x) = x
            h''' = h'' h'
            {-c listas = foldl (\acc (s, (DMap m)) -> if s == "head" then (\x y -> x ++ [y]) 
            else (\x (DMap y) -> x ++ (c y))) [] listas-}
            
            
            cc listas = foldl (func) [] listas
            func acc ("head", (DMap m)) = acc ++ [DMap m]
            func acc ("tail", DNull) = acc ++ []
            func acc (s, (DMap m)) = acc ++ (cc m)
            
            temp = cc h'''
            
            lists = map extract temp
            extract (DMap m) = m
            
            coords = map getCoords lists
            getCoords [(col, DInteger c), (row, DInteger r)] = (r, c)
            
            
            guesses = o
            
            
            
            withHints = nub (guesses ++ coords)
            