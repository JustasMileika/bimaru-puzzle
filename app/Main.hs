{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock
import           Web.Spock.Config

import Data.ByteString.UTF8 as BSU
import Control.Monad.Except
import qualified Data.List as L
import Data.IORef
import Lib2 ( renderDocument )
import Lib3 ( parseDocument )
import Types( Document(..), ToDocument, toDocument )

import           Data.Text        (pack)


type Api = SpockM () () ServerState ()

type ApiAction a = SpockAction () () ServerState a

newtype ServerState = ServerState { games :: IORef [Game] }

data Game = Game { token :: String, occupied :: [(Int, Int)] } deriving (Show)

instance ToDocument Game where
  toDocument (Game token' occ) = DMap dmap
    where dmap = [("token", DString token'), ("occupied", DList (L.map toDoc occ)), ("cols", DList occupiedCols), ("rows", DList occupiedRows)]
          toDoc (col, row) = DMap [("col", DInteger col), ("row", DInteger row)]

main :: IO ()
main = do
  state <- ServerState <$> newIORef []
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  post ("game" <//> var <//> "show" ) $ \token' -> do
    games' <- getState >>= (liftIO . readIORef . games)
    let currGame = getGame' games' token'
    case currGame of
      Left e -> do
        --setStatus $ Status 400 "Bad Request"
        text $ pack $ renderDocument $ DString e
      Right game -> text $ pack $ renderDocument $ toDocument game
      
  post ("game" <//> var <//> "check") $ \token' -> do
    games' <- getState >>= (liftIO . readIORef . games)
    let currGame = getGame' games' token' 
    
    case currGame of
      Left e -> do
        --setStatus $ Status 400 "Bad Request"
        text $ pack $ renderDocument $ DString e
      Right game -> do
        if (L.sort (occupied game) == L.sort secret1) || (L.sort (occupied game) == L.sort secret2) then
          text $ pack $ renderDocument $ DString "Well done!" else
            text $ pack $ renderDocument $ DString "Try again..."
          
    
  post ("game" <//> var) $ \token' -> do
    startGame $ token'
    text $ pack $ renderDocument $ DString token'
    
  post ("game" <//> var <//> "toggle") $ \token' -> do
    b <- body
    games' <- getState >>= (liftIO . readIORef . games)
    let game = getGame' games' token' >>= (\game' -> toggleGame game' (BSU.toString b))
    case game of
      Left e -> do
        --setStatus $ Status 400 "Bad Request"
        text $ pack $ renderDocument $ DString e
      Right l -> do
        gamesRef <- games <$> getState
        liftIO $ atomicModifyIORef' gamesRef $ \gameList ->
          ((Game token' l) : (filter (\(Game token'' _) -> token'' /= token') gameList), ())
        text $ pack $ renderDocument $ DString token'
    
    
startGame :: String -> ApiAction ()
startGame tokenStr = do
  games' <- getState >>= (liftIO . readIORef . games) :: ApiAction [Game]
  let game = getGame' games' tokenStr
  case game of
    Left _ -> do
      gamesRef <- games <$> getState
      liftIO $ atomicModifyIORef' gamesRef $ \gameList ->
        ((Game tokenStr []) : gameList, ())
    _ -> return ()
    
getGame' :: [Game] -> String -> Either String Game
getGame' gameList tokenStr = do
  let currGame = filter ( \(Game token' _) -> token' == tokenStr ) gameList
  case currGame of
    [] -> Left "no game found"
    [game] -> Right game
    _ -> Left "invalid server state"
    
  
toggleGame :: Game -> String -> Either String [(Int, Int)]
toggleGame game str = do
  res <- parseDocument str
  tokens <- getToggleTokens res
  let toggledGame = addGameToggle game tokens
  return toggledGame


addGameToggle :: Game -> (Int, Int) -> [(Int, Int)]
addGameToggle game (col, row) = updatedGame
  where updatedGame = if (col, row) `elem` (occupied game) then L.delete (col, row) (occupied game) else (col, row) : (occupied game)
  


getToggleTokens :: Document -> Either String (Int, Int)
getToggleTokens (DMap dmap) = do
  col <- getValByKey "col" dmap
  row <- getValByKey "row" dmap
  colInt <- dintToInt col
  rowInt <- dintToInt row
  if colInt < 0 || rowInt < 0 || colInt > 9 || rowInt > 9 then
    Left "invalid indexes" else return (colInt, rowInt)
getToggleTokens _ = Left "Dmap needed"


dintToInt :: Document -> Either String Int
dintToInt (DInteger a) = return a
dintToInt _ = Left "dinteger needed"

getValByKey :: String -> [(String, Document)] -> Either String Document
getValByKey str docList = do
  let res = Prelude.foldl (\acc (str', doc) -> if str' == str then Just doc else acc) Nothing docList
  case res of
    Nothing -> Left $ "could not find " ++ str
    Just d -> Right d
  
occupiedRows :: [Document]
occupiedRows = [DInteger 0, DInteger 2, DInteger 4, DInteger 1, DInteger 4, DInteger 2, DInteger 1, DInteger 2, DInteger 4, DInteger 0]
occupiedCols :: [Document]
occupiedCols = [DInteger 1, DInteger 1, DInteger 3, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 1, DInteger 0, DInteger 4]

secret1 :: [(Int, Int)]
--secret1 = [(2, 1), (2, 10), (3, 3), (3, 4), (3, 5), (3, 7), (4, 7), (5, 3), (5, 4), (5, 5), (5, 7), (6, 7), (6, 10), (7, 10), (9, 8), (9, 10), (8, 2), (8, 3), (9, 5), (9, 6) ]
secret1 = [(0, 1), (9, 1), (2, 2), (3, 2), (4, 2), (6, 2), (6, 3), (2, 4), (3, 4), (4, 4), (6, 4), (6, 5), (9, 5), (9, 6), (7, 8), (9, 8), (1, 7), (2, 7), (4, 8), (5, 8) ]

secret2 :: [(Int, Int)]
--secret2 = [(2, 1), (2, 10), (3, 3), (3, 4), (3, 5), (3, 7), (4, 7), (5, 3), (5, 4), (5, 5), (5, 7), (6, 7), (6, 10), (7, 10), (9, 8), (9, 10), (8, 5), (8, 6), (9, 2), (9, 3) ]
secret2 = [(0, 1), (9, 1), (2, 2), (3, 2), (4, 2), (6, 2), (6, 3), (2, 4), (3, 4), (4, 4), (6, 4), (6, 5), (9, 5), (9, 6), (7, 8), (9, 8), (4, 7), (5, 7), (1, 8), (2, 8) ]