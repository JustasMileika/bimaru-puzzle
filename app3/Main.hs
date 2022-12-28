{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, StateT )
import Data.ByteString as B ( empty, ByteString )
import qualified Data.List as L
import Data.Text as T ( concat, pack, unpack, Text )
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Lib1 ( render )

import Lib2 ( renderDocument )
import Lib3 ( parseDocument )
import Types(Document(..), fromDocument)
import Network.Wreq
    ( post, postWith, defaults, header, responseBody )

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT String IO) a

commandShow :: String
commandShow = "show"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = do
    docStr <- lift get >>= fetchBoard
    let state = parseDocument docStr >>= (\d -> fromDocument d)
    case state of
      Left e -> liftIO $ fatal $ cs e
      Right st -> liftIO $ Prelude.putStrLn $ Lib1.render $ st
        
  | trim c == commandCheck = lift get >>= check >>= liftIO . Prelude.putStrLn
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at leas one argument"
      t -> do
        url <- lift get
        let toggleIndexes = getToggleIndexes t
        case toggleIndexes of
          Left e -> liftIO $ fatal $ cs e
          Right (row, col) -> do
            let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
            let body = cs $ renderDocument $ DMap [("col", DInteger $ col - 1), ("row", DInteger $ row - 1)] :: B.ByteString
            _ <- liftIO $ postWith opts (url ++ "/toggle") body
            return ()
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

getToggleIndexes :: [String] -> Either String (Int, Int)
getToggleIndexes (t1 : t2 : []) = do
  row <- strToInt t1
  col <- strToInt t2
  return (row, col)
getToggleIndexes _ = Left "invalid toggle indexes"

strToInt :: String -> Either String Int
strToInt str = do
  let prefix = takeWhile isDigit str
  case prefix of
    [] -> Left "Empty integer"
    _ -> return $ read prefix

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace
  
fetchBoard :: String -> Repl String
fetchBoard url = do
  resp <- liftIO $ post (url ++ "/show") B.empty
  pure $ cs $ resp ^. responseBody

check :: String -> Repl String
check url = do
  resp <- liftIO $ post (url ++ "/check") B.empty
  let maybeDoc = parseDocument $ cs $  resp ^. responseBody
  case maybeDoc of
    Left e -> return e
    Right doc -> pure $ renderDocument doc

completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandCheck, commandToggle]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  url <- lift get
  _ <- liftIO $ post url B.empty
  liftIO $ TIO.putStrLn "Welcome to Bimaru v3. Press [TAB] for available commands list"

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> fatal "token not provided, expected at least one command argument"

run :: T.Text -> IO ()
run token = do
  let url = "localhost:8080"
  let fullUrl = T.unpack (T.concat ["http://", url, "/game/", token])
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) fullUrl
