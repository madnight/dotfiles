#!/usr/bin/env stack
-- stack --install-ghc runghc wreq

import Control.Lens
import Data.List
import Network.Wreq
import System.Exit
import System.Process

processIsRunning :: String -> IO Bool
processIsRunning = (<$> readProcess "ps" ["x"] []) . isInfixOf

startConky ::  String -> IO ExitCode
startConky = system . ("conky -c ~/.config/conky/" ++)

conky :: String -> IO ()
conky name = do
     exit <- processIsRunning name
     case exit of
       True  -> putStrLn ("conky " ++ name ++ " already running")
       False -> print =<< startConky (name ++  " &")

hasInternet :: IO Bool
hasInternet = do
    response <- get "https://google.com"
    return $ (==) 200 (response ^. responseStatus . statusCode)

sidebar :: IO ()
sidebar = do
    hostname <- readProcess "hostname" [] []
    case hostname of
      "arch\n" -> conky "conkyrc"
      "skylake\n" -> conky "conkyrc-work"
      _ -> putStrLn "unkown host"

main :: IO ()
main = do
    sidebar
    {- online <- hasInternet -}
    {- if online then mapM_ conky ["weather", "errors"] else main -}
