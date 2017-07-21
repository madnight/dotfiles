#!/usr/bin/env stack
-- stack --install-ghc runghc turtle wreq http-conduit string-conversions

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Turtle
import Network.Wreq
import Control.Lens
import Data.Text.ICU.Replace
import System.Process (readProcess)
import Data.String.Conversions (cs)

processIsRunning :: String -> IO ExitCode
processIsRunning = flip shell empty . cs . ("ps x | /usr/bin/grep " ++) . grepFormat
    where grepFormat = cs . replaceAll "^." "[$0]" . cs

startConky ::  String -> IO ExitCode
startConky = flip shell empty . cs . ("conky -c ~/.config/conky/" ++)

conky :: String -> IO ()
conky name = do
     exit <- processIsRunning name
     case exit of
         ExitSuccess   -> print ("conky " ++ name ++ " already running")
         ExitFailure _ -> print =<< startConky (name ++  " &")

hasInternet :: IO Bool
hasInternet = do
    response <- get "https://google.com"
    return $ (==) 200 (response ^. responseStatus . statusCode)

sidebar :: IO ()
sidebar = do
    hostname <- readProcess "hostname" [] ""
    case hostname of
      "arch\n" -> conky "conkyrc"
      "skylake\n" -> conky "conkyrc-work"
      _ -> echo "unkown host"

main :: IO ()
main = do
    sidebar
    online <- hasInternet
    if online then mapM_ conky ["weather", "errors"] else main
