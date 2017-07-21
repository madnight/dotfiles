#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package http-conduit
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Network.Wreq
import qualified Data.Text as T
import Control.Lens
import Data.Text.ICU.Replace

processIsRunning :: String -> IO ExitCode
processIsRunning = flip shell empty . T.pack . ("ps x | /usr/bin/grep " ++) . grepFormat
            where grepFormat = T.unpack . replaceAll "^." "[$0]" . T.pack

startConky ::  String -> IO ExitCode
startConky = flip shell empty . T.pack . ("conky -c ~/.config/conky/" ++)

conky :: String -> IO ()
conky s = do
     exit <- processIsRunning s
     case exit of
         ExitSuccess   -> print ("conky " ++ s ++ " already running")
         ExitFailure n -> print =<< startConky (s ++  " &")

hasInternet :: IO Bool
hasInternet = do
    r <- get "https://google.com"
    return $ (==) 200 (r ^. responseStatus . statusCode)

main :: IO ()
main = do
    online <- hasInternet
    if online then mapM_ conky ["weather", "errors"] else main

