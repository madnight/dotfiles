#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package http-conduit
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Network.Wreq
import qualified Data.Text as T
import Control.Lens

getStatus :: Response body -> Int
getStatus = (^. (responseStatus . statusCode))

hasInternet :: Int -> Bool
hasInternet = (==) 200

processIsRunning :: String -> IO ExitCode
processIsRunning = flip shell empty . T.pack . ("ps x | /usr/bin/grep " ++)

startConky ::  String -> IO ExitCode
startConky = flip shell empty . T.pack . ("conky -c ~/.config/conky/" ++)

main :: IO ()
main = do
    r <- get "https://google.com"
    let online = hasInternet $ getStatus r
    if online then do
        exit <- processIsRunning "[w]eather"
        case exit of
            ExitSuccess   -> print "conky weather already running"
            ExitFailure n -> print =<< startConky "weather &"
        exit <- processIsRunning "[e]rrors"
        case exit of
            ExitSuccess   -> print "conky error already running"
            ExitFailure n -> print =<< startConky "errors &"
     else
        main
