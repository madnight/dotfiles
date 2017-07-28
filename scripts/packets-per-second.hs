#!/usr/bin/env stack
-- stack --install-ghc runghc concurrent-extra strict

import System.Environment
import System.Directory
import System.Exit
import System.IO.Error
import System.IO.Strict as Strict
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Exception.Base
import Control.Monad.IO.Class
import Control.Monad.Trans
import Text.Read (readMaybe)

exampleUsage = "\nexample usage: packets-per-second enp5s0 OUT\npackets-per-second eth0 IN"
packetsOSPath i d  = "/sys/class/net/" ++ i ++ "/statistics/"++ d ++ "x_packets"

error' :: String -> a
error' = errorWithoutStackTrace . (++ exampleUsage)

packetsPerSecond :: String -> String -> IO ()
packetsPerSecond i d  = do
    let readStrictSafe = Strict.readFile (packetsOSPath i d) `catch` exceptionHandler
    packetsT0 <- readStrictSafe
    delay 1000000 -- sleep one second
    packetsT1 <- readStrictSafe
    let [x, y] = (\x -> readMaybe x :: Maybe Integer) <$> [packetsT0, packetsT1]
    print (fromJust y - fromJust x)
        where -- error handling
            exceptionHandler e
              | isDoesNotExistError e = error' "wrong interface name"
              | isPermissionError e = error' "cannot access network interface packets information: permission denied"
            fromJust (Just x) = x
            fromJust Nothing  = errorWithoutStackTrace "interface packet information malformed or non existing"

direction :: String -> String -> IO ()
direction i d = case d of
      "IN" -> packetsPerSecond i "r"
      "OUT" -> packetsPerSecond i "t"
      _ -> error' "wrong direction argument! "

main :: IO ()
main = do
    args <- getArgs
    case args of
        [interface, dir] -> direction interface dir
        [] -> error' "too few arguments"
        [_] -> error' "too few arguments"
        _ -> error' "too many arguments"
