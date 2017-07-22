#!/usr/bin/env stack
-- stack --install-ghc runghc concurrent-extra strict

import System.Environment
import System.IO.Strict as Strict
import Control.Concurrent.Thread.Delay
import System.Directory
import Control.Monad

exampleUsage = "\nexample usage: packets-per-second enp5s0 OUT\npackets-per-second eth0 IN"
packetsOSPath i d  = "/sys/class/net/" ++ i ++ "/statistics/"++ d ++ "x_packets"

error' :: String -> IO ()
error' = error . (++ exampleUsage)

packetsPerSecond :: String -> String -> IO ()
packetsPerSecond i d  = do
    fileExists <- doesFileExist $ packetsOSPath i d
    if not fileExists then
        error' "wrong interface name"
    else do
        packetsT0 <- Strict.readFile $ packetsOSPath i d
        delay 1000000 -- sleep one second
        packetsT1 <- Strict.readFile $ packetsOSPath i d
        let p0 = read packetsT0 :: Integer
        let p1 = read packetsT1 :: Integer
        print (p1 - p0)

direction :: String -> String -> IO ()
direction i d = case d of
      "IN" -> packetsPerSecond i "r"
      "OUT" -> packetsPerSecond i "t"
      _ -> error' "wrong direction argument! "

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error' "too few arguments"
        [_] -> error' "too few arguments"
        [interface, dir] -> direction interface dir
        _ -> error' "too many arguments"
