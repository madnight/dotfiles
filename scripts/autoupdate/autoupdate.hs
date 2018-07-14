#!/usr/bin/env stack
{-
stack --install-ghc runghc
      --package concurrent-extra
      --package monad-extras
      --package lens
      --package wreq
      --package timerep
      --package xml
-}

-- compile
-- nix-shell --run 'ghc autoupdate.hs'

module Main where

import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import RSSreader
import System.Exit
import System.Process

checkUpdates :: IO Bool
checkUpdates = not . null <$> readProcess "checkupdates" [] []

systemUpdate :: IO ExitCode
systemUpdate = system "pacman -Syu --noconfirm"

kernelDownload :: IO ExitCode
kernelDownload = system "pacman -Sw --noconfirm linux linux-headers"

removeCache :: IO ExitCode
removeCache = system "paccache -r -k 0"

checkOrphans :: IO Bool
checkOrphans = not . null <$> readProcess "pkg-list_true_orphans" [] []

deleteOrphans :: IO ExitCode
deleteOrphans = system "pacman -Rns --noconfirm $(pkg-list_true_orphans)"

minutes :: Integer -> Integer
minutes = (*) $ product [60, 1000, 1000]

main :: IO ()
main = do
    print "checking upstream feed ..."
    feed <- getFeed "https://www.archlinux.org/feeds/news/" 8
    let keywords = ["inter", "requi", "manual"]
    let warning = flip filterItemsbyTitle feed =<< keywords
    unless (null warning) $ do
        print "Warning! Found update that requires user intervention"
        delay (minutes 30)
        main
    print "updating ..."
    updates <- checkUpdates
    orphrans <- checkOrphans
    when updates $ sequence_ [systemUpdate, kernelDownload]
    when orphrans $ sequence_ [deleteOrphans, removeCache]
    delay $ minutes 30 -- sleep 30 mins
    main
