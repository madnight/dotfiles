#!/usr/bin/env stack
-- stack --install-ghc runghc concurrent-extra strict

module Main where

import RSSreader
import System.Process
import Control.Concurrent.Thread.Delay
import System.Exit
import Data.List
import Control.Monad.IO.Class
import Control.Monad

checkUpdates :: IO Bool
checkUpdates = not . null <$> readProcess "checkupdates" mempty mempty

systemUpdate :: IO ExitCode
systemUpdate = system "pacman -Syu --noconfirm"

kernelDownload :: IO ExitCode
kernelDownload = system "pacman -Sw --noconfirm linux linux-headers"

removeCache :: IO ExitCode
removeCache = system "paccache -r -k 1"

checkOrphans :: IO Bool
checkOrphans = not . null <$> readProcess "pkg-list_true_orphans" mempty mempty

deleteOrphans :: IO ExitCode
deleteOrphans = system "pacman -Rns --noconfirm $(pkg-list_true_orphans)"

minToMicroseconds :: Integer -> Integer
minToMicroseconds = (*) $ product [60, 1000, 1000]

main = do
    {- Get News from Archlinux.org -}
    feed <- getFeed "https://www.archlinux.org/feeds/news/" 8
    {- inter := interaction, intervention; requi := requires, required -}
    let keywords = ["inter", "requi", "manual"]
    let warning = flip filterItemsbyTitle feed =<< keywords
    unless (null warning) main
    updates <- checkUpdates
    orphrans <- checkOrphans
    when updates $ sequence_ [systemUpdate, kernelDownload]
    when orphrans $ sequence_ [deleteOrphans, removeCache]
    delay $ minToMicroseconds 30 -- sleep 30 mins
    main

