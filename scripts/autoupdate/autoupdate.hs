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

checkUpdates = null <$> readProcess "checkupdates" mempty mempty
systemUpdate = system "pacman -Syu --noconfirm"

kernelDownload = system "pacman -Sw linux linux-headers"
removeCache = system "paccache -r -k 1"

checkOrphans = null <$> readProcess "pkg-list_true_orphans" mempty mempty
deleteOrphans = system "pacman -Rns --noconfirm $(pkg-list_true_orphans)"

mins = (*) (60 * 1000000)

main = do
    {- delay (mins 30) -- sleep 30 mins -}
    {- Get News from Archlinux.org -}
    feed <- getFeed "https://www.archlinux.org/feeds/news/" 8
    {- inter := interaction, intervention; requi := requires, required -}
    let keywords = ["inter", "requi", "manual"]
    let warning = flip filterItemsbyTitle feed =<< keywords
    unless (null warning) main
    updates <- checkUpdates
    orphrans <- checkOrphans
    when updates $ mapM_ id [systemUpdate, kernelDownload]
    when orphrans $ mapM_ id [deleteOrphans, removeCache]
    main

