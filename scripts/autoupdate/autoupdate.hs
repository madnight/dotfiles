module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Exit
import System.Process

checkUpdates = not . null <$> readProcess "checkupdates" [] []
systemUpdate = system "pacman -Syu --noconfirm"
kernelDownload = system "pacman -Sw --noconfirm linux linux-headers linux-lts \
                        \ linux-lts-headers"
removeCache = system "paccache -r -k 0"
checkOrphans = not . null <$> readProcess "pkg-list_true_orphans" [] []
deleteOrphans = system "pacman -Rns --noconfirm $(pkg-list_true_orphans)"
getFeed = readProcess "python" ["/home/x/Git/dotfiles/scripts/autoupdate/archnews.py", "-d", "16"] []

main :: IO ()
main = do
    putStrLn "checking upstream feed ..."
    feed <- getFeed
    let keywords = ["intervention", "require", "manual"]
    when (or $ (flip isInfixOf feed) <$> keywords) $ do
        putStrLn "Updates found that require manual user intervention"
        exitSuccess
    putStrLn "updating ..."
    updates <- checkUpdates
    orphrans <- checkOrphans
    when updates $ sequence_ [systemUpdate, kernelDownload]
    when orphrans $ sequence_ [deleteOrphans, removeCache]
