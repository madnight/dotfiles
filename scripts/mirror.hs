#!/usr/bin/env stack
-- stack --install-ghc runghc wreq

import Network.Wreq
import Control.Lens
import System.Process
import System.Exit
import System.Random
import Data.List
import Control.Monad
import Control.Concurrent.Thread.Delay


isOnline :: String -> IO Bool
isOnline url = do
    response <- get url
    return $ (==) 200 (response ^. responseStatus . statusCode)

push :: String
push = "&& git push -f origin master"

svn :: String
svn = "&& git svn rebase"

git :: String
git = "&& git pull upstream master"

cd :: String -> String
cd = ("cd ~/Git/" ++)

delaySystem :: String -> IO ()
delaySystem command = do
    newRand <- randomIO :: IO Integer
    let rand = newRand `mod` 60
    let secToNs = (*) $ 1000 * 1000
    delay $ secToNs rand  -- sleep random secs
    void $ system command

updateSVN :: String -> IO ()
updateSVN dir = delaySystem $ cd dir ++ svn ++ push

updateGit :: String -> IO ()
updateGit dir = delaySystem $ cd dir ++ git ++ push

gitRepos :: [String]
gitRepos = [ "MetaGer", "scid", "sed", "gnupg", "grub", "nano", "babel-preset-php"]

svnRepos :: [String]
svnRepos = ["lfs", "filezilla", "gnuchess", "valgrind", "scidvspc", "chessx", "codeblocks"]

main :: IO ()
main = do
    online <- isOnline "https://google.com"
    if not online then main else do
    mapM updateGit gitRepos
    mapM updateSVN svnRepos
    let hoursToNanoseconds = (*) $ 60 * 60 * 1000 * 1000
    delay $ hoursToNanoseconds 2 -- sleep 2 hours
    main

