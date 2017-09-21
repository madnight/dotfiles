#!/usr/bin/env stack
-- stack --install-ghc runghc wreq

import Control.Concurrent.Thread.Delay
import Control.Lens
import Control.Monad
import Data.List
import Network.Wreq
import System.Exit
import System.Process
import System.Random

isOnline :: String -> IO Bool
isOnline url = do
    response <- get url
    pure $ 200 == response ^. responseStatus . statusCode

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
    let seconds = (*) $ 1000 * 1000
    delay $ seconds rand  -- ^ sleep random seconds to avoid api rate limit
    void $ system command

update :: String -> String -> IO ()
update vcs dir = delaySystem $ cd dir ++ vcs ++ push

gitRepos :: [String]
gitRepos =
    ["MetaGer", "scid", "sed", "gnupg", "gitter",
    "grub", "nano", "babel-preset-php"]

svnRepos :: [String]
svnRepos =
    ["lfs", "filezilla", "gnuchess", "valgrind",
    "scidvspc", "chessx", "codeblocks"]

main :: IO ()
main = do
    online <- isOnline "https://google.com"
    unless online main
    forM_ gitRepos $ update git
    forM_ svnRepos $ update svn
    let hours = (*) $ product [60, 60, 1000, 1000]
    delay $ hours 2 -- sleep 2 hours
    main
