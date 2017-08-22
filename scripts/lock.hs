#!/usr/bin/env stack
-- stack --install-ghc runghc wreq
--
import Control.Monad
import Data.Maybe
import System.Environment
import System.Process

parseArgs :: Maybe a -> a
parseArgs Nothing  = errorWithoutStackTrace "missing arg: lock picture"
parseArgs (Just x) = x

main :: IO ()
main = do
    args <- getArgs
    let icon = parseArgs $ listToMaybe args
    let tmp = "/tmp/screen.png"
    cmd "scrot" [tmp]
    cmd "convert" [tmp, "-scale", "10%", "-scale", "1000%", tmp]
    cmd "convert" [tmp, icon, "-gravity", "center", "-composite", "-matte", tmp]
    cmd "i3lock" ["-i",  tmp]
    where
        cmd = (void .) . rawSystem
