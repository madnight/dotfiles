#!/usr/bin/env stack
-- stack --install-ghc runghc

import Control.Monad
import Data.Maybe
import System.Environment
import System.Process

main :: IO ()
main = do
    args <- getArgs
    let icon = parseArgs $ listToMaybe args
    let tmp = "/tmp/screen.png"
    -- | take screenshot
    cmd "scrot" [tmp]
    -- | blur the screenshot
    cmd "convert" [tmp, "-scale", "10%", "-scale", "1000%", tmp]
    -- | add lock image to the center of the blured screenshot
    cmd "convert" [tmp, icon, "-gravity", "center", "-composite", "-matte", tmp]
    -- | call i3 with lock screen image
    cmd "i3lock" ["-i",  tmp]
    where
        cmd = (void .) . rawSystem
        parseArgs Nothing  = errorWithoutStackTrace "missing arg: lock picture"
        parseArgs (Just x) = x
