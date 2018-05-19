import Data.List
import Control.Monad
import Control.Concurrent.Thread.Delay
import System.Process

main = do
    let minimumSpace = 3 * 1024 * 1024 -- ^ 3 GB
    free <- readProcess "df" ["-k", "."] []
    let avail = read $ words free !! 10 :: Int
    let availMB = avail `div` 1024
    let warning = "'Diskspace is running out " ++ show availMB ++ " MB left'"
    when (avail < minimumSpace) . void $ system ("notify-send " ++ warning)
    let seconds = (*) 1000000
    delay $ seconds 10
    main
