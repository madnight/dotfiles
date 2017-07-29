import System.Process
import Data.List
import Control.Monad
import Control.Concurrent.Thread.Delay

main = do
    let minimumMemory = 2000 -- Megabytes
    free <- readProcess "free" ["-m"] []
    let avail = read $ words free !! 12 :: Int
    let warning = ("'Memory is running out " ++ (show avail) ++ " MB " ++ "left'")
    when (avail < minimumMemory) . void $ system ("notify-send " ++ warning)
    let secs = (*) 1000000
    delay $ secs 10
    main
