import Data.List
import Control.Monad
import Control.Concurrent.Thread.Delay
import System.Process

main = do
    let minimumMemory = 2000 -- ^ Megabytes
    free <- readProcess "free" ["-m"] []
    let avail = read $ words free !! 12 :: Int
    let warning = ("'Memory is running out " ++ (show avail) ++ " MB left'")
    when (avail < minimumMemory) . void $ system ("notify-send " ++ warning)
    let seconds = (*) 1000000
    delay $ seconds 10
    main
