import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.List
import Network.Wreq
import System.Exit
import System.Process

processIsRunning :: String -> IO Bool
processIsRunning = (<$> readProcess "ps" ["x"] []) . isInfixOf

startConky ::  String -> IO ExitCode
startConky = system . ("conky -c ~/.config/conky/" ++)

conky :: String -> IO ()
conky configName = do
     exit <- processIsRunning configName
     if exit then
         putStrLn $ "conky " ++ configName ++ " already running"
     else
         print =<< startConky (configName ++  " &")

--                                       +---- List of command line arguments
-- The command to run ----------+        |
--                              |        |      +------- String to pass
sideBar :: IO () --             |        |      |        on standard input
sideBar = do     --             V        V      V
    hostname <- readProcess "hostname" mempty mempty
    case hostname of
      "arch\n"    -> conky "conkyrc"
      "skylake\n" -> conky "conkyrc-work"
      _           -> putStrLn "unkown host"

main :: IO ()
main =
  forever $ do
    sideBar
    threadDelay $ 60 * 1000 * 1000 -- 60 seconds

-- hasInternet :: IO Bool
-- hasInternet = do
--     response <- get "https://google.com"
--     pure $ 200 == (response ^. responseStatus . statusCode)

-- online <- hasInternet
-- if online then mapM_ conky ["weather", "errors"] else main
