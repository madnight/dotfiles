import Control.Monad
import Data.Maybe
import System.Environment
import System.Process

main :: IO ()
main = do
  home <- getEnv "HOME"
  let icon = home ++ "/scripts/lock.png"
  let tmp = "/tmp/screen.png"
    -- | take screenshot
  cmd "scrot" [tmp]
    -- | blur the screenshot
  cmd "convert" [tmp, "-scale", "10%", "-scale", "1000%", tmp]
    -- | add lock image to the center of the blured screenshot
  cmd "convert" [tmp, icon, "-gravity", "center", "-composite", "-matte", tmp]
    -- | call i3 with lock screen image
  cmd "i3lock" [ "-i" , tmp]
  where
    cmd = (void .) . rawSystem
