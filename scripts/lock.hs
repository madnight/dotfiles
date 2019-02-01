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
  cmd
    "i3lock"
    [ "-i"
    , tmp
    , "--insidevercolor=#ffffff00"
    , "--ringvercolor=#0000bb00"
    , "--insidewrongcolor=#CF42C100"
    , "--ringwrongcolor=#CF42C1ff"
    , "--insidecolor=#eeeeee00"
    , "--ringcolor=#00000000"
    , "--linecolor=#D648C3aa"
    , "--separatorcolor=#ffffffff"
    , "--verifcolor=#ffffffaa"
    , "--wrongcolor=#CF42C1ff"
    , "--timecolor=#ee00e0ee"
    , "--datecolor=#ee00e0ee"
    , "--layoutcolor=#000000ff"
    , "--keyhlcolor=#00DBF1ff"
    , "--bshlcolor=#00DBF1ff"
    , "--ring-width=10"
    , "--screen=1"
    , "--radius=180"
    , "--wrongtext=''"
    , "--veriftext=''"
    , "--noinputtext=''"
    , "--locktext=''"
    , "--lockfailedtext=''"
    ]
  where
    cmd = (void .) . rawSystem
