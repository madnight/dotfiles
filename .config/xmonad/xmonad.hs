import XMonad
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.StackSet as W
import XMonad.Layout.LayoutModifier as X
import Data.List (isInfixOf)

main :: IO ()
main = xmonad $ def
    { terminal = "urxvt"
    , manageHook = customManager
    , modMask = mod4Mask -- apple / win key
    , layoutHook = customLayout
    , focusedBorderColor = "#87AFAF"
    , borderWidth = 2
    , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
    } `additionalKeysP` customKeys

customLayout :: X.ModifiedLayout Gaps (X.ModifiedLayout Spacing Tall) a
customLayout = gaps' . spacing 8 $ Tall 1 (3/100) (1/2)
    where gaps' = gaps [(U,45), (D,10), (R,10), (L,10)]

customKeys :: [(String, X())]
customKeys =
    [ ("M-p",                    spawn "")
    , ("M-<Return>",             spawn "urxvt")
    , ("M-q",                    spawn "xmonad --recompile && xmonad --restart")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
    , ("<XF86AudioMute>",        spawn "amixer sset Master toggle")
    ]

customManager :: ManageHook
customManager = mconcat
    [ isFullscreen                                --> doFullFloat
    , className =? "Xmessage"                     --> doCenterFloat
    , className =? "stalonetray"                  --> doIgnore
    , className =? "Gimp"                         --> doShift "9"
    , className =? "Thunderbird"                  --> doShift "2"
    , fmap ("libreoffice"  `isInfixOf`) className --> doShift "5"
    ]
