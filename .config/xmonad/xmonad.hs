import Data.List (isInfixOf)
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier as X
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Actions.FloatKeys
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

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

customLayout :: ModifiedLayout Gaps -- gaps between windows
    (ModifiedLayout Spacing -- spacing between display border and windows
    (ModifiedLayout WindowNavigation ResizableTall)) a -- additional window navigations
customLayout = gaps' . spacing 8 . windowNavigation $ ResizableTall 2 (3/100) (1/2) []
    where gaps' = gaps [(U,45), (D,10), (R,10), (L,10)]

{- wm independent defined via sxhkd keybind deamon, only xmonad specific shortcuts here -}
customKeys :: [(String, X())]
customKeys =
    [ ("M-<Return>", spawn "urxvt")
    , ("C-q",        kill)                     -- close window
    , ("M-j",        sendMessage $ Go D)       -- focus down
    , ("M-k",        sendMessage $ Go U)       -- focus up
    , ("M-h",        sendMessage $ Go L)       -- focus left
    , ("M-l",        sendMessage $ Go R)       -- focus right
    , ("M-C-h",      sendMessage $ Swap L)     -- swap left
    , ("M-C-j",      sendMessage $ Swap D)     -- swap down
    , ("M-C-k",      sendMessage $ Swap U)     -- swap up
    , ("M-C-l",      sendMessage $ Swap R)     -- swap right
    , ("M-S-j",      sendMessage MirrorShrink) -- shrink down
    , ("M-S-k",      sendMessage MirrorExpand) -- expand up
    , ("M-S-h",      sendMessage Shrink)       -- shrink left
    , ("M-S-l",      sendMessage Expand)       -- expand right
    , ("M-<Left>",   floatMove (-50, 0))       -- move floating left
    , ("M-<Right>",  floatMove (50, 0))        -- move floating right
    , ("M-<Up>",     floatMove (0, -50))       -- move floating up
    , ("M-<Down>",   floatMove (0, 50))        -- move floating down
    , ("M-q",        spawn "xmonad --recompile && xmonad --restart")
    ] ++ moveFollow
        where floatMove = withFocused . keysMoveWindow
              moveFollow = [("M-C-S-" ++ [k],
                mapM_ windows [W.shift i, W.greedyView i])
                | (i, k) <- zip (XMonad.workspaces def) (['1' .. '9'])]

customManager :: ManageHook
customManager = mconcat
    [ isFullscreen                                --> doFullFloat
    , className =? "Meld"                         --> doFullFloat
    , className =? "gcolor2"                      --> doCenterFloat
    , className =? "stalonetray"                  --> doIgnore
    , className =? "Vlc"                          --> doShift "5"
    , className =? "Thunderbird"                  --> doShift "3"
    , fmap ("libreoffice"  `isInfixOf`) className --> doShift "5"
    ]
