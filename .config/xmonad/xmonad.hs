import Data.List (isInfixOf)
import XMonad
import XMonad.CustomGaps
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Actions.FloatKeys
import XMonad.Util.XUtils (fi)
import XMonad.Util.EZConfig
import XMonad.StackSet (greedyView, shift, RationalRect(..))
import XMonad.Hooks.InsertPosition
import XMonad.Util.NamedScratchpad
import Control.Monad

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

{- press mod-shift-space for live update -}
customLayout :: ModifiedLayout Gaps                    -- gaps between windows
    (ModifiedLayout Spacing                            -- spacing between display border and windows
    (ModifiedLayout WindowNavigation ResizableTall)) a -- additional window navigations
customLayout = gaps [(U,45), (D,10), (R,10), (L,10)]
    . spacing 8
    . windowNavigation
    $ ResizableTall 1 (6/100) (1/2) []

{- filter windows that shouldn't be killed -}
filterKill :: Window -> X ()
filterKill window = do
    windowClass <- runQuery className window
    unless ("Chromium" `isInfixOf` windowClass) kill

{- wm independent sxhkd in use as keybing deamon, only xmonad specific shortcuts here -}
customKeys :: [(String, X())]
customKeys =
    [ ("M-<Return>",  spawn "urxvt")
    , ("C-q",         withFocused filterKill)   -- close window
    , ("M-c",         conkyGap 220)             -- toggle right conky gap
    , ("M-j",         sendMessage $ Go D)       -- focus down
    , ("M-k",         sendMessage $ Go U)       -- focus up
    , ("M-h",         sendMessage $ Go L)       -- focus left
    , ("M-l",         sendMessage $ Go R)       -- focus right
    , ("M-C-h",       sendMessage $ Swap L)     -- swap left
    , ("M-C-j",       sendMessage $ Swap D)     -- swap down
    , ("M-C-k",       sendMessage $ Swap U)     -- swap up
    , ("M-C-l",       sendMessage $ Swap R)     -- swap right
    , ("M-S-j",       sendMessage MirrorShrink) -- shrink down
    , ("M-S-k",       sendMessage MirrorExpand) -- expand up
    , ("M-S-h",       sendMessage Shrink)       -- shrink left
    , ("M-S-l",       sendMessage Expand)       -- expand right
    , ("M-<Space>",   withFocused float)        -- float current windows
    , ("M-S-<Left>",  resizeFloat (-10) 0)      -- float current windows
    , ("M-S-<Right>", resizeFloat 10 0)         -- float current windows
    , ("M-S-<Up>",    resizeFloat 0 10)         -- float current windows
    , ("M-S-<Down>",  resizeFloat 0 (-10))      -- float current windows
    , ("M-<Left>",    floatMove (-50, 0))       -- move floating left
    , ("M-<Right>",   floatMove (50, 0))        -- move floating right
    , ("M-<Up>",      floatMove (0, -50))       -- move floating up
    , ("M-<Down>",    floatMove (0, 50))        -- move floating down
    , ("<F3>",        notes)                    -- open notes in scratchpad
    , ("M-q",         spawn "xmonad --recompile && xmonad --restart")
    ] ++ moveFollow
        where floatMove = withFocused . keysMoveWindow
              moveFollow = [("M-C-S-" ++ [k],
                mapM_ windows $ ($ i) <$> [shift, greedyView])
                | (i, k) <- XMonad.workspaces def `zip` ['1'..'9']]
              conkyGap g = broadcastMessage (SetGap g R) >> refresh
              resizeFloat x y = withFocused $ keysResizeWindow (x, y) (0, 0)
              notes = namedScratchpadAction scratchpads "notes"

scratchpads :: NamedScratchpads
scratchpads = [
    -- run vim in urxvt with notes opened
    NS "notes" "urxvt -e vim ~/notes" (title =? "notes (~) - VIM")
        (customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

customManager :: ManageHook
customManager = mconcat
    [ isFullscreen                               --> doFullFloat
    , className =? "Meld"                        --> doFullFloat
    , className =? "gcolor2"                     --> insertPosition Below Older
    , className =? "SpeedCrunch"                 --> insertPosition End Newer
    , className =? "stalonetray"                 --> doIgnore
    , className =? "Conky"                       --> doIgnore
    , className =? "Vlc"                         --> doShift "5"
    , className =? "Thunderbird"                 --> doShift "3"
    , ("libreoffice"  `isInfixOf`) <$> className --> doShift "5"
    ]
