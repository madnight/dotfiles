import Control.Monad
import CustomGaps
import Data.List (isInfixOf)
import System.IO
import XMonad hiding (manageHook, layoutHook, workspaces)
import XMonad.Actions.FloatKeys
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import XMonad.Layout.IndependentScreens
import XMonad.StackSet (greedyView, view, shift, RationalRect(..))
import XMonad.Util.EZConfig hiding (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.XUtils (fi)
import qualified XMonad.Core as XMonad
import XMonad.Layout.IndependentScreens

main :: IO ()
main = xmonad . ewmh . fullscreenSupport $ conf

conf = def { XMonad.borderWidth = 2
    , XMonad.focusFollowsMouse = False
    , XMonad.focusedBorderColor = "#4dc1b5"
    , XMonad.layoutHook = layoutHook
    , XMonad.manageHook = manageHook
    , XMonad.modMask = mod4Mask -- ^ apple / win key
    , XMonad.normalBorderColor = "#000000"
    , XMonad.startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
    , XMonad.terminal = "urxvt"
    , XMonad.workspaces = workspaces
    } `additionalKeysP` additionalKeys


workspaces :: [PhysicalWorkspace]
workspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- | Press mod-shift-space for live reload
{- layoutHook :: ModifiedLayout Gaps             -- ^ gaps between windows -}
             {- (ModifiedLayout Spacing          -- ^ gaps between display border -}
             {- (ModifiedLayout WindowNavigation -- ^ additional window navigations -}
             {- (ModifiedLayout SmartBorder      -- ^ hide borders if fullscreen -}
             {- (Choose ResizableTall Full))))   -- ^ resizable layout fullscreen -}
             {- Window -}
layoutHook =
    gaps [(U,45), (D,10), (R,10), (L,10)]
    . spacing 8
    . windowNavigation
    . smartBorders
    $ ResizableTall master delta frac slaves ||| Full
        where master = 2   -- ^ number of master windows
              delta = 0.06 -- ^ change when resizing
              frac = 0.5   -- ^ width of master
              slaves = []  -- ^ fraction to multiply the window height

-- | Perform action on window that doesnt contain string as window class name
filterWindowByClass :: String -> (Window -> X ()) -> Window -> X ()
filterWindowByClass filter action window = do
    windowClass <- runQuery className window
    unless (filter `isInfixOf` windowClass) (action window)

-- | Filter windows that shouldn't be killed
filterKill :: Window -> X ()
filterKill = filterWindowByClass "Chromium" killWindow

-- | Resize a floating window
resizeFloat :: Dimension -> Dimension -> X ()
resizeFloat x y = withFocused $ keysResizeWindow (x, y) (0, 0)

-- | WM independent sxhkd in use as keybing deamon
-- Only xmonad specific shortcuts here
additionalKeys :: [(String, X())]
additionalKeys =
    [ ("M-<Return>",  spawn "urxvt")
    , ("C-q",         withFocused filterKill)   -- ^ close window
    , ("M-c",         conkyGap 220)             -- ^ toggle right conky gap
    , ("M-r",         rotAllDown)               -- ^ rotate all windows
    , ("M-j",         sendMessage $ Go D)       -- ^ focus down
    , ("M-k",         sendMessage $ Go U)       -- ^ focus up
    , ("M-h",         sendMessage $ Go L)       -- ^ focus left
    , ("M-l",         sendMessage $ Go R)       -- ^ focus right
    , ("M-C-h",       sendMessage $ Swap L)     -- ^ swap left
    , ("M-C-j",       sendMessage $ Swap D)     -- ^ swap down
    , ("M-C-k",       sendMessage $ Swap U)     -- ^ swap up
    , ("M-C-l",       sendMessage $ Swap R)     -- ^ swap right
    , ("M-S-j",       sendMessage MirrorShrink) -- ^ shrink down
    , ("M-S-k",       sendMessage MirrorExpand) -- ^ expand up
    , ("M-S-h",       sendMessage Shrink)       -- ^ shrink left
    , ("M-S-l",       sendMessage Expand)       -- ^ expand right
    , ("M-S-<Left>",  resizeFloat (-10) 0)      -- ^ float current windows
    , ("M-S-<Right>", resizeFloat 10 0)         -- ^ float current windows
    , ("M-S-<Up>",    resizeFloat 0 10)         -- ^ float current windows
    , ("M-S-<Down>",  resizeFloat 0 (-10))      -- ^ float current windows
    , ("M-<Left>",    floatMove (-50, 0))       -- ^ move floating left
    , ("M-<Right>",   floatMove (50, 0))        -- ^ move floating right
    , ("M-<Up>",      floatMove (0, -50))       -- ^ move floating up
    , ("M-<Down>",    floatMove (0, 50))        -- ^ move floating down
    , ("<F3>",        notes)                    -- ^ open notes in scratchpad
    , ("M-q",         spawn "xmonad --recompile && xmonad --restart")
    ] ++ moveFollow ++
        [
          ("M" ++ k, windows $ f i)
               | (i, k) <- zip (workspaces) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
               , (f, m) <- [(view, 0), (shift, shiftMask)]
        ]
        where floatMove = withFocused . keysMoveWindow
              moveFollow = [("M-C-S-" ++ [k],
                mapM_ (windows  ) $ ($ (i)) <$> [(onCurrentScreen shift), (onCurrentScreen view)])
                | (i, k) <- (workspaces' conf) `zip` ['1'..'9']]
              conkyGap g = broadcastMessage (SetGap g R) >> refresh
              notes = namedScratchpadAction scratchpads "notes"

-- | Spawn GVIM as notepad
scratchpads :: NamedScratchpads
scratchpads = [
    NS "notes" "gvim ~/notes" (title =? "notes (~) - GVIM")
        . customFloating $ RationalRect (1/6) (1/6) (2/3) (2/3)
    ]

manageHook :: ManageHook
manageHook = mconcat
    [ isFullscreen               --> disableDPMS
    , isDialog                   --> doCenterFloat
    , className =? "Meld"        --> doFullFloat
    , className =? "MPlayer"     --> doFullFloat
    , className =? "gcolor2"     --> doSideFloat SE
    , className =? "SpeedCrunch" --> doSideFloat SE
    , className =? "stalonetray" --> doIgnore
    , className =? "Conky"       --> doIgnore
    , className =? "Chromium"    --> doShift "1"
    , className =? "Thunderbird" --> doShift "3"
    , className =? "Vlc"         --> doShift "4"
    , className =? "Hexchat"     --> doShift "5"
    , className =? "Electrum"    --> doShift "6"
    ]
        where disableDPMS = safeSpawn "xset -dpms" [] >> doFullFloat
