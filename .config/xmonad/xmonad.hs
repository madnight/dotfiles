{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, PatternGuards #-}

import Data.List (isInfixOf)
import XMonad
import XMonad.Actions.FloatKeys
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
import XMonad.StackSet (greedyView, shift)
import XMonad.Hooks.InsertPosition

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

type GapSpec = [(Direction2D,Int)]

g :: GapSpec
g = [(U,45), (D,10), (R,10), (L,10)]

customLayout = example g
    . spacing 8
    . windowNavigation
    $ ResizableTall 2 (3/100) (1/2) []

data ExampleMsg = SetGap !Int !Direction2D    -- ^ Decrease a gap.
  deriving (Typeable)

data Example a = Example GapSpec [Direction2D]
  deriving (Show, Read)

instance Message ExampleMsg

applyGaps :: Example a -> Rectangle -> Rectangle
applyGaps gs r = foldr applyGap r (activeGaps gs)
  where
    applyGap (U,z) (Rectangle x y w h) = Rectangle x (y + fi z) w (h - fi z)
    applyGap (D,z) (Rectangle x y w h) = Rectangle x y w (h - fi z)
    applyGap (L,z) (Rectangle x y w h) = Rectangle (x + fi z) y (w - fi z) h
    applyGap (R,z) (Rectangle x y w h) = Rectangle x y (w - fi z) h
    activeGaps (Example conf cur) = filter ((`elem` cur) . fst) conf

example g = ModifiedLayout (Example g (map fst g))

instance LayoutModifier Example a where
    modifyLayout g w r = runLayout w (applyGaps g r)
    pureMess (Example conf cur) m
         | Just (SetGap i d)  <- fromMessage m
           = Just $ Example (setGap conf d (i)) cur
         | otherwise = Nothing

setGap :: GapSpec -> Direction2D -> Int -> GapSpec
setGap gs d i = map (\(dir,j) ->
    if (dir == d  && j /= 220)
       then (dir, max i 0)
       else if (dir == R)
           then (dir, 0)
           else (dir, j)) gs

{- wm independent defined via sxhkd keybind deamon, only xmonad specific shortcuts here -}
customKeys :: [(String, X())]
customKeys =
    [ ("M-<Return>", spawn "urxvt")
    , ("C-q",        kill)                     -- close window
    , ("M-c",        broadcastMessage (SetGap 220 R) >> refresh )       -- focus down
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
                mapM_ windows $ ($ i) <$> [shift, greedyView])
                | (i, k) <- XMonad.workspaces def `zip` ['1'..'9']]

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
