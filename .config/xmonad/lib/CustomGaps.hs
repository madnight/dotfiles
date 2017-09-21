{-# LANGUAGE
   FlexibleInstances,
   MultiParamTypeClasses,
   DeriveDataTypeable,
   TypeSynonymInstances,
   PatternGuards #-}

module CustomGaps
  (
    Direction2D(..)
  , Gaps
  , GapSpec
  , gaps
  , GapMessage(..)
  ) where

import Data.List (delete)
import Graphics.X11 (Rectangle(..))
import XMonad.Core
import XMonad.Layout.LayoutModifier
import XMonad.Util.Types (Direction2D(..))
import XMonad.Util.XUtils (fi)

type GapSpec = [(Direction2D,Int)]

data Gaps a = Gaps GapSpec [Direction2D]
  deriving (Show, Read)

data GapMessage = ToggleGaps
                | ToggleGap  !Direction2D
                | IncGap !Int !Direction2D
                | DecGap !Int !Direction2D
                | SetGap !Int !Direction2D
  deriving (Typeable)

instance Message GapMessage

instance LayoutModifier Gaps a where
    modifyLayout g w r = runLayout w (applyGaps g r)

    pureMess (Gaps conf cur) m
      | Just ToggleGaps    <- fromMessage m
        = Just $ Gaps conf (toggleGaps conf cur)
      | Just (ToggleGap d) <- fromMessage m
        = Just $ Gaps conf (toggleGap conf cur d)
      | Just (IncGap i d)  <- fromMessage m
        = Just $ Gaps (incGap conf d i) cur
      | Just (DecGap i d)  <- fromMessage m
        = Just $ Gaps (incGap conf d (-i)) cur
      | Just (SetGap i d)  <- fromMessage m
        = Just $ Gaps (setGap conf d (i)) cur
      | otherwise = Nothing

applyGaps :: Gaps a -> Rectangle -> Rectangle
applyGaps gs r = foldr applyGap r (activeGaps gs)
  where
    applyGap (U,z) (Rectangle x y w h) = Rectangle x (y + fi z) w (h - fi z)
    applyGap (D,z) (Rectangle x y w h) = Rectangle x y w (h - fi z)
    applyGap (L,z) (Rectangle x y w h) = Rectangle (x + fi z) y (w - fi z) h
    applyGap (R,z) (Rectangle x y w h) = Rectangle x y (w - fi z) h

activeGaps :: Gaps a -> GapSpec
activeGaps (Gaps conf cur) = filter ((`elem` cur) . fst) conf

toggleGaps :: GapSpec -> [Direction2D] -> [Direction2D]
toggleGaps conf [] = map fst conf
toggleGaps _    _  = []

toggleGap :: GapSpec -> [Direction2D] -> Direction2D -> [Direction2D]
toggleGap conf cur d | d `elem` cur            = delete d cur
                     | d `elem` (map fst conf) = d:cur
                     | otherwise               = cur

incGap :: GapSpec -> Direction2D -> Int -> GapSpec
incGap gs d i = map (\(dir,j) ->
  if dir == d then (dir,max (j+i) 0) else (dir,j)) gs

gaps :: GapSpec
     -> l a
     -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g (map fst g))

gaps' :: [((Direction2D,Int),Bool)]
      -> l a
      -> ModifiedLayout Gaps l a
gaps' g = ModifiedLayout (Gaps (map fst g) [d | ((d,_),v) <- g, v])

setGap :: GapSpec -> Direction2D -> Int -> GapSpec
setGap gs d i = map (\(dir,j) ->
    if dir == d  && j /= 220
       then (dir, max i 0)
       else if dir == R
           then (dir, 10)
           else (dir, j)) gs
