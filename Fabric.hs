module Fabric
  ( intersect
  ) where

data Rect =
  Rect
    { left :: Int
    , top :: Int
    , w :: Int
    , h :: Int
    , overlap :: Bool
    }
  deriving (Eq, Ord, Show)

right :: Rect -> Int
right rect = left rect + w rect

bottom :: Rect -> Int
bottom rect = top rect + h rect

intersect :: Rect -> Rect -> [Rect]
intersect r1 r2
  | eqGeometry r1 r2 = [r1 {overlap = True}]
  | not leftCornerIntersects && not topCornerIntersects = [r1, r2]
  | leftCornerIntersects && not leftsCoincide =
    let (leftHalf, rightHalf) = vSplit leftRect $ left rightRect
     in leftHalf : intersect rightHalf rightRect
  | topCornerIntersects && not topsCoincide =
    let (topHalf, bottomHalf) = hSplit topRect $ top bottomRect
     in topHalf : intersect bottomHalf bottomRect
  | not widthsCoincide =
    let (leftHalf, rightHalf) = vSplit widerRect $ right narrowerRect
     in rightHalf : intersect leftHalf narrowerRect
  | not heightsCoincide =
    let (topHalf, bottomHalf) = hSplit tallerRect $ bottom shorterRect
     in bottomHalf : intersect topHalf shorterRect
  where
    (leftRect, rightRect) = orderBy left r1 r2
    (topRect, bottomRect) = orderBy top r1 r2
    (narrowerRect, widerRect) = orderBy w r1 r2
    (shorterRect, tallerRect) = orderBy h r1 r2
    leftsCoincide = left r1 == left r2
    topsCoincide = top r1 == top r2
    widthsCoincide = w r1 == w r2
    heightsCoincide = h r1 == h r2
    leftCornerIntersects = left rightRect < right leftRect
    topCornerIntersects = top bottomRect < bottom topRect

vSplit :: Rect -> Int -> (Rect, Rect)
vSplit r x = (leftHalf, rightHalf)
  where
    leftHalf = r {w = leftWidth}
    rightHalf = r {w = rightWidth, left = x}
    leftWidth = x - left r
    rightWidth = left r + w r - x

hSplit :: Rect -> Int -> (Rect, Rect)
hSplit r y = (topHalf, bottomHalf)
  where
    topHalf = r {h = topHeight}
    bottomHalf = r {h = bottomHeight, top = y}
    topHeight = y - top r
    bottomHeight = top r + h r - y

orderBy :: (Rect -> Int) -> Rect -> Rect -> (Rect, Rect)
orderBy f r1 r2
  | f r1 < f r2 = (r1, r2)
  | otherwise = (r2, r1)

eqGeometry :: Rect -> Rect -> Bool
eqGeometry r1 r2 = r1 {overlap = False} == r2 {overlap = False}

inBounds :: Int -> (Int, Int) -> Bool
inBounds n (x, y) = n <= (max x y) && n >= (min x y)
