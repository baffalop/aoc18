module FabricClaim
  ( Rect(..)
  , intersect
  , addIntersect
  , intersectAll
  ) where

data Rect =
  Rect
    { id :: Int
    , left :: Int
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

intersectAll :: [Rect] -> [Rect]
intersectAll = foldr addIntersect []

addIntersect :: Rect -> [Rect] -> [Rect]
addIntersect r [] = [r]
addIntersect r (x:xs) =
  case intersect r x of
    Nothing -> x : addIntersect r xs
    Just intersects -> foldr addIntersect xs intersects

intersect :: Rect -> Rect -> Maybe [Rect]
intersect r1 r2
  | eqGeometry r1 r2 = Just [r1 {overlap = True}]
  | not horizontalOverlap || not verticalOverlap = Nothing
  | horizontalOverlap && not alignedLeft =
    let (leftHalf, rightHalf) = vSplit leftRect $ left rightRect
     in fmap (leftHalf :) $ intersect rightHalf rightRect
  | verticalOverlap && not alignedTop =
    let (topHalf, bottomHalf) = hSplit topRect $ top bottomRect
     in fmap (topHalf :) $ intersect bottomHalf bottomRect
  | widthsCoincide =
    let (leftHalf, rightHalf) = vSplit widerRect $ right narrowerRect
     in fmap (rightHalf :) $ intersect leftHalf narrowerRect
  | heightsCoincide =
    let (topHalf, bottomHalf) = hSplit tallerRect $ bottom shorterRect
     in fmap (bottomHalf :) $ intersect topHalf shorterRect
  | otherwise = Nothing
  where
    (leftRect, rightRect) = orderBy left r1 r2
    (topRect, bottomRect) = orderBy top r1 r2
    (narrowerRect, widerRect) = orderBy w r1 r2
    (shorterRect, tallerRect) = orderBy h r1 r2
    horizontalOverlap = left rightRect < right leftRect
    verticalOverlap = top bottomRect < bottom topRect
    alignedLeft = left r1 == left r2
    alignedTop = top r1 == top r2
    widthsCoincide = w r1 == w r2
    heightsCoincide = h r1 == h r2

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
