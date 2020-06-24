module FabricClaim
  ( Rect(..)
  , intersect
  , addIntersect
  , intersectAll
  ) where

import qualified Data.Set as S

data Rect =
  Rect
    -- a Set of ids so we can track merged rects after intersect
    { claimId :: S.Set Int
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

-- recursively intersect a Rect with all Rects in a list
-- whenever an intersect splits up Rects, we need to individually add each of these
-- I think this ends up being O(n!)... 🤘
addIntersect :: Rect -> [Rect] -> [Rect]
addIntersect r [] = [r]
addIntersect r (x:xs) =
  case intersect r x of
    Nothing -> x : addIntersect r xs
    Just intersects -> foldr addIntersect xs intersects

-- if two Rects intersect, split them into non-overlapping Rects, one of which will represent the intersection
-- recursively chop off non-overlapping Rects until we're left with an exact overlap
intersect :: Rect -> Rect -> Maybe [Rect]
intersect r1 r2
  -- Rects overlap exactly: we've found our intersection
  | eqGeometry r1 r2 =
    let mergedIds = S.union (claimId r1) (claimId r2)
     in Just [r1 {claimId = mergedIds, overlap = True}]
  -- no overlap
  | not horizontalOverlap || not verticalOverlap = Nothing
  -- chop off left portion
  | not alignedLeft =
    let (leftHalf, rightHalf) = vSplit leftRect $ left rightRect
     in fmap (leftHalf :) $ intersect rightHalf rightRect
  -- chop off top portion
  | not alignedTop =
    let (topHalf, bottomHalf) = hSplit topRect $ top bottomRect
     in fmap (topHalf :) $ intersect bottomHalf bottomRect
  -- chop off right portion
  | not widthsCoincide =
    let (leftHalf, rightHalf) = vSplit widerRect $ right narrowerRect
     in fmap (rightHalf :) $ intersect leftHalf narrowerRect
  -- chop off bottom portion
  | not heightsCoincide =
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

orderBy :: (Ord b) => (a -> b) -> a -> a -> (a, a)
orderBy f r1 r2
  | f r1 < f r2 = (r1, r2)
  | otherwise = (r2, r1)

eqGeometry :: Rect -> Rect -> Bool
eqGeometry r1 r2 = normalise r1 == normalise r2
  where
    normalise r = r {claimId = S.empty, overlap = False}
