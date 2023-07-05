-- | Utility functions to get the points in a 2D triangle
module Triangles where

import Data.Array.Accelerate
import qualified Prelude

type PointT    = (Int, Int)
type LineT     = (PointT, PointT)
type TriangleT = (PointT, PointT, PointT)
type Point     = Exp PointT
type Line      = Exp LineT
type Triangle  = Exp TriangleT

-- | Returns how many points are in the given line
pointsInLine :: Line -> Exp Int
pointsInLine (T2 (T2 x1 y1) (T2 x2 y2)) = 1 + max (abs $ x2 - x1) (abs $ y2 - y1)

-- | Returns whether the first value is greater than (1), less than (-1) or equal to (0) the first value
compare :: Exp Int -> Exp Int -> Exp Int
compare v1 v2 = ifThenElse (v2 > v1) 1 (ifThenElse (v1 > v2) (-1) 0)

-- | Returns the slope between the two given points
slope :: Point -> Point -> Exp Float
slope (T2 x1 y1) (T2 x2 y2) = 
    ifThenElse 
      (x2 == x1) 
      (ifThenElse (y2 > y1) 1.0 (-1.0))
      ((fromIntegral (y2 - y2)) / (abs $ (fromIntegral (x2 - x1))))

-- | Gets the point in the given line, at the given index
getPointInLine :: Line -> Exp Int -> Point
getPointInLine (T2 p1 p2) idx =
    ifThenElse
      ((abs $ (fst p1) - (fst p2)) > (abs $ (snd p1) - (snd p2)) )
      (
        let dir = Triangles.compare (fst p1) (fst p2)
            sl  = slope p1 p2
        in
            T2 (fst p1 + dir * idx) (snd p1 + (round $ sl * fromIntegral idx))
      )
      (
        let dir = Triangles.compare (snd p1) (snd p2)
            sl  = slope (T2 (snd p1) (fst p1)) (T2 (snd p2) (fst p2))
        in
            T2 (fst p1 + (round $ sl * fromIntegral idx)) (snd p1  + dir * idx )
      )

-- | Returns all points that make up the lines in the given 'Vector' of lines
pointsOfLines :: Acc (Vector LineT) -> Acc (Vector PointT)
pointsOfLines = Data.Array.Accelerate.expand Basic pointsInLine getPointInLine

-- |Returns how many lines are in the given triangle
linesInTriangle :: Triangle -> Exp Int
linesInTriangle (T3 p _ r) = (snd r) - (snd p) + 1

-- | Returns the slope of the line between the two given points
dxdy :: Point -> Point -> Exp Float
dxdy a b =
    let dx = fst b - fst a
        dy = snd b - snd a
    in
      ifThenElse (dy == 0)
        0
        (fromIntegral dx) / (fromIntegral dy)

-- | Considering that a triangle can be drawn using horizontal lines, returns
--   the ith line that makes up the given triangle
getLineInTriangle :: Triangle -> Exp Int -> Line
getLineInTriangle (T3 p q r) idx =
    let y = (snd p) + idx in
    ifThenElse (idx <= (snd q) - (snd p) && not (snd p == snd q))
      (
        let sl1 = dxdy p q
            sl2 = dxdy p r
            x1  = (fst p) + (round $ sl1 * fromIntegral idx)
            x2  = (fst p) + (round $ sl2 * fromIntegral idx)
        in T2 (T2 x1 y) (T2 x2 y)
      )
      (
        let sl1 = dxdy r p
            sl2 = dxdy r q
            dy  = ((snd r) - (snd p)) - idx
            x1  = (fst r) - (round $ sl1 * fromIntegral dy)
            x2  = (fst r) - (round $ sl2 * fromIntegral dy)
        in T2 (T2 x1 y) (T2 x2 y)
      )

-- | Orders the three points of a triangle, such that its coordinates are in a specific order
normalise :: Triangle -> Triangle
normalise (T3 p'' q''' r') =
    let bubble a b = ifThenElse (snd b < snd a) (T2 b a) (T2 a b)
        (T2 p' q'') = bubble p'' q'''
        (T2 q' r)   = bubble q'' r'
        (T2 p q)    = bubble p' q'
    in
      T3 p q r

-- | Returns all lines that make up a given 'Vector' of triangles
linesOfTriangles :: Acc (Vector TriangleT) -> Acc (Vector LineT)
linesOfTriangles = (Data.Array.Accelerate.expand Basic linesInTriangle getLineInTriangle) . map normalise

-- | Returns all points that make up a given 'Vector' of triangles
pointsOfTriangles :: Acc (Vector TriangleT) -> Acc (Vector PointT)
pointsOfTriangles = pointsOfLines . linesOfTriangles