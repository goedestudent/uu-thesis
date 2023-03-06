{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RebindableSyntax     #-}

module Lib
    ( someFunc
    ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.PTX (run)
import qualified Prelude
import Prelude ((>>=), (<$>), return)
import Prelude (IO, putStrLn, id, show)
import Debug.Trace (trace)
import qualified Data.List
import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture.Types
import Codec.Picture (convertRGB8, readImage, writePng)

import Expand as Lib

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- -----------------------
-- -- Utility functions --
-- -----------------------

-- -- | Moves elements n indices to the left. That is b[x] = a[x+n] for `rotate n a`.
-- -- >>> run $ rotate (-1) (use $ fromList (Z:.10) [0..])
-- --     Vector (Z :. 10) [9.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]
-- rotate
--   :: (Elt e, Prelude.Num (Exp e)) =>
--      Exp Int -> Acc (Vector e) -> Acc (Vector e)
-- -- Note: the commented one uses scatter, thus allows for less fusion
-- -- rotate n a = scatter (generate (shape a) ((\(I1 i) -> (abs (i - n)) `rem` l))) (generate (shape a) (\(I1 i) -> -1)) a
-- rotate n a = backpermute (shape a) (\(I1 i) -> I1 $ (abs (i + n)) `rem` l) a
--     where l = length a
--           abs x = ifThenElse (x < 0) (l + x) x

-- -- These are directly translated from the paper by Elsman et al. They therefore
-- -- use the custom provided segmented scan.
-- segm_scan
--   :: (Lift Exp b, Elt b) =>
--      (Exp b -> Exp b -> Exp b)
--      -> Exp b
--      -> Acc (Vector Bool)
--      -> Acc (Vector b)
--      -> Acc (Vector b)
-- segm_scan op ne flags as = map snd scan
--     where zp   = zip flags as
--           scan = postscanl (\(T2 x_flag x) (T2 y_flag y) -> T2 ( x_flag || y_flag) (if (y_flag) then y else (x `op` y))) (T2 False_ ne) $ zp

-- repl_iota
--   :: Acc (Array (DIM0 :. Int) Int) -> Acc (Array (DIM0 :. Int) Int)
-- repl_iota reps =
--     let s1 = postscanl (+) 0 reps
--         s2 = map (\i -> ifThenElse (i == 0) 0 (s1 !! (i - 1))) (generate (shape reps) (\(I1 i) -> i))
--         fld = (the $ fold (+) 0 reps)
--         tmp = scatter s2 (generate (I1 fld) (const 0)) (generate (shape reps) (\(I1 i) -> i))
--         flags = map (>0) tmp
--     in segm_scan (+) 0 flags tmp

-- segm_iota :: Acc (Vector Bool) -> Acc (Vector Int)
-- segm_iota flags = map (\x -> x-1) $ segm_scan (+) 0 flags (fill (shape flags) 1)

-- -- This is zipWith in Accelerate
-- map2
--   :: (Shape sh, Elt x0, Elt x1, Elt b) =>
--      (Exp x0 -> Exp x1 -> Exp b)
--      -> Acc (Array sh x0) -> Acc (Array sh x1) -> Acc (Array sh b)
-- map2 f a b = map (\(T2 e1 e2) -> f e1 e2) (zip a b)


-- -- Accelerate-based variants of the utility functions. They use the Accelerate segmented scan
-- replIota'
--   :: Acc (Array (DIM0 :. Int) Int) -> Acc (Array (DIM0 :. Int) Int)
-- replIota' reps =
--     let s1 = postscanl (+) 0 reps
--         s2 = map (\i -> ifThenElse (i == 0) 0 (s1 !! (i - 1))) (generate (shape reps) (\(I1 i) -> i))
--         fld = (the $ fold (+) 0 reps)
--         tmp = scatter s2 (generate (I1 fld) (const 0)) (generate (shape reps) (\(I1 i) -> i))
--     in  postscanlSeg (+) 0 tmp reps

-- segmIota'
--   :: Acc (Segments Int) -> Acc (Vector Int)
-- segmIota' segments = 
--     let total = the $ fold (+) 0 segments
--     in map (\x -> x-1) $ 
--       postscanlSeg (+) 0 (generate (I1 total) (const 1)) segments

-- trc x e = trace (show (run x)) e

-- -- More optimised/shorter varients of the utility functions
-- replIota
--   :: Acc (Vector Int) -> (Acc (Vector Int), Acc (Vector Bool))
-- replIota reps =
--     let s1    = scanl (+) 0 reps      -- How many items before this one?
--         fld   = s1 !! (length s1 - 1) -- How many items in total?
--         tmp   = scatter s1 (fill (I1 fld) 0) (enumFromN (shape reps) 0) -- Index of item at first occurance
--         flags = map (>0) tmp          -- Where the new segments start
--     in  (segm_scan (+) 0 flags tmp, flags)    -- Smear the first occurance to the next elements

-- segmIota = segm_iota

-- -- let mat = fromList (Z:.15) [0..] :: Vector Int
-- -- let seg = fromList (Z:.5) [3,3,1,1,20] :: Segments Int
-- -- run $ postscanlSeg (+) 0 (use mat) (use seg)

-- -- seg' = Lib.expand (id) (\_ i -> ifThenElse (i == 0) (lift True) (lift False)) (use $ seg)
-- --  run $ segm_scan (+) 0 seg' (use $ mat)

-- -- Expand as defined in the paper
-- expandPaper
--   :: (Elt e, Elt b) =>
--      (Exp e -> Exp Int)
--      -> (Exp e -> Exp Int -> Exp b)
--      -> Acc (Vector e)
--      -> Acc (Vector b)
-- expandPaper sz get arr = 
--     let szs   = map sz arr 
--         idxs  = repl_iota szs
--         iotas = segm_iota $
--                   zipWith 
--                     (\a b -> not (a == b)) 
--                     idxs 
--                     (rotate (-1) idxs)
--     in zipWith (\i j -> get (arr !! i) j) idxs iotas

-- -- Expand using Accelerate segmented scan
-- expandAccelerate
--   :: (Elt e, Elt b) =>
--      (Exp e -> Exp Int)
--      -> (Exp e -> Exp Int -> Exp b)
--      -> Acc (Vector e)
--      -> Acc (Vector b)
-- expandAccelerate sz get arr = 
--     let szs   = map sz arr 
--         idxs  = replIota' szs
--         iotas = segmIota' szs
--     in zipWith (\i j -> get (arr !! i) j) idxs iotas

-- -- Expand using fewest computations
-- expand
--   :: (Elt e, Elt b) =>
--      (Exp e -> Exp Int)
--      -> (Exp e -> Exp Int -> Exp b)
--      -> Acc (Vector e)
--      -> Acc (Vector b)
-- expand sz get arr = 
--     let szs           = map sz arr 
--         (idxs, flags) = replIota szs
--         iotas         = segmIota flags
--     in zipWith (\i j -> get (arr !! i) j) idxs iotas

test = Lib.expand id (\a i -> a * i) (use $ fromList (Z:.4) [0..])
testR = run $ test


-- type PointT    = (Int, Int)
-- type LineT     = (PointT, PointT)
-- type TriangleT = (PointT, PointT, PointT)
-- type Point     = Exp PointT
-- type Line      = Exp LineT
-- type Triangle  = Exp TriangleT

-- pointsInLine :: Line -> Exp Int
-- pointsInLine (T2 (T2 x1 y1) (T2 x2 y2)) = 1 + max (abs $ x2 - x1) (abs $ y2 - y1)

-- compare :: Exp Int -> Exp Int -> Exp Int
-- compare v1 v2 = ifThenElse (v2 > v1) 1 (ifThenElse (v1 > v2) (-1) 0)

-- -- slope :: Point -> Point -> Exp Float
-- -- slope (T2 x1 y1) (T2 x2 y2) = 
-- --     ifThenElse 
-- --       (x2 == x1) 
-- --       (ifThenElse (y2 > y1) 1.0 (-1.0))
-- --       ((fromIntegral y2) - (fromIntegral y1)) / (abs $ (fromIntegral x2) - (fromIntegral x1))
-- slope :: Point -> Point -> Exp Float
-- slope (T2 x1 y1) (T2 x2 y2) = 
--     ifThenElse 
--       (x2 == x1) 
--       (ifThenElse (y2 > y1) 1.0 (-1.0))
--       ((fromIntegral (y2 - y2)) / (abs $ (fromIntegral (x2 - x1))))

-- getPointInLine :: Line -> Exp Int -> Point
-- getPointInLine (T2 p1 p2) idx =
--     ifThenElse
--       ((abs $ (fst p1) - (fst p2)) > (abs $ (snd p1) - (snd p2)) )
--       (
--         let dir = Lib.compare (fst p1) (fst p2)
--             sl  = slope p1 p2
--         in
--             T2 (fst p1 + dir * idx) (snd p1 + (round $ sl * fromIntegral idx))
--       )
--       (
--         let dir = Lib.compare (snd p1) (snd p2)
--             sl  = slope (T2 (snd p1) (fst p1)) (T2 (snd p2) (fst p2))
--         in
--             T2 (fst p1 + (round $ sl * fromIntegral idx)) (snd p1  + dir * idx )
--       )

-- pointsOfLines :: Acc (Vector LineT) -> Acc (Vector PointT)
-- pointsOfLines = Data.Array.Accelerate.expand pointsInLine getPointInLine

-- test2 = run $ pointsOfLines (use $ fromList (Z:.2) [((1,2), (3,4)), ((1,1), (3,3))])


-- linesInTriangle :: Triangle -> Exp Int
-- linesInTriangle (T3 p _ r) = (snd r) - (snd p) + 1

-- dxdy :: Point -> Point -> Exp Float
-- dxdy a b =
--     let dx = fst b - fst a
--         dy = snd b - snd a
--     in
--       ifThenElse (dy == 0)
--         0
--         (fromIntegral dx) / (fromIntegral dy)

-- getLineInTriangle :: Triangle -> Exp Int -> Line
-- getLineInTriangle (T3 p q r) idx =
--     let y = (snd p) + idx in
--     ifThenElse (idx <= (snd q) - (snd p) && not (snd p == snd q))
--       (
--         let sl1 = dxdy p q
--             sl2 = dxdy p r
--             x1  = (fst p) + (round $ sl1 * fromIntegral idx)
--             x2  = (fst p) + (round $ sl2 * fromIntegral idx)
--         in T2 (T2 x1 y) (T2 x2 y)
--       )
--       (
--         let sl1 = dxdy r p
--             sl2 = dxdy r q
--             dy  = ((snd r) - (snd p)) - idx
--             x1  = (fst r) - (round $ sl1 * fromIntegral dy)
--             x2  = (fst r) - (round $ sl2 * fromIntegral dy)
--         in T2 (T2 x1 y) (T2 x2 y)
--       )

-- normalise :: Triangle -> Triangle
-- normalise (T3 p'' q''' r') =
--     let bubble a b = ifThenElse (snd b < snd a) (T2 b a) (T2 a b)
--         (T2 p' q'') = bubble p'' q'''
--         (T2 q' r)   = bubble q'' r'
--         (T2 p q)    = bubble p' q'
--     in
--       T3 p q r

-- linesOfTriangles :: Acc (Vector TriangleT) -> Acc (Vector LineT)
-- linesOfTriangles = (Data.Array.Accelerate.expand linesInTriangle getLineInTriangle) . map normalise


-- pointsOfTriangles :: Acc (Vector TriangleT) -> Acc (Vector PointT)
-- pointsOfTriangles = pointsOfLines . linesOfTriangles


-- -- Colored varients of above functions
-- pointsOfLines' :: Acc (Vector (LineT, PixelRGB8)) -> Acc (Vector (PointT, PixelRGB8))
-- pointsOfLines' = Lib.expand (\(T2 l _) -> pointsInLine l) (\(T2 l c) i -> T2 (getPointInLine l i) c)

-- linesOfTriangles' :: Acc (Vector (TriangleT, PixelRGB8)) -> Acc (Vector (LineT, PixelRGB8))
-- linesOfTriangles' = Lib.expand (\(T2 t _) -> linesInTriangle t) (\(T2 t c) i -> T2 (getLineInTriangle t i) c) . map (\(T2 t c) -> T2 (normalise t) c)

-- pointsOfTriangles' :: Acc (Vector (TriangleT, PixelRGB8)) -> Acc (Vector (PointT, PixelRGB8))
-- pointsOfTriangles' = pointsOfLines' . linesOfTriangles'

-- -- Z-buffer varients of above functions
-- pointsOfLinesZ' :: Acc (Vector (LineT, (PixelRGB8, Float))) -> Acc (Vector (PointT, (PixelRGB8, Float)))
-- pointsOfLinesZ' = Lib.expand (\(T2 l _) -> pointsInLine l) (\(T2 l (T2 c z)) i -> T2 (getPointInLine l i) (T2 c z) )

-- linesOfTrianglesZ' :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> Acc (Vector (LineT, (PixelRGB8, Float)))
-- linesOfTrianglesZ' = Lib.expand (\(T2 t _) -> linesInTriangle t) (\(T2 t (T2 c z)) i -> T2 (getLineInTriangle t i) (T2 c z)) . map (\(T2 t (T2 c z)) -> T2 (normalise t) (T2 c z))

-- pointsOfTrianglesZ' :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> Acc (Vector (PointT, (PixelRGB8, Float)))
-- pointsOfTrianglesZ' = pointsOfLinesZ' . linesOfTrianglesZ'

-- test3 :: Vector PointT
-- test3 = run test3Acc

-- test3Acc :: Acc (Vector PointT)
-- test3Acc = pointsOfTriangles $ use $ fromList (Z:.(Prelude.length triangles)) triangles
--   where triangles = [
--             ((5,10), (2,28) , (18,20)),
--             ((42,6), (58,10), (25,22)),
--             ((8,3) , (15,15), (35,7)),
--             ((1,1), (5,2), (3, 5)),
--             ((6, 40), (0, 40), (3, 35))
--           ]

-- test3' :: Array (Z:. Int :. Int) Int
-- test3' = let maxY = 1 + (the $ maximum $ map snd test3Acc)
--              maxX = 1 + (the $ maximum $ map fst test3Acc)
--              def  = generate (I2 maxY maxX) (\_ -> 0)
--              p (I1 i) = Just_ $ (\(T2 a b) -> I2 b a) (test3Acc !! i)
--              xs = generate (I1 $ length test3Acc) (\_ -> 7) :: Acc (Vector Int)
--          in
--            run $ permute (max) def p xs

-- type Point3T = (Int, Int, Int)
-- type Triangle3T = (Point3T, Point3T, Point3T)
-- type Point3 = Exp Point3T
-- type Triangle3 = Exp Triangle3T

-- -- threeDToScreen :: Acc (Matrix Float) -> Acc (Vector Float) -> Acc (Vector PointT)
-- threeDToScreen transformation vector = imap (\(I2 i j) e -> e * (vector !! j) ) transformation

-- --threeD :: Exp (Float, Float, Float) -> Acc (Matrix Float)
-- threeD (a, b, c) = use $ fromList (Z:.1:.4) [a,b,c, 1]

-- type Matrix4D = ((Float, Float, Float, Float), (Float, Float, Float, Float), (Float, Float, Float, Float), (Float, Float, Float, Float))
-- type Vector4D = (Float, Float, Float, Float)
-- type Vector3D = (Float, Float, Float)

-- -- mat :: Acc (Matrix Float)
-- -- mat = use $ fromList (Z:.4:.4) [
-- --     1.0, 0.0, 0.0, 0.0,
-- --     1.0, 1.0, 0.0, 0.0,
-- --     0.0, 0.0, 1.0, 0.0,
-- --     0.0, 0.0, 0.0, 1.0
-- --   ]

-- -- vec :: Acc (Matrix Float)
-- -- vec = use $ fromList (Z:.1:.4) [
-- --     2.0, 3.0, 4.0, 1.0
-- --   ]

-- vp :: Exp Matrix4D
-- vp = T4 
--   (T4 (screenX / 2.0) 0.0 0.0 ((screenX - 1.0) / 2.0))
--   (T4 0.0 (screenY / 2.0) 0.0 ((screenY - 1.0) / 2.0))
--   (T4 0.0 0.0 1.0 0.0)
--   (T4 0.0 0.0 0.0 1.0)



-- mul3 :: Exp Float -> Exp Vector3D -> Exp Vector3D
-- mul3 s (T3 x y z) = T3 (x * s) (y * s) (z * s)

-- vecW :: Exp Vector3D
-- vecW = mul3 (-1 / norm3 gaze ) gaze

-- -- Returns the norm/length of a 3D vector
-- norm3 :: Exp Vector3D -> Exp Float
-- norm3 (T3 x y z) = sqrt $ (x*x) + (y*y) + (z*z)

-- -- Returns the cross product of two 3D vectors
-- cross3 :: Exp Vector3D -> Exp Vector3D -> Exp Vector3D
-- cross3 (T3 ax ay az) (T3 bx by bz) = T3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

-- vecU :: Exp Vector3D
-- vecU = mul3 (norm3 crossed) crossed
--   where crossed = cross3 up vecW

-- vecV :: Exp Vector3D
-- vecV = cross3 vecW vecU

-- -- Camera coordinate transformation matrix
-- cam :: Exp Matrix4D
-- cam = matMul mat1 mat2
--   where thd (T3 _ _ z) = z
--         snd (T3 _ y _) = y
--         fst (T3 x _ _) = x

--         mat1 = T4 
--                 (T4 (fst vecU) (snd vecU) (thd vecU) 0)
--                 (T4 (fst vecV) (snd vecV) (thd vecV) 0)
--                 (T4 (fst vecW) (snd vecW) (thd vecW) 0)
--                 (T4 0          0          0          1)
--         mat2 = T4 
--                 (T4 1 0 0 ((-1) * fst eye))
--                 (T4 0 1 0 ((-1) * snd eye))
--                 (T4 0 0 1 ((-1) * thd eye))
--                 (T4 0 0 0 1               )

-- -- Orthographic projection matrix
-- orth :: Exp Matrix4D
-- orth = T4 
--   (T4 (2.0 / (right - left)) 0.0 0.0 (-((right + left) / (right - left))))
--   (T4 0.0 (2.0 / (top - bottom)) 0.0 (-((top + bottom) / (top - bottom))))
--   (T4 0.0 0.0 (2.0 / (near - far)) (-((near + far) / (near - far))))
--   (T4 0.0 0.0 0.0 1.0)

-- -- Matrix multiplication for square matrices
-- matMul :: Exp Matrix4D -> Exp Matrix4D -> Exp Matrix4D
-- matMul
--   (T4
--     (T4 a b c d)
--     (T4 e f g h)
--     (T4 i j k l)
--     (T4 m n o p)
--   )

--   (T4
--     (T4 a' b' c' d')
--     (T4 e' f' g' h')
--     (T4 i' j' k' l')
--     (T4 m' n' o' p')
--   )
--   = (T4
--     (T4 (a * a' + b * e' + c * i' + d * m') (a * b' + b * f' + c * j' + d * n') (a * c' + b * g' + c * k' + d * o') (a * d' + b * h' + c * l' + d * p'))
--     (T4 (e * a' + f * e' + g * i' + h * m') (e * b' + f * f' + g * j' + h * n') (e * c' + f * g' + g * k' + h * o') (e * d' + f * h' + g * l' + h * p'))
--     (T4 (i * a' + j * e' + k * i' + l * m') (i * b' + j * f' + k * j' + l * n') (i * c' + j * g' + k * k' + l * o') (i * d' + j * h' + k * l' + l * p'))
--     (T4 (m * a' + n * e' + o * i' + p * m') (m * b' + n * f' + o * j' + p * n') (m * c' + n * g' + o * k' + p * o') (m * d' + n * h' + o * l' + p * p'))
--   )

-- -- Matrix-vector multiplication
-- matVecMul :: Exp Matrix4D -> Exp Vector3D -> Exp Vector4D
-- matVecMul
--   (T4
--     (T4 a b c d)
--     (T4 e f g h)
--     (T4 i j k l)
--     (T4 m n o p)
--   )

--   (T3 x y z)
--   = T4
--     (a * x + b * y + c * z + d * 1)
--     (e * x + f * y + g * z + h * 1)
--     (i * x + j * y + k * z + l * 1)
--     (m * x + n * y + o * z + p * 1)

-- -- Transform a 4D vector to a 2D point by stripping the homogeneos and z axes
-- to2D :: Exp (Float, Float, Float, Float) -> Point
-- to2D (T4 x y z one) = T2 (round x) (round y)


-- screenTriangle mat (T3 a b c) = T3 (to2D $ matVecMul mat a) (to2D $ matVecMul mat b) (to2D $ matVecMul mat c)

-- test4Acc = map (screenTriangle mat) (use $ fromList (Z:.(Prelude.length triangles)) triangles)
--   where triangles = [
--             ((-0.9,-1, -2), (1, 1, -2), (1, -1, -2)),
--             ((-1,1, -2), (0, 1, -3), (-0.9, 0, -2)),
--             ((0, 0, -7), (0.5, 0.5, -3), (0, 0.5, -2))
--           ]
--         mat = matMul (matMul vp orth) cam

-- -- see page 144
-- -- test4 :: Array (Z:. Int) [Char]
-- test4 = let minX = (abs $ the $ minimum $ map fst acc)
--             minY = (abs $ the $ minimum $ map snd acc)
--             maxY = 1 + (the $ maximum $ map snd acc) + minY
--             maxX = 1 + (the $ maximum $ map fst acc) + minX
--             def  = generate (I2 maxY maxX) (\_ -> 46)
--             p (I1 i) = Just_ $ (\(T2 a b) -> I2 (b + minY) (a + minX)) (acc !! i)
--             xs = generate (I1 $ length acc) (\_ -> 120) :: Acc (Vector Int)
--         in
--           Data.List.intercalate "\n" $ splitEvery (65) $ toList $ run $ map chr $ permute (max) def p xs
--     where acc = pointsOfTriangles test4Acc

-- splitEvery :: Int -> [a] -> [[a]]
-- splitEvery n = Prelude.takeWhile (Prelude.not.Prelude.null) . Prelude.map (Prelude.take n) . Prelude.iterate (Prelude.drop n)

-- screenTriangle' mat (T4 a b c col) = T2 (T3 (to2D $ matVecMul mat a) (to2D $ matVecMul mat b) (to2D $ matVecMul mat c)) col
-- screenTriangleZ mat (T4 a b c col) = 
--     let a' = matVecMul mat a
--         b' = matVecMul mat b
--         c' = matVecMul mat c
--         getZ (T4 _ _ z _) = z
--         maxZ = max (getZ a') $ max (getZ b') (getZ c')
--     in T2 (T3 (to2D a') (to2D b') (to2D $ c')) (T2 col maxZ)

-- ---------------
-- -- 3D settings:
-- ---------------

-- -- Size of the output
-- screenX = 1080.0
-- screenY = 1080.0

-- -- Rectangle of the view volume
-- left = -1.0
-- right = 1.0
-- top = 1
-- bottom = -1
-- far = 3.0
-- near = 1.0

-- -- Where to place the camera
-- eye :: Exp (Float, Float, Float)
-- eye  = T3 0 (1) 1

-- -- In what direction the camera will look
-- gaze :: Exp (Float, Float, Float)
-- gaze = T3 (-1) (-1) (-1)

-- -- What "up" is for the camera
-- up :: Exp (Float, Float, Float)
-- up   = T3 0 1 0


-- test6Acc :: Acc (Vector (TriangleT, (PixelRGB8, Float)))
-- test6Acc = map (screenTriangleZ mat) (use $ fromList (Z:.(Prelude.length triangles)) triangles)
--   -- where triangles = [
--   --           ((-0.9,-1, -2), (1, 1, -2), (1, -1, -2), PixelRGB8 255 0 0),
--   --           ((-0,-1, -1), (1, 1, -1), (1, -1, -1), PixelRGB8 0 255 0),
--   --           ((-1,1, -2), (0, 1, -3), (-0.9, 0, -2), PixelRGB8 0 255 0),
--   --           ((0, 0, -7), (0.5, 0.5, -3), (0, 0.5, -2), PixelRGB8 0 0 255)
--   --         ]
--   where triangles = [
--             ((-0.9, 0, 0.9), (0.9, 0, -0.9), (-0.9, 0, -0.9), PixelRGB8 255 0 0),
--             ((-0.9, 0, 0.9), (0.9, 0, -0.9), (0.9, 0, 0.9), PixelRGB8 0 255 0),
--             ((-0.9, 0, 0.9), (-0.9, 0, -0.9),   (-0.9, 0.9, -0.9), PixelRGB8 0 0 255),
--             ((-0.9, 0, 0.9), (-0.9, 0.9, -0.9),   (-0.9, 0.9, 0.9), PixelRGB8 255 0 255),
--             ((-0.9, 0, -0.9), (-0.9, 0.9, -0.9),   (0.9, 0, -0.9), PixelRGB8 255 255 255),
--             ((-0.9, 0.9, -0.9), (0.9, 0.9, -0.9),   (0.9, 0, -0.9), PixelRGB8 0 255 255)
--           ]
--         mat = matMul (matMul vp orth) cam

-- -- writePng "tst.png" $ imageOfArray $ run test6
-- test6 :: Acc (Array DIM2 PixelRGB8)
-- test6 = let minX = round $ screenX / 2.0 :: Exp Int
--             minY = round $ screenY / 2.0 :: Exp Int
--             maxY = round $ screenX :: Exp Int
--             maxX = round $ screenY :: Exp Int

--             def  = generate (I2 maxX maxY) (const $ T2 (PixelRGB8_ 0 0 0) (1000.0)) -- Vector (PixelRGB8, Float)

--             -- Note: In an array, the first coordinate is row, and the second column. Thus x and y swap!
--             p (I1 i) = (\(T2 a b) -> 
--                 if (maxY - b) > 0 && (maxY - b) < maxY && a < maxX && a > 0
--                   then Just_ $ I2 (maxY - b) (a)
--                   else Nothing_) (points !! i)
--             comb a@(T2 c1 z1) b@(T2 c2 z2) = if z1 < z2 then a else b
--             xs = map snd acc -- Acc (Vector (PixelRGB8, Float))
--         in
--           map fst $ permute comb def p xs
--     where acc = pointsOfTrianglesZ' test6Acc -- Acc (Vector (PointT, (PixelRGB8, Float)))
--           points = map fst acc -- Acc (Vector PointT)