{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE FlexibleContexts     #-}

-- | Module that renders the 3D triangles
module ThreeD (render, renderableAcc, fromFile, orthographic, perspective, paintToCanvas) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.PTX (run)
import qualified Prelude
import Prelude (IO)
import Expand as Lib
import Triangles
import LinAlg
import ObjParser

import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture.Types
import Codec.Picture (convertRGB8, readImage, writePng)

---------------
-- 3D settings:
---------------

-- Size of the output
screenX = 1080.0
screenY = 1080.0

-- Rectangle of the view volume
left = -5.0
right = 5.0
top = 5
bottom = -5
far = 8.0
near = -5.0

-- | Where to place the camera
eye :: Exp Vector3D
eye  = T3 4 (8) 5

-- | In what direction the camera will look
gaze :: Exp Vector3D
gaze = T3 (-1) (-1) (-1)

-- | What "up" is for the camera
up :: Exp Vector3D
up   = T3 0 1 0

-- | Where the lightsource should be located
light :: Exp Vector3D
light = T3 0 10 10


--------------------
-- Utility functions
--------------------

-- | Given a list of 2D lines (with their colour and z-index), returns a list of points that make up that 2D line (with their colour and z-index)
pointsOfLines :: Acc (Vector (LineT, (PixelRGB8, Float))) -> Acc (Vector (PointT, (PixelRGB8, Float)))
pointsOfLines = Lib.expand (\(T2 l _) -> pointsInLine l) (\(T2 l (T2 c z)) i -> T2 (getPointInLine l i) (T2 c z) )

-- | Given a list of 2D triangles (with their colour and z-index), returns a list of lines that make up that 2D triangle (with their colour and z-index)
linesOfTriangles :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> Acc (Vector (LineT, (PixelRGB8, Float)))
linesOfTriangles = Lib.expand (\(T2 t _) -> linesInTriangle t) (\(T2 t (T2 c z)) i -> T2 (getLineInTriangle t i) (T2 c z)) . map (\(T2 t (T2 c z)) -> T2 (normalise t) (T2 c z))

-- | Given a list of 2D triangles (with their colour and z-index), returns a list of points that make up that 2D triangle (with their colour and z-index)
pointsOfTriangles :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> Acc (Vector (PointT, (PixelRGB8, Float)))
pointsOfTriangles = ThreeD.pointsOfLines . ThreeD.linesOfTriangles

-- | Multiplies a color by a given float
(***) :: Exp PixelRGB8 -> Exp Float -> Exp PixelRGB8
(***) (PixelRGB8_ a b c) n = 
  let fa = fromIntegral a :: Exp Float 
      fb = fromIntegral b :: Exp Float
      fc = fromIntegral c :: Exp Float
  in PixelRGB8_ (round $ fa * n) (round $ fb * n) (round $ fc * n)

-- | When a model does not have its own normals, this utility calculates them using the vertices in the face
ensureNormals :: Exp (Triangle3DS, Maybe Triangle3D) -> Exp (Triangle3DS, Triangle3D)
ensureNormals (T2 triangle (Just_ normals)) = T2 triangle normals
ensureNormals (T2 t@(T4 a b c _) _)         = T2 t normals
  where aToB = b `sub3` a
        aToC = c `sub3` a
        cross = normalise3 $ cross3 aToB aToC
        normals = T3 cross cross cross

-- | Transform a 4D vector to a 2D point by stripping the homogeneos and z axes
to2D :: Exp (Float, Float, Float, Float) -> Point
to2D (T4 x y z w) = T2 (round (x / w)) (round (y / w))

-- | Projects the given triangle to the screen, using the given projection matrix
screenTriangle :: Exp Matrix4D -> Exp (Triangle3DS, Triangle3D) -> Exp (TriangleT, (PixelRGB8, Float))
screenTriangle mat (T2 triangle@(T4 a b c col) (T3 na nb nc)) = 
    let a' = matVecMul mat a
        b' = matVecMul mat b
        c' = matVecMul mat c

        -- Code for shading
        normal = normalise3 (na `add3` nb `add3` nc)  -- Avg vertex normal == face normal
        center = mul3 (1/3) (a `add3` b `add3` c) -- Barycenter of triangle
        l      = normalise3 $ light `sub3` center
        nDotL  = normal `dot3` l

        getZ (T4 _ _ z _) = z
        maxZ = max (getZ a') $ max (getZ b') (getZ c')
        -- col' (T3 x y z) = PixelRGB8_ (round (x * 255)) (round (y * 255)) (round (z * 255)) -- Turns a vector into a PixelRGB8 color
    in T2 (T3 (to2D a') (to2D b') (to2D $ c')) (T2 (col *** nDotL) maxZ)

----------------------
-- Calculation of the
-- projection matrices
----------------------

vecU :: Exp Vector3D
vecU = mul3 (norm3 crossed) crossed
  where crossed = cross3 up vecW

vecV :: Exp Vector3D
vecV = cross3 vecW vecU

vecW :: Exp Vector3D
vecW = mul3 (-1 / norm3 gaze ) gaze

-- Camera coordinate transformation matrix
cam :: Exp Matrix4D
cam = matMul mat1 mat2
  where thd (T3 _ _ z) = z
        snd (T3 _ y _) = y
        fst (T3 x _ _) = x

        mat1 = T4 
                (T4 (fst vecU) (snd vecU) (thd vecU) 0)
                (T4 (fst vecV) (snd vecV) (thd vecV) 0)
                (T4 (fst vecW) (snd vecW) (thd vecW) 0)
                (T4 0          0          0          1)
        mat2 = T4 
                (T4 1 0 0 ((-1) * fst eye))
                (T4 0 1 0 ((-1) * snd eye))
                (T4 0 0 1 ((-1) * thd eye))
                (T4 0 0 0 1               )

-- Orthographic projection matrix
orth :: Exp Matrix4D
orth = T4 
  (T4 (2.0 / (right - left)) 0.0 0.0 (-((right + left) / (right - left))))
  (T4 0.0 (2.0 / (top - bottom)) 0.0 (-((top + bottom) / (top - bottom))))
  (T4 0.0 0.0 (2.0 / (near - far)) (-((near + far) / (near - far))))
  (T4 0.0 0.0 0.0 1.0)

-- Viewport matrix
vp :: Exp Matrix4D
vp = T4 
  (T4 (screenX / 2.0) 0.0 0.0 ((screenX - 1.0) / 2.0))
  (T4 0.0 (screenY / 2.0) 0.0 ((screenY - 1.0) / 2.0))
  (T4 0.0 0.0 1.0 0.0)
  (T4 0.0 0.0 0.0 1.0)

-- Perspective matrix
per :: Exp Matrix4D
per = T4
  (T4 near 0 0 0)
  (T4 0 near 0 0)
  (T4 0 0 (near + far) (-near * far))
  (T4 0 0 1 0)

----------------------
-- Rendering functions
----------------------

-- | Accelerate computation that returns triangles, with corresponding color and z0index
renderableAcc :: Acc (Vector (Triangle3DS, Triangle3D))
renderableAcc = map (match ensureNormals) $ use $ fromList (Z:.(Prelude.length triangles)) trianglesWithNormals
  where triangles = [
            ((-0.9, 0, 0.9), (0.9, 0, -0.9), (-0.9, 0, -0.9), PixelRGB8 255 0 0),
            ((-0.9, 0, 0.9), (0.9, 0, -0.9), (0.9, 0, 0.9), PixelRGB8 0 255 0),
            ((-0.9, 0, 0.9), (-0.9, 0, -0.9),   (-0.9, 0.9, -0.9), PixelRGB8 0 0 255),
            ((-0.9, 0, 0.9), (-0.9, 0.9, -0.9),   (-0.9, 0.9, 0.9), PixelRGB8 255 0 255),
            ((-0.9, 0, -0.9), (-0.9, 0.9, -0.9),   (0.9, 0, -0.9), PixelRGB8 255 255 255),
            ((-0.9, 0.9, -0.9), (0.9, 0.9, -0.9),   (0.9, 0, -0.9), PixelRGB8 0 255 255)
          ]
        trianglesWithNormals = Prelude.map (\t -> (t, Nothing)) triangles

-- | Loads a list of 3D shaded triangles from the given .obj file
fromFile :: Prelude.String -> Acc (Vector (Triangle3DS, Triangle3D))
fromFile path = map (match ensureNormals) $ use $ objToTriangles path

-- | Project a list of 3D triangles onto the set screen using perspective projection
perspective :: Acc (Vector (Triangle3DS, Triangle3D)) -> Acc (Vector (TriangleT, (PixelRGB8, Float)))
perspective triangles = map (screenTriangle mat) triangles
  where -- View transformation matrix
        mat = matMul (matMul vp (matMul orth per)) cam

-- | Project a list of 3D triangles onto the set screen using ortographic projection
orthographic :: Acc (Vector (Triangle3DS, Triangle3D)) -> Acc (Vector (TriangleT, (PixelRGB8, Float)))
orthographic triangles = map (screenTriangle mat) triangles
  where -- View transformation matrix
        mat = matMul (matMul vp orth) cam

-- | Renders the input triangles to a canvas, taking the z-index of the triangles into account
paintToCanvas :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> Acc (Array DIM2 PixelRGB8)
paintToCanvas accIn = 
  let maxY = round $ screenX :: Exp Int
      maxX = round $ screenY :: Exp Int

      def  = generate (I2 maxX maxY) (const $ T2 (PixelRGB8_ 0 0 0) (1000.0)) -- Vector (PixelRGB8, Float)

      -- Note: In an array, the first coordinate is row, and the second column. Thus x and y swap!
      p (I1 i) = (\(T2 a b) -> 
          if (maxY - b) > 0 && (maxY - b) < maxY && a < maxX && a > 0
            then Just_ $ I2 (maxY - b) (a)
            else Nothing_) (points !! i)
      comb a@(T2 c1 z1) b@(T2 c2 z2) = if z1 < z2 then a else b
      xs = map snd acc -- Acc (Vector (PixelRGB8, Float))
  in
      map fst $ permute comb def p xs

  where acc = ThreeD.pointsOfTriangles accIn -- Acc (Vector (PointT, (PixelRGB8, Float)))
        points = map fst acc -- Acc (Vector PointT)

-- | Renders the input triangles to a canvas, and saves them to the file "test.png"
render :: Acc (Vector (TriangleT, (PixelRGB8, Float))) -> IO ()
render accIn = writePng "test.png" $ imageOfArray $ run $ paintToCanvas accIn