-- | Some linear algebra operations in Accelerate
module LinAlg where

import Data.Array.Accelerate

type Matrix4D = ((Float, Float, Float, Float), (Float, Float, Float, Float), (Float, Float, Float, Float), (Float, Float, Float, Float))
type Vector4D = (Float, Float, Float, Float)
type Vector3D = (Float, Float, Float)

-- | Multiplies a 3D vector by a scalar
mul3 :: Exp Float -> Exp Vector3D -> Exp Vector3D
mul3 s (T3 x y z) = T3 (x * s) (y * s) (z * s)

-- | Returns the norm/length of a 3D vector
norm3 :: Exp Vector3D -> Exp Float
norm3 (T3 x y z) = sqrt $ (x*x) + (y*y) + (z*z)

-- | Returns the cross product of two 3D vectors
cross3 :: Exp Vector3D -> Exp Vector3D -> Exp Vector3D
cross3 (T3 ax ay az) (T3 bx by bz) = T3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

-- | Adds up the two vectors
add3 :: Exp Vector3D -> Exp Vector3D -> Exp Vector3D
add3 (T3 ax ay az) (T3 bx by bz) = T3 (ax + bx) (ay + by) (az + bz)

-- | Subtracts vector b from vector a
sub3 :: Exp Vector3D -- ^ Vector a
     -> Exp Vector3D -- ^ Vector b
     -> Exp Vector3D
sub3 (T3 ax ay az) (T3 bx by bz) = T3 (ax - bx) (ay - by) (az - bz)

-- | Returns the normalised version of the given vector
normalise3 :: Exp Vector3D -> Exp Vector3D
normalise3 v = mul3 (1 / norm3 v) v

-- | Returns the dot product between the two vectors
dot3 :: Exp Vector3D -> Exp Vector3D -> Exp Float
dot3 (T3 ax ay az) (T3 bx by bz) = (ax * bx) + (ay * by) + (az * bz)

-- | Matrix multiplication for square matrices
matMul :: Exp Matrix4D -> Exp Matrix4D -> Exp Matrix4D
matMul
  (T4
    (T4 a b c d)
    (T4 e f g h)
    (T4 i j k l)
    (T4 m n o p)
  )

  (T4
    (T4 a' b' c' d')
    (T4 e' f' g' h')
    (T4 i' j' k' l')
    (T4 m' n' o' p')
  )
  = (T4
    (T4 (a * a' + b * e' + c * i' + d * m') (a * b' + b * f' + c * j' + d * n') (a * c' + b * g' + c * k' + d * o') (a * d' + b * h' + c * l' + d * p'))
    (T4 (e * a' + f * e' + g * i' + h * m') (e * b' + f * f' + g * j' + h * n') (e * c' + f * g' + g * k' + h * o') (e * d' + f * h' + g * l' + h * p'))
    (T4 (i * a' + j * e' + k * i' + l * m') (i * b' + j * f' + k * j' + l * n') (i * c' + j * g' + k * k' + l * o') (i * d' + j * h' + k * l' + l * p'))
    (T4 (m * a' + n * e' + o * i' + p * m') (m * b' + n * f' + o * j' + p * n') (m * c' + n * g' + o * k' + p * o') (m * d' + n * h' + o * l' + p * p'))
  )

-- | Matrix-vector multiplication
matVecMul :: Exp Matrix4D -> Exp Vector3D -> Exp Vector4D
matVecMul
  (T4
    (T4 a b c d)
    (T4 e f g h)
    (T4 i j k l)
    (T4 m n o p)
  )

  (T3 x y z)
  = T4
    (a * x + b * y + c * z + d * 1)
    (e * x + f * y + g * z + h * 1)
    (i * x + j * y + k * z + l * 1)
    (m * x + n * y + o * z + p * 1)