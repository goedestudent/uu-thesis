module ObjParser (objToTriangles, Triangle3DS, Triangle3D) where

import Data.Array.Accelerate (Vector, fromList, (:.)(..), Z(..))
import LinAlg
import Triangles
import System.IO.Unsafe (unsafePerformIO)

import Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture
import Data.Array.Accelerate.IO.Codec.Picture.Types

type Triangle3D = (Vector3D, Vector3D, Vector3D)
type Triangle3DS = (Vector3D, Vector3D, Vector3D, PixelRGB8)

-- | Given a filepath to a .obj file, returns the shaded triangles in that file
objToTriangles :: String -> Vector (Triangle3DS, Maybe Triangle3D)
objToTriangles path = fromList (Z:. (length shadedTriangles)) shadedTriangles
    where lines     = readLines path
          vertices  = findVertices lines
          normals   = findNormals lines
          (triangles, tNormals) = unzip $ findTriangles lines vertices normals
          shadedTriangles = zip (shadeTriangles (getBoundingBox triangles ((0,0,0), (0,0,0))) triangles) tNormals

-- | Returns all lines in the given file
readLines :: String -> [String]
readLines path = unsafePerformIO $ 
    do
        content <- readFile path
        return $ lines content

-- | Calculates both the minimum x, y and z coordinates and the maximum x, y and z coordinates and
-- returns them as a pair of vectors (min, max)
getBoundingBox :: [Triangle3D] -> (Vector3D, Vector3D) -> (Vector3D, Vector3D)
getBoundingBox [] bounds = bounds
getBoundingBox ((a, b, c):xs) (boundMin, boundMax) = getBoundingBox xs (minV boundMin $ minV a $ minV b c, maxV boundMax $ maxV a $ maxV b c)
    where minV (x, y, z) (x', y', z') = (min x x', min y y', min z z')
          maxV (x, y, z) (x', y', z') = (max x x', max y y', max z z')

-- | Given the bounding box and a list of triangles, returns the shaded version of the given triangles
shadeTriangles :: (Vector3D, Vector3D) -> [Triangle3D] -> [Triangle3DS]
shadeTriangles _ [] = []
shadeTriangles bounds@((minX, minY, minZ), (maxX, maxY, maxZ)) ((a@(x,y,z), b, c):xs) = (a,b,c, PixelRGB8 255 255 255) : shadeTriangles bounds xs
    where totalY = maxY - minY
          shadeY = round $ ((y - minY) / totalY) * 255

-- | Given a list of strings, representing the lines of a .obj file, returns all vectors in that file
findVertices :: [String] -> [Vector3D]
findVertices [] = []
findVertices (('v' : ' ' :x):xs) = parseVector (words x) : findVertices xs
findVertices (_:xs) = findVertices xs

-- | Given a list of strings, representing the lines of a .obj file, returns all normals in that file
findNormals :: [String] -> [Vector3D]
findNormals [] = []
findNormals (('v' : 'n' : ' ' :x):xs) = parseVector (words x) : findNormals xs
findNormals (_:xs) = findNormals xs

-- | Given a list of strings, representing the lines of a .obj file, returns all faces in that file
findTriangles :: [String] -> [Vector3D] -> [Vector3D] -> [(Triangle3D, Maybe Triangle3D)]
findTriangles []                  _  _  = []
findTriangles (('f' : ' ' :x):xs) vs vns = parseFace vs vns (words x) : findTriangles xs vs vns
findTriangles (_:xs)              vs vns = findTriangles xs vs vns

-- | Parses a line that represents a vector
parseVector :: [String] -> Vector3D
parseVector (x : y : z : []) = (read x, read y, read z)

-- | Parses a line that represents a face
parseFace :: [Vector3D] -> [Vector3D] -> [String] -> (Triangle3D, Maybe Triangle3D)
parseFace vs vns (x : y : z : []) = ((vs !! getVertexId x, vs !! getVertexId y, vs !! getVertexId z), normals)
    where getVertexId val = (read (fst $ until '/' val )) - 1 :: Int -- Subtract 1, because the vertex index is the offset in the list, starting with 1
          getNormalId val = (read (snd $ until '/' $ snd $ until '/' val )) - 1 :: Int -- Subtract 1, because the normal index is the offset in the list, starting with 1

          -- If there are no normals, we return nothing, else we return a triangle, where the vertices are the vertex normals
          normals = if length vns == 0 then Nothing else Just (vns !! getNormalId x, vns !! getNormalId y, vns !! getNormalId z)

          until c (x:xs)
            | x == c    = ([], xs)
            | otherwise = let (val, rest) = until c xs in (x : val, rest)
          until _ [] = ([], [])