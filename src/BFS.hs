{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module BFS (bfs, bfsBenchmark, makeGraph, exampleGraph, sparsifyGraph) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate
import qualified Prelude
import Debug.Trace
import Types


-- | Returns whether the given element is Nothing_ or not
isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing Nothing_ = True_
isNothing _        = False_

-- | Alternative: Returns the first Just_ if one of the values is Just_, Nothing_ otherwise
(<|>) :: (Elt a, Ord a) => Exp (Maybe a) -> Exp (Maybe a) -> Exp (Maybe a)
(<|>) a Nothing_ = a
(<|>) (Just_ a) (Just_ b) = (Just_ (min a b))
(<|>) _        b = b

-- | Turns Just_ into its actual value. If a Nothing_ was given, returns undefined.
fromJust :: Elt a => Exp (Maybe a) -> Exp a
fromJust (Just_ v)  = v
fromJust (Nothing_) = Prelude.undefined

-- | Calculates the previous node on the path for every node. The path starts at
-- the given source. Is a single-source shortest path algorithm, where the distance
-- between nodes is always 1.
--
-- This assumes a directed graph.
--
-- Does not require the graph to be connected: if a path cannot be found the
-- value for the previous node will be returned as 'Nothing'.
--
-- Example of the usage:
-- >>> src = 0
-- >>> accGraph = makeGraph exampleGraph
-- >>> run $ bfs expand' src accGraph
-- Vector (Z :. 9) [Just 0,Just 0,Just 1,Just 1,Just 0,Just 2,Just 4,Just 6,Just 7]
bfs :: Expand Int (Maybe DIM1, Maybe Int) -> Exp Int -> (Exp Int, (Exp Int -> Exp Int), (Exp Int -> Exp Int -> Exp Int)) -> Acc (Vector (Maybe Int))
bfs expand src (totalNodes, numNeighbours, getNeighbour) = afst loop
  where
    -- Initial value for parents
    parentsI = generate (I1 totalNodes) (\(I1 i) -> cond (i == src) (Just_ src) Nothing_) :: Acc (Vector (Maybe Int))
    -- Initial value for oldParents, should be different from parentsI to satisfy the condition in the awhile
    parentsN = fill (I1 totalNodes) Nothing_ :: Acc (Vector (Maybe Int))
    loop = 
      awhile
        (\(T2 parents oldParents) -> map not $ and $ zipWith (==) parents oldParents)
        (\(T2 parents oldParents) ->
          let 
              known = afst $ compact 
                (map (not . (match isNothing)) parents) 
                (enumFromN (I1 totalNodes) 0) :: Acc (Vector (Int))
              -- For every node of which a parent is known, calculates a list of its neighbours and returns for every neighbour (neighbourId, self).
              -- The self value is then permuted into the array of parents, so that the the new parent becomes known.
              e = expand (match numNeighbours) 
                  (\elem i -> T2 (Just_ $ I1 $ (match getNeighbour) elem i) 
                  (Just_ elem)) known
              -- (neighbours, newParents) = unzip e
              -- perm (I1 i) = neighbours !! i
              p = permute' (match (<|>)) parents e
          in T2 p parents
        )
        (T2 parentsI parentsN)

bfsBenchmark:: Expand Int (Maybe DIM1, Maybe Int) -> Acc (Scalar Int) -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector (Maybe Int))
bfsBenchmark expand src' aSizes aNeighbours aOffsets = afst loop
  where
    src = the src'
    totalNodes = A.length aSizes

    numNeighbours i = aSizes !! i
    getNeighbour elem i = aNeighbours !! ((aOffsets !! elem) + i)

    -- Initial value for parents
    parentsI = generate (I1 totalNodes) (\(I1 i) -> cond (i == src) (Just_ src) Nothing_) :: Acc (Vector (Maybe Int))
    -- Initial value for oldParents, should be different from parentsI to satisfy the condition in the awhile
    parentsN = fill (I1 totalNodes) Nothing_ :: Acc (Vector (Maybe Int))
    loop = 
      awhile
        (\(T2 parents oldParents) -> map not $ and $ zipWith (==) parents oldParents)
        (\(T2 parents oldParents) ->
          let 
              known = afst $ compact 
                (map (not . (match isNothing)) parents) 
                (enumFromN (I1 totalNodes) 0) :: Acc (Vector (Int))
              -- For every node of which a parent is known, calculates a list of its neighbours and returns for every neighbour (neighbourId, self).
              -- The self value is then permuted into the array of parents, so that the the new parent becomes known.
              e = expand 
                  (match numNeighbours) 
                  (\elem i -> T2 (Just_ $ I1 $ (match getNeighbour) elem i) (Just_ elem))
                  known
              -- (neighbours, newParents) = unzip e
              -- perm (I1 i) = neighbours !! i
              p = permute' (match (<|>)) parents e
          in T2 p parents
        )
        (T2 parentsI parentsN)

-- | Example graph that can be used to test the BFS
exampleGraph :: [[Int]]
exampleGraph = [
    [1, 4],
    [0, 2, 3, 4],
    [1, 5],
    [1, 5],
    [0, 1, 6],
    [2, 3, 6],
    [4, 5, 7],
    [5, 6, 8],
    []
  ]

-- | Lift a Haskell list to an embedded Accelerate 'Vector'
useL :: Elt a => [a] -> Acc (Vector a)
useL xs = use $ toVector xs

-- | Lift a Haskell list to an Accelerate 'Vector'
toVector :: Elt a => [a] -> Vector a
toVector xs = fromList (Z:. (Prelude.length xs)) xs

-- | Given a list that contains lists of neighbouring nodes, returns a triple that can be used by the BFS algorithm.
-- Note that this makes a directed graph!
makeGraph :: [[Int]] -> ( Exp Int, Exp Int -> Exp Int, Exp Int -> Exp Int -> Exp Int)
makeGraph graph =
  let sizes       = Prelude.map Prelude.length graph :: [Int]
      offsets     = Prelude.scanl (+) 0 sizes :: [Int]
      neighbours  = Prelude.concat graph :: [Int]

      aSizes      = useL sizes
      aNeighbours = useL neighbours
      aOffsets    = useL offsets

      sz i = aSizes !! i
      get elem i = aNeighbours !! ((aOffsets !! elem) + i)

  in (lift $ Prelude.length graph, sz, get)

-- | Given a list that contains lists of neighbouring nodes, returns a triple that is the sparse representation of that list.
sparsifyGraph :: [[Int]] -> (Vector Int, Vector Int, Vector Int)
sparsifyGraph graph =
  let sizes       = Prelude.map Prelude.length graph :: [Int]
      offsets     = Prelude.scanl (+) 0 sizes :: [Int]
      neighbours  = Prelude.concat graph :: [Int]

      vSizes      = toVector sizes
      vNeighbours = toVector neighbours
      vOffsets    = toVector offsets

  in (vSizes, vNeighbours, vOffsets)