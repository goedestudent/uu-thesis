
-- | This module contains the Expand, implemented using the segmented scan from accelerate
module Expand.Accelerate
    ( expand ) where

-----------------------
-- Utility functions --
-----------------------

-- Accelerate-based variants of the utility functions. They use the Accelerate segmented scan
replIota
  :: Acc (Array (DIM0 :. Int) Int) -> Acc (Array (DIM0 :. Int) Int)
replIota reps =
    let s1 = postscanl (+) 0 reps
        s2 = map (\i -> ifThenElse (i == 0) 0 (s1 !! (i - 1))) (generate (shape reps) (\(I1 i) -> i))
        fld = (the $ fold (+) 0 reps)
        tmp = scatter s2 (generate (I1 fld) (const 0)) (generate (shape reps) (\(I1 i) -> i))
    in  postscanlSeg (+) 0 tmp reps

segmIota
  :: Acc (Segments Int) -> Acc (Vector Int)
segmIota segments = 
    let total = the $ fold (+) 0 segments
    in map (\x -> x-1) $ 
      postscanlSeg (+) 0 (generate (I1 total) (const 1)) segments

-- Expand using Accelerate segmented scan
expand
  :: (Elt e, Elt b) =>
     (Exp e -> Exp Int)
     -> (Exp e -> Exp Int -> Exp b)
     -> Acc (Vector e)
     -> Acc (Vector b)
expand sz get arr = 
    let szs   = map sz arr 
        idxs  = replIota szs
        iotas = segmIota szs
    in zipWith (\i j -> get (arr !! i) j) idxs iotas