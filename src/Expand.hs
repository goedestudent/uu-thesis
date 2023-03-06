{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RebindableSyntax     #-}

-- | Implementation of expand that uses the fewest computations
module Expand
    ( expand ) where

import Data.Array.Accelerate hiding (expand)
import qualified Prelude

-- Expand using fewest computations
expand
  :: (Elt e, Elt b) =>
     (Exp e -> Exp Int)
     -> (Exp e -> Exp Int -> Exp b)
     -> Acc (Vector e)
     -> Acc (Vector b)
expand sz get arr = 
    let szs           = map sz arr 
        (idxs, flags) = replIota szs
        iotas         = segmIota flags
    in zipWith (\i j -> get (arr !! i) j) idxs iotas

-- More optimised/shorter varients of the utility functions
replIota
  :: Acc (Vector Int) -> (Acc (Vector Int), Acc (Vector Bool))
replIota reps =
    let s1    = scanl (+) 0 reps      -- How many items before this one?
        fld   = s1 !! (length s1 - 1) -- How many items in total?
        tmp   = scatter s1 (fill (I1 fld) 0) (enumFromN (shape reps) 0) -- Index of item at first occurance
        flags = map (>0) tmp          -- Where the new segments start
    in  (segmScan (+) 0 flags tmp, flags)    -- Smear the first occurance to the next elements

-- The functions below are directly translated from the paper by Elsman et al.
segmScan
  :: (Lift Exp b, Elt b) =>
     (Exp b -> Exp b -> Exp b)
     -> Exp b
     -> Acc (Vector Bool)
     -> Acc (Vector b)
     -> Acc (Vector b)
segmScan op ne flags as = map snd scan
    where zp   = zip flags as
          scan = postscanl (\(T2 x_flag x) (T2 y_flag y) -> T2 ( x_flag || y_flag) (if (y_flag) then y else (x `op` y))) (T2 False_ ne) $ zp

segmIota :: Acc (Vector Bool) -> Acc (Vector Int)
segmIota flags = map (\x -> x-1) $ segmScan (+) 0 flags (fill (shape flags) 1)