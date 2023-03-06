-- | Implementation of Expand, as a direct translation from the paper by Elsman et al.
module Expand.Futhark
    ( expand ) where

-----------------------
-- Utility functions --
-----------------------

-- | Moves elements n indices to the left. That is b[x] = a[x+n] for `rotate n a`.
-- >>> run $ rotate (-1) (use $ fromList (Z:.10) [0..])
--     Vector (Z :. 10) [9.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0]
rotate
  :: (Elt e, Prelude.Num (Exp e)) =>
     Exp Int -> Acc (Vector e) -> Acc (Vector e)
-- Note: the commented one uses scatter, thus allows for less fusion
-- rotate n a = scatter (generate (shape a) ((\(I1 i) -> (abs (i - n)) `rem` l))) (generate (shape a) (\(I1 i) -> -1)) a
rotate n a = backpermute (shape a) (\(I1 i) -> I1 $ (abs (i + n)) `rem` l) a
    where l = length a
          abs x = ifThenElse (x < 0) (l + x) x

-- The following 3 functions are directly translated from the paper by Elsman et al. They therefore
-- use the custom provided segmented scan.
segm_scan
  :: (Lift Exp b, Elt b) =>
     (Exp b -> Exp b -> Exp b)
     -> Exp b
     -> Acc (Vector Bool)
     -> Acc (Vector b)
     -> Acc (Vector b)
segm_scan op ne flags as = map snd scan
    where zp   = zip flags as
          scan = postscanl (\(T2 x_flag x) (T2 y_flag y) -> T2 ( x_flag || y_flag) (if (y_flag) then y else (x `op` y))) (T2 False_ ne) $ zp

repl_iota
  :: Acc (Array (DIM0 :. Int) Int) -> Acc (Array (DIM0 :. Int) Int)
repl_iota reps =
    let s1 = postscanl (+) 0 reps
        s2 = map (\i -> ifThenElse (i == 0) 0 (s1 !! (i - 1))) (generate (shape reps) (\(I1 i) -> i))
        fld = (the $ fold (+) 0 reps)
        tmp = scatter s2 (generate (I1 fld) (const 0)) (generate (shape reps) (\(I1 i) -> i))
        flags = map (>0) tmp
    in segm_scan (+) 0 flags tmp

segm_iota :: Acc (Vector Bool) -> Acc (Vector Int)
segm_iota flags = map (\x -> x-1) $ segm_scan (+) 0 flags (fill (shape flags) 1)

-- This is zipWith in Accelerate
map2
  :: (Shape sh, Elt x0, Elt x1, Elt b) =>
     (Exp x0 -> Exp x1 -> Exp b)
     -> Acc (Array sh x0) -> Acc (Array sh x1) -> Acc (Array sh b)
map2 f a b = map (\(T2 e1 e2) -> f e1 e2) (zip a b)

-- Expand as defined in the paper
expand
  :: (Elt e, Elt b) =>
     (Exp e -> Exp Int)
     -> (Exp e -> Exp Int -> Exp b)
     -> Acc (Vector e)
     -> Acc (Vector b)
expand sz get arr = 
    let szs   = map sz arr 
        idxs  = repl_iota szs
        iotas = segm_iota $
                  zipWith 
                    (\a b -> not (a == b)) 
                    idxs 
                    (rotate (-1) idxs)
    in zipWith (\i j -> get (arr !! i) j) idxs iotas