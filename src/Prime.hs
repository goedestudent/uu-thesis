{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Prime(primes, primes', primesE, primesBenchmark, primesBenchmarkInefficient, primesBenchmarkBalanced) where 
  import Data.Array.Accelerate
  import Data.Array.Accelerate.AST.ExpandFusionStrategy
  import Prelude ()
  import qualified Prelude

  import Types
    
  primes :: Acc (Scalar Int) -> Acc (Vector Int)
  primes n' = afst loop
    where
      n     = the n'
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c
                      c2 = c1 <= limit ? ( c1*c1, n+1 )
                      m  = c2-c1
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` p
                            get p i = (2+i)*p
                        in
                        map (\x -> T2 (x >= 0 && x < m ? (Just_ (I1 x), Nothing_)) 0) $ map (subtract c1) (expand Basic sz get old)
                      --
                      new =
                        -- let m     = c2-c1
                        --     put i = let s = sieves ! i
                        --             in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        -- in
                        afst
                          $ filter (> 0)
                          $ permute' const (enumFromN (I1 m) c1) sieves
                   in
                   T2 (old ++ new) (unit c2))
                (T2 a0 c0)

  primesBenchmarkInefficient :: Expand Int Int -> Acc (Scalar Int) -> Acc (Vector Int)
  -- | Like 'primesBenchmarkInefficient', except that all sieves will be about the same size.
  primesBenchmarkInefficient expand n' = afst loop
    where
      n     = the n'
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c
                      c2 = c1 <= limit ? ( c1*c1, n+1 )
                      m  = c2-c1
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` 2
                            get p i = (2+i)*p
                        in
                        map (\x -> T2 (x >= 0 && x < m ? (Just_ (I1 x), Nothing_)) 0) $ map (subtract c1) (expand sz get old)
                      --
                      new =
                        -- let m     = c2-c1
                        --     put i = let s = sieves ! i
                        --             in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        -- in
                        afst
                          $ filter (> 0)
                          $ permute' const (enumFromN (I1 m) c1) sieves
                   in
                   T2 (old ++ new) (unit c2))
                (T2 a0 c0)

  primesBenchmark :: Expand Int Int -> Acc (Scalar Int) -> Acc (Vector Int)
  primesBenchmark expand n' = afst loop
    where
      n     = the n'
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c
                      c2 = c1 <= limit ? ( c1*c1, n+1 )
                      m  = c2-c1
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` p
                            get p i = (2+i)*p
                        in
                        map (\x -> T2 (x >= 0 && x < m ? (Just_ (I1 x), Nothing_)) 0) $ map (subtract c1) (expand sz get old)
                      --
                      new =
                        -- let m     = c2-c1
                        --     put i = let s = sieves ! i
                        --             in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        -- in
                        afst
                          $ filter (> 0)
                          $ permute' const (enumFromN (I1 m) c1) sieves
                   in
                   T2 (old ++ new) (unit c2))
                (T2 a0 c0)

  primesBenchmarkBalanced :: Expand Int Int -> Acc (Scalar Int) -> Acc (Vector Int)
  -- | The expansion sizes decrease. This kernel tries to account for this in the following way.
  -- The sizes are ordered like s_1 > s_2 > .. > s_n
  -- We want to therefore re-order elements to 1, n, 2, n-1, 3, n-2, ...
  -- This should mean that every two consecutive elements have about the same workload.
  primesBenchmarkBalanced expand n' = afst loop
    where
      n     = the n'
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      d (I1 i) = i
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c
                      c2 = c1 <= limit ? ( c1*c1, n+1 )
                      m  = c2-c1
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` p
                            get p i = (2+i)*p
                        in
                        map (\x -> T2 (x >= 0 && x < m ? (Just_ (I1 x), Nothing_)) 0) $ map (subtract c1) (expand sz get old)
                      --
                      new =
                        -- let m     = c2-c1
                        --     put i = let s = sieves ! i
                        --             in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        -- in
                        afst
                          $ filter (> 0)
                          $ permute' const (enumFromN (I1 m) c1) sieves
                      new' = backpermute (I1 m) (\(I1 i) -> ifThenElse (i `mod` 2 == 0) (I1 (i `quot` 2)) (I1 $ m - 1 - (i `quot` 2))) new
                      -- new' = permute const (fill (I1 m) 0) (\(I1 i) -> (Just_ $ I1 i)) new
                   in
                   T2 (old ++ new') (unit c2))
                (T2 a0 c0)

  primesE :: Exp Int -> Acc (Vector Int)
  primesE n = afst loop
    where
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c --Number we are currently evaluating
                      c2 = c1 <= limit ? ( c1*c1, n+1 ) -- Square of that number
                      m  = c2-c1 -- Factor can maximally be this nr
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` p -- Amount of factors we can make using already found primes
                            get p i = (2+i)*p           -- Factors we can find using already found primes
                        in
                        -- Expand:  Get all the factors
                        -- map -c1: Our enumFromN starts from c1, so offset it so we can use the numbers as indices
                        -- map ...: If the index from previous map is within bounds, we map 0 into the iota array, to flag that a certain number is a factor of another
                        map (\x -> T2 (x >= 0 && x < m ? (Just_ (I1 x), Nothing_)) 0) $ map (subtract c1) (expand' sz get old)
                      --
                      new =
                        let m     = c2-c1
                            -- put i = let s = sieves ! i
                            --          in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        in
                        afst
                          $ filter (> 0)
                          $ permute' (\a b -> a + 2*b - b - b ) (enumFromN (I1 m) c1) sieves
                   in
                   T2 (old ++ new) (unit c2))
                (T2 a0 c0)
  
  primes' :: Acc (Scalar Int) -> Acc (Vector Int)
  primes' n' = error "Check implementation"--afst loop
    where
      n     = the n'
      c0    = unit 2
      a0    = use $ fromList (Z:.0) []
      limit = truncate (sqrt (fromIntegral (n+1) :: Data.Array.Accelerate.Exp Float))
      loop  = awhile
                (\(T2 _   c) -> map (< n+1) c)
                (\(T2 old c) ->
                  let c1 = the c
                      c2 = c1 <= limit ? ( c1*c1, n+1 )
                      --
                      sieves =
                        let sz p    = (c2 - p) `quot` p
                            get p i = (2+i)*p
                        in
                        map (\x -> T2 (Just_ (I1 x)) 0) $ map (subtract c1) (expand' sz get old)
                      --
                      new =
                        let m     = c2-c1
                            -- put i = let s = sieves ! i
                            --          in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
                        in
                        afst
                          $ filter (> 0)
                          $ permute' min (enumFromN (I1 m) c1) sieves
                   in
                   T2 (old ++ new) (unit c2))
                (T2 a0 c0)