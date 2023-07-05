{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils (nestedListToExpandInput, genericKernel, genInputThatPermutesToNothing, genInputThatPermutesToRandom, genInputThatPermutesToUnique, genInputThatPermutesToRandomWithLikelyRange', genInputThatPermutesToRandomOrNothingWithLikelyRange, BasicInputGenerator, ExpandInput) where

    import Data.Array.Accelerate (Vector, Exp, DIM1, Acc, Elt, (:.)(..), Z(..), permute', fromList)
    import qualified Data.Array.Accelerate as A
    import qualified Data.Array.Accelerate.LLVM.PTX as PTX
    import Data.Array.Accelerate.System.Random.MWC as MWC
    import System.Random
    import Debug.Trace
    import System.IO.Unsafe
    import Data.Vector.Unboxed (singleton)

    import Prelude

    type ExpandInput = (Int, (Vector Int, Vector Int, Vector Int, Vector Int, Vector (Maybe DIM1, Int)))
    type ExpandI = (Exp Int -> Exp Int) -> (Exp Int -> Exp Int -> Exp (Maybe DIM1, Int)) -> Acc (Vector Int) -> Acc (Vector (Maybe DIM1, Int))

    type BasicInputGenerator = Int -> (Int, Int) -> ExpandInput

    -- | Like 'fromList' in Accelerate, but without the need to specify a shape as it is always makes a vector.
    fromList' :: Elt a => [a] -> Vector a
    fromList' xs = fromList (Z:.(length xs)) xs

    nestedListToExpandInput :: [[(Maybe DIM1, Int)]] -> ExpandInput
    nestedListToExpandInput xs = 
        let input       = fromList (Z:.length xs) [0..length xs]
            sizes       = map length xs
            totalSize   = sum sizes
            segments    = scanl (+) 0 sizes
            allElements = concat xs

            accSizes    = fromList' sizes
            accSegments = fromList' segments
            accElements = fromList' allElements

            defs = PTX.run $ A.fill (A.constant (Z:.totalSize)) 0--fromList (Z:.totalSize) [0 | _ <- [0..]]
        in (totalSize, (input, defs, accSizes, accSegments, accElements))

    listsToExpandInput :: Int -> [Int] -> Vector (Maybe DIM1, Int) -> ExpandInput
    listsToExpandInput inputSize sizes' allElements = 
        let input       = fromList (Z:.inputSize) [0..inputSize]
            sizes       = take inputSize sizes'
            totalSize   = sum sizes
            
            accSizes    = fromList' sizes
            accElements = allElements
            accSegments  = PTX.run $ A.scanl (+) 0 (A.use accSizes)

            -- Note that in the case for random, we will permute only 1000 indices.
            -- As this function is used only in the random case, we can safely assume we do not need more indices to permute into.
            defs = PTX.run $ A.fill (A.constant (Z:.min 1001 (totalSize + 1))) 0--fromList (Z:.totalSize) [0 | _ <- [0..]]
        in (totalSize, (input, defs, accSizes, accSegments, accElements))

    -- genericKernel :: ExpandI -> Int -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector (Maybe DIM1, Int)) -> Acc (Vector Int)
    -- genericKernel expand totalSize input sizes segments allElements = trace (P.show totalSize) $ use $ permute' const (fill (I1 (constant totalSize)) 0) $ expand sz get input
    --     where sz i      = sizes !! i
    --           get e i   = allElements !! ((segments !! e) + i)

    genericKernel :: ExpandI -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector (Maybe DIM1, Int)) -> Acc (Vector Int)
    genericKernel expand input defs sizes segments allElements = permute' const defs $ expand sz get input
        where sz i      = sizes A.!! i
              get e i   = allElements A.!! ((segments A.!! e) + i)

    -- genInput :: ((Int, Int) -> ((Int, Int) -> (Maybe DIM1, Int))) -> BasicInputGenerator
    -- genInput permutationTarget inputSize expandRange
    --     = nestedListToExpandInput [ getListForIndex i | i <- [0..inputSize-1]]
    --     where genSz = mkStdGen 42  -- Generator for sizes
    --           genId = mkStdGen 137 -- Generator for permutation indices
    --           sizes = randomRs expandRange genSz :: [Int]
    --           totalSize = P.sum $ P.take inputSize sizes
    --           prefixSum = P.scanl (+) 0 sizes :: [Int]
    --           elements = randomRs (0, totalSize) genId -- Infinite list of permutation targets to choose from

    --           getListForIndex i = let offset = prefixSum P.!! i
    --                                   size   = sizes P.!! i
    --                               in P.map (permutationTarget (i, offset)) $ P.zip [0..] (P.take size (P.drop offset elements))
    
    genInput :: ((Int -> Int -> (Maybe DIM1, Int))) -> BasicInputGenerator
    genInput permutationTarget inputSize expandRange
        = nestedListToExpandInput $ genNestedLists inputSize [] 0 sizes elements--[ getListForIndex i | i <- [0..inputSize-1]]
        where genSz = mkStdGen 42  -- Generator for sizes
              genId = mkStdGen 137 -- Generator for permutation indices
              sizes = randomRs expandRange genSz :: [Int]
              totalSize = sum $ take inputSize sizes
              prefixSum = scanl (+) 0 sizes :: [Int]
              elements = randomRs (0, totalSize) genId -- Infinite list of permutation targets to choose from

              genNestedLists 0 _ _ _ _ = []
              genNestedLists elementsLeft acc elementNr (0:sizes) elems = acc : genNestedLists (elementsLeft - 1) [] (elementNr + 1) sizes elems
              genNestedLists elementsLeft acc elementNr (s:sizes) (e:elems)  = genNestedLists elementsLeft ((permutationTarget elementNr e):acc) (elementNr + 1) (s-1 : sizes) elems

            --   getListForIndex i = let offset = prefixSum P.!! i
            --                           size   = sizes P.!! i
            --                       in P.map (permutationTarget (i, offset)) $ P.zip [0..] (P.take size (P.drop offset elements))

    genInputThatPermutesToNothing :: BasicInputGenerator
    genInputThatPermutesToNothing = genInput ((\_ x -> (Nothing, x)))

    genInputThatPermutesToRandom :: BasicInputGenerator
    genInputThatPermutesToRandom = genInput ((\_ x -> (Just (Z:.x), x)))

    genInputThatPermutesToUnique :: BasicInputGenerator
    genInputThatPermutesToUnique inputSize = genInput (\elementNr x -> ((Just (Z:. elementNr), elementNr))) inputSize

    -- ! Deprecated in favor of more general genInputThatPermutesToRandomWithLikelyRange' !
    --
    -- genInputThatPermutesToRandomWithLikelyRange :: Int -> (Int, Int) -> (Int, Int) -> ExpandInput
    -- genInputThatPermutesToRandomWithLikelyRange inputSize likelyExpandRange unlikelyExpandRange
    --     = trace ("Generating input for " ++ show inputSize) $ nestedListToExpandInput $ genNestedLists inputSize [] sizes elements--[ getListForIndex i | i <- [0..inputSize-1]]
    --     where genSz  = mkStdGen 16  -- Generator for sizes
    --           genSz2 = mkStdGen 17  -- Generator for sizes indices
    --           genSzL = mkStdGen 333 -- Generator for likeliness of whether to use genSz or genSz2
    --           genId  = mkStdGen 42 -- Generator for permutation indices
    --           sizes1 = randomRs likelyExpandRange genSz    :: [Int]
    --           sizes2 = randomRs unlikelyExpandRange genSz2 :: [Int]
    --           sizesL = randomRs (0, 1) genSzL              :: [Float]
    --           sizes  = map getCorrectSize $ zip sizesL (zip sizes1 sizes2)

    --           getCorrectSize (c, (l, u)) = if (c <= 0.9) then l else u

    --           totalSize = sum $ take inputSize sizes
    --           elements  = randomRs (0, min 1000 totalSize) genId -- Infinite list of permutation targets to choose from

    --         --   getListForIndex i = let offset = prefixSum P.!! i
    --         --                           size   = sizes P.!! i
    --         --                       in P.map (permutationTarget (i, offset)) $ P.zip [0..] (P.take size (P.drop offset elements))

    --           genNestedLists 0 _ _ _ = []
    --           genNestedLists elementsLeft acc (0:sizes) elems = acc : genNestedLists (elementsLeft - 1) [] sizes elems
    --           genNestedLists elementsLeft acc (s:sizes) (e:elems)  = genNestedLists elementsLeft ((permutationTarget e):acc) (s-1 : sizes) elems

    --           permutationTarget x = (Just (Z:.x), x)

    genInputThatPermutesToRandomWithLikelyRange' :: Int -> [(Float, Int, Int)] -> ExpandInput
    genInputThatPermutesToRandomWithLikelyRange' inputSize [] = error "Error, no ranges were given"
    genInputThatPermutesToRandomWithLikelyRange' inputSize rangesWithProbabilities
        = trace ("Generating input for " ++ show inputSize) $ listsToExpandInput inputSize sizes elements
        where 
              probabilities = scanl1 (+) [p | (p, _, _) <- rangesWithProbabilities]
              ranges        = [(l, u) | (_, l, u) <- rangesWithProbabilities]

              genSzs   = [mkStdGen (16 + i) | i <- [0..((length rangesWithProbabilities) - 1)]]
              genSzL   = mkStdGen 333 -- Generator for probability that size should come from range i
            --   genId    = mkStdGen 42  -- Generator for permutation indices
              !genId    =  unsafePerformIO $ (MWC.initialize $ singleton 42) :: GenIO -- Generator for permutation indices

              sizesAll = zipWith randomRs ranges genSzs :: [[Int]]
              sizesL   = randomRs (0, 1) genSzL         :: [Float]
              sizes    = getCorrectSize sizesL sizesAll

              getCorrectSize [] _ = []
              getCorrectSize (p:sizesL) sizesAll = 
                  let sizesIndex = getRangeForProbability p 0
                  in (head $ sizesAll !! sizesIndex) : getCorrectSize sizesL (map tail sizesAll)

              getRangeForProbability p i = 
                  if i < length probabilities
                  then 
                      if p <= probabilities !! i
                      then i
                      else getRangeForProbability p (i + 1)
                  -- We could not find it in any list. Thus we just take the value from the last array
                  else trace ("Could not find probability " ++ show p) ((length probabilities) - 1)

            --   getCorrectSize :: Int -> (Float, Int) -> Int
            --   getCorrectSize i (p, sizeIndex) = 
            --       if  i P.< P.length probabilities
            --       then 
            --           if p P.< probabilities P.!! i 
            --           then (sizesAll P.!! i) P.!! sizeIndex
            --           else getCorrectSize (i+1) (p, sizeIndex) 
            --        else ((sizesAll P.!! (P.length probabilities - 1)) P.!! sizeIndex)

              totalSize = sum $ take inputSize sizes
              kernel = PTX.run1 $ A.map permutationTarget'
              !elements  = kernel $ unsafePerformIO $ MWC.randomArrayWith genId (MWC.uniformR (0, min 1000 totalSize)) (Z:.totalSize)
            --   elements  = randomRs (0, min 1000 totalSize) genId -- Infinite list of permutation targets to choose from

            --   getListForIndex i = let offset = prefixSum P.!! i
            --                           size   = sizes P.!! i
            --                       in P.map (permutationTarget (i, offset)) $ P.zip [0..] (P.take size (P.drop offset elements))

              genNestedLists 0 _ _ _ = []
              genNestedLists elementsLeft acc (0:sizes) elems = acc : genNestedLists (elementsLeft - 1) [] sizes elems
              genNestedLists elementsLeft acc (s:sizes) (e:elems) = genNestedLists elementsLeft ((permutationTarget e):acc) (s-1 : sizes) elems

              permutationTarget x = (Just (Z:.x), x)
              permutationTarget' x = A.T2 (A.Just_ (A.I1 x))  x


    genInputThatPermutesToRandomOrNothingWithLikelyRange :: Int -> [(Float, Int, Int)] -> Float -> ExpandInput
    genInputThatPermutesToRandomOrNothingWithLikelyRange inputSize [] _ = error "Error, no ranges were given"
    genInputThatPermutesToRandomOrNothingWithLikelyRange inputSize rangesWithProbabilities probabilityNothing
        = trace ("Generating input for " ++ show inputSize) $ listsToExpandInput inputSize sizes elements -- nestedListToExpandInput $ genNestedLists inputSize [] sizes elements idsNorRs
        where 
              probabilities = scanl1 (+) [p | (p, _, _) <- rangesWithProbabilities]
              ranges        = [(l, u) | (_, l, u) <- rangesWithProbabilities]

              genSzs   = [mkStdGen (16 + i) | i <- [0..((length rangesWithProbabilities) - 1)]]
              genSzL   = mkStdGen 333 -- Generator for probability that size should come from range i
            --   genId    = mkStdGen 42  -- Generator for permutation indices
              !genId    = unsafePerformIO $ (MWC.initialize $ singleton 42) :: GenIO -- Generator for permutation indices
              genIdN   = mkStdGen 803 -- Generator for probability that permutation index should be 'Nothing'

              sizesAll = zipWith randomRs ranges genSzs :: [[Int]]
              sizesL   = randomRs (0, 1) genSzL         :: [Float]
              idsNorRs = randomRs (0, 1) genIdN         :: [Float]
              sizes    = getCorrectSize sizesL sizesAll

              getCorrectSize [] _ = []
              getCorrectSize (p:sizesL) sizesAll = 
                  let sizesIndex =getRangeForProbability p 0
                  in (head $ sizesAll !! sizesIndex) : getCorrectSize sizesL (map tail sizesAll)

              getRangeForProbability p i = 
                  if i < length probabilities
                  then 
                      if p <= probabilities !! i
                      then i
                      else getRangeForProbability p (i + 1)
                  -- We could not find it in any list. Thus we just take the value from the last array
                  else trace ("Could not find probability " ++ show p) ((length probabilities) - 1)

            --   getCorrectSize :: Int -> (Float, Int) -> Int
            --   getCorrectSize i (p, sizeIndex) = 
            --       if  i P.< P.length probabilities
            --       then 
            --           if p P.< probabilities P.!! i 
            --           then (sizesAll P.!! i) P.!! sizeIndex
            --           else getCorrectSize (i+1) (p, sizeIndex) 
            --        else ((sizesAll P.!! (P.length probabilities - 1)) P.!! sizeIndex)

              totalSize = sum $ take inputSize sizes
              kernel = PTX.run1 $ A.zipWith permutationTarget' accIdsNorRs
              !accIdsNorRs = A.use $ A.fromList (Z:.totalSize) idsNorRs
              !elements  = kernel $ unsafePerformIO $ MWC.randomArrayWith genId (MWC.uniformR (0, min 1000 totalSize)) (Z:.totalSize)

            --   getListForIndex i = let offset = prefixSum P.!! i
            --                           size   = sizes P.!! i
            --                       in P.map (permutationTarget (i, offset)) $ P.zip [0..] (P.take size (P.drop offset elements))

              genNestedLists 0 _ _ _ _ = []
              genNestedLists elementsLeft acc (0:sizes) elems idNorRs = acc : genNestedLists (elementsLeft - 1) [] sizes elems idNorRs
              genNestedLists elementsLeft acc (s:sizes) (e:elems) (idNorR:idNorRs)  = genNestedLists elementsLeft ((permutationTarget idNorR e):acc) (s-1 : sizes) elems idNorRs

              permutationTarget idNorR x =
                  if    idNorR < probabilityNothing 
                  then (Nothing, x) 
                  else (Just (Z:.x), x)
              permutationTarget' idNorR x = A.ifThenElse (idNorR A.< A.lift probabilityNothing) (A.T2 A.Nothing_ x) (A.T2 (A.Just_ (A.I1 x)) x)
