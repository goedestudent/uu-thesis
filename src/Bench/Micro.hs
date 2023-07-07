{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}

module Bench.Micro where
    import Data.Array.Accelerate as A
    import Data.Array.Accelerate.LLVM.PTX (run1, runN)
    import Criterion.Main
    import Criterion.Types
    import Types
    import Utils

    import Prelude as P
    import System.Random
    import System.IO.Unsafe
    import Debug.Trace

    -- type Expand = forall a b . (Elt a, Elt b) => (Exp a -> Exp Int) -> (Exp a -> Exp Int -> Exp b) -> Acc (Vector a) -> Acc (Vector b)
    type ExpandI = (Exp Int -> Exp Int) -> (Exp Int -> Exp Int -> Exp (Maybe DIM1, Int)) -> Acc (Vector Int) -> Acc (Vector (Maybe DIM1, Int))

    inputSizes = [1, 10, 100, 1000, 10000, 100000, 200000]--, 200000] --50000]
    expansionSizeRange = (0, 100)

    -- toIndexBase :: BasicInputGenerator -> String -> ExpandI -> Benchmark
    -- toIndexBase genInput benchmarkName expand = bgroup benchmarkName [ bench (show size)  $ nf f xs  | (size, f, xs) <- curriedKernelsAndInputs]
    --     where !inputs = [ (size, genInput size expansionSizeRange) | size <- sizes ]
    --           !kernelsAndInputs = [ (size, runN (genericKernel expand totalSize), input) | (size, (totalSize, input)) <- inputs ]
    --           !curriedKernelsAndInputs = [ (size, curry kernel, input) | (size, kernel, input) <- kernelsAndInputs ]
    --           sizes = inputSizes
    --           curry kernel (a, b, c, d) = kernel a b c d
    toIndexBase :: BasicInputGenerator -> String -> (String, [(Int, ExpandInput)])
    toIndexBase genInput benchmarkName = (benchmarkName, inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInput size expansionSizeRange) | size <- sizes ]
              sizes = inputSizes

    -- toIndexNothing :: (String, [(Int, ExpandInput)])
    -- toIndexNothing = toIndexBase genInputThatPermutesToNothing "permute to N with sz "

    -- toIndexRand :: (String, [(Int, ExpandInput)])
    -- toIndexRand = toIndexBase genInputThatPermutesToRandom "permute to rand with sz "
    toIndexRand :: (String, [(Int, ExpandInput)])
    toIndexRand = ("permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(1, 0, 100)]) | size <- sizes ]
              sizes = inputSizes

    -- toIndexRandWithLikeliness :: ExpandI -> Benchmark
    -- toIndexRandWithLikeliness expand = bgroup "likely expand to small, permute to rand with sz " [ bench (show size)  $ nf f xs  | (size, f, xs) <- curriedKernelsAndInputs]
    --     where !inputs = [ (size, genInputThatPermutesToRandomWithLikelyRange size (0, 10) (3000, 3500)) | size <- sizes ]
    --           !kernelsAndInputs = [ (size, runN (genericKernel expand totalSize), input) | (size, (totalSize, input)) <- inputs ]
    --           !curriedKernelsAndInputs = [ (size, curry kernel, input) | (size, kernel, input) <- kernelsAndInputs ]
    --           sizes = inputSizes
    --           curry kernel (a, b, c, d) = kernel a b c d
    toIndexRandWithLikeliness :: (String, [(Int, ExpandInput)])
    toIndexRandWithLikeliness = ("likely expand to small, permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(0.9, 0, 10), (0.1, 3000, 3500)]) | size <- sizes ]
              sizes = inputSizes

    toIndexRandWithLikelinessSmaller :: (String, [(Int, ExpandInput)])
    toIndexRandWithLikelinessSmaller = ("likely expand to small, sometimes moderate, permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(0.9, 0, 10), (0.1, 100, 500)]) | size <- sizes ]
              sizes = inputSizes
    
    toIndexRandWithLikeliness' :: (String, [(Int, ExpandInput)])
    toIndexRandWithLikeliness' = ("likely expand to small, likely permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomOrNothingWithLikelyRange size [(0.9, 0, 10), (0.1, 3000, 3500)] 0.25) | size <- sizes ]
              sizes = inputSizes

    smallSizes :: (String, [(Int, ExpandInput)])
    smallSizes = ("expand to small, permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(1, 0, 10)]) | size <- sizes ]
              sizes = inputSizes

    smallSizesN :: (String, [(Int, ExpandInput)])
    smallSizesN = ("expand to small, permute to N ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomOrNothingWithLikelyRange size [(1, 0, 10)] 0.99) | size <- sizes ]
              sizes = inputSizes
    
    bigSizes :: (String, [(Int, ExpandInput)])
    bigSizes = ("expand to big, permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(1, 1000, 3000)]) | size <- P.init $ P.init sizes ]
              sizes = inputSizes

    filterSizes :: (String, [(Int, ExpandInput)])
    filterSizes = ("expand as filter, permute to rand with sz ", inputs)
        where inputs = [ (size, trace ("Generating input for size " P.++ show size ) $ genInputThatPermutesToRandomWithLikelyRange' size [(0.75, 1, 1), (0.25, 0, 0)]) | size <- sizes ]
              sizes = inputSizes

    -- toIndexUnique :: ExpandI -> Benchmark
    -- toIndexUnique expand = toIndexBase genInputThatPermutesToUnique "permute to unique with sz " expand


    -- bigSizeRanges = [(0, 3000), (0, 5000), (3000, 5000)]
    -- bigSizeInputSizes = [0, 1, 10, 100, 1000]
    -- bigSizeRange :: ExpandI -> Benchmark
    -- bigSizeRange expand = bgroup "permute to unique" [ bench ("expand " P.++ (show size) P.++ " elements with sizes in range " P.++ show range)  $ nf f xs  | (size, range, f, xs) <- curriedKernelsAndInputs]
    --     where !inputs = [ (size, expansionSizeRange, genInputThatPermutesToUnique size expansionSizeRange) | size <- sizes, expansionSizeRange <- bigSizeRanges]
    --           !kernelsAndInputs = [ (size, range, runN (genericKernel expand totalSize), input) | (size, range, (totalSize, input)) <- inputs ]
    --           !curriedKernelsAndInputs = [ (size, range, curry kernel, input) | (size, range, kernel, input) <- kernelsAndInputs ]
    --           sizes = bigSizeInputSizes
    --           curry kernel (a, b, c, d) = kernel a b c d


    -- toIndex0 :: ExpandI -> Benchmark
    -- toIndex0 expand = bgroup "permute to (Just 0) with sz "  [ bench (show i P.++ " to " P.++ show j)  $ nf f xs  | (i, j, xs) <- inputs]
    --     where !inputs = [ (head xs, (head $ P.reverse xs), listToAcc xs) | xs <- sizes ]
    --           !f      = run1 (toIndex0' expand)

    -- toIndex0' :: ExpandI -> (Acc (Vector Int)) -> (Acc (Vector Int))
    -- toIndex0' expand szs = permute' P.max (use $ listToAcc (P.replicate sizesSize 0)) $ expand sz get (use $ listToAcc [0..inputSize])
    --     where get e i = T2 (Just_ (I1 0)) i--e * i
    --           sz i = szs A.!! i

    -- toIndexE :: ExpandI -> Benchmark
    -- toIndexE expand = bgroup "permute to (Just e) with sz "  [ bench (show i P.++ " to " P.++ show j)  $ nf f xs  | (i, j, xs) <- inputs]
    --     where !inputs = [ (head xs, (head $ P.reverse xs), listToAcc xs) | xs <- sizes ]
    --           !f      = run1 (toIndexE' expand)

    -- toIndexE' :: ExpandI -> (Acc (Vector Int)) -> (Acc (Vector Int))
    -- toIndexE' expand szs = permute' P.max (use $ listToAcc (P.replicate sizesSize 0)) $ expand sz get (use $ listToAcc [0..inputSize])
    --     where get e i = T2 (Just_ (I1 0)) i--e * i
    --           sz i = szs A.!! i


    -- toIndexRand :: ExpandI -> Benchmark
    -- toIndexRand expand = bgroup "permute to (Just rand) with sz "  [ bench (show i P.++ " to " P.++ show j)  $ nf f xs  | (i, j, xs) <- inputs]
    --     where !inputs = [ (head xs, (head $ P.reverse xs), listToAcc xs) | xs <- sizes ]
    --           !f      = run1 (toIndexRand' expand)

    -- toIndexRand' :: ExpandI -> (Acc (Vector Int)) -> (Acc (Vector Int))
    -- toIndexRand' expand szs = permute' P.max (use $ listToAcc (P.replicate sizesSize 0)) $ expand sz get (use $ listToAcc [0..inputSize])
    --     where get e i = T2 (Just_ (I1 $ randInt (e,i))) i--e * i
    --           sz i    = szs A.!! i
    --           randInt _ = lift $ unsafePerformIO $ randomRIO (0, sizesSize)



    -- manyDifferentSizes :: ExpandI -> Benchmark
    -- manyDifferentSizes expand = bgroup "permute to (Just rand) with "  $ [ bench (show i P.++ " high var szs")  $ nf f xs  | (i, xs) <- inputs]
    --     where !inputs = [ (P.length xs, listToAcc xs) | xs <- sizes ]
    --           sizes   = P.map (\i -> P.map (\j -> j + (unsafePerformIO $ randomRIO (-i, i))) (P.replicate i i)) [ 100, 1000, 10000, 100000, 1000000 ]
    --           !f      = run1 (toIndexRand' expand)

    -- bigSizes :: ExpandI -> Benchmark
    -- bigSizes expand = bgroup "permute to (Just rand) with "  $ [ bench (show i P.++ " big szs")  $ nf f xs  | (i, xs) <- inputs]
    --     where !inputs = [ (P.length xs, listToAcc xs) | xs <- sizes ]
    --           sizes   = P.map (\i -> P.map (\j -> j + (unsafePerformIO $ randomRIO (-(i `P.quot` 100), i `P.quot` 100))) (P.replicate i i)) [ 100, 1000, 10000, 100000, 1000000 ]
    --           !f      = run1 (toIndexRand' expand)
    
    -- smallSizes :: ExpandI -> Benchmark
    -- smallSizes expand = bgroup "permute to (Just rand) with "  $ [ bench (show i P.++ " small szs")  $ nf f xs  | (i, xs) <- inputs]
    --     where !inputs = [ (P.length xs, listToAcc xs) | xs <- sizes ]
    --           sizes   = P.map (\i -> P.map (\j -> (unsafePerformIO $ randomRIO (0, i `P.quot` 100))) (P.replicate i i)) [ 100, 1000, 10000, 100000, 1000000 ]
    --           !f      = run1 (toIndexRand' expand)

    -- smallSizes'' :: ExpandI -> Benchmark
    -- smallSizes'' e = bgroup "permute to (Just rand) with "  $ [ bench (show size P.++ " small sz elements")  $ nf f xs  | (f, size, xs) <- kernels]
    --     where !kernels = smallSizes' e

    -- bigSizes'' :: ExpandI -> Benchmark
    -- bigSizes'' e = bgroup "permute to (Just rand) with "  $ [ bench (show size P.++ " big sz elements")  $ nf f xs  | (f, size, xs) <- kernels]
    --     where !kernels = bigSizes' e

    -- randomSizes'' :: ExpandI -> Benchmark
    -- randomSizes'' e = bgroup "permute to (Just rand) with "  $ [ bench (show size P.++ " random sz elements")  $ nf f xs  | (f, size, xs) <- kernels]
    --     where !kernels = randomSizes' e

    -- smallSizes' e = kernels
    --     where inputSizes  = [100, 1000, 10000, 100000]
    --           inputArrays = P.map (\i -> unsafePerformIO $ sequence $ P.map (\j -> randSize j) $ P.replicate i 0) inputSizes
    --           inputs      = P.zip inputSizes inputArrays
    --           kernels     = [(run1 $ kernel e size, size, listToAcc input) | (size, input) <- inputs]
    --           randSize _  = randomRIO (0, 16)

    -- bigSizes' e = kernels
    --     where inputSizes  = [10, 100, 1000, 10000]
    --           inputArrays = P.map (\i -> unsafePerformIO $ sequence $ P.map (\j -> randSize j) $ P.replicate i 0) inputSizes
    --           inputs      = P.zip inputSizes inputArrays
    --           kernels     = [(run1 $ kernel e size, size, listToAcc input) | (size, input) <- inputs]
    --           randSize _  = randomRIO (100, 500)

    -- randomSizes' e = kernels
    --     where inputSizes  = [10, 100, 1000, 10000]
    --           inputArrays = P.map (\i -> unsafePerformIO $ sequence $ P.map (\j -> randSize j) $ P.replicate i 0) inputSizes
    --           inputs      = P.zip inputSizes inputArrays
    --           kernels     = [(run1 $ kernel e size, size, listToAcc input) | (size, input) <- inputs]
    --           randSize _  = randomRIO (0, 3000)

    -- kernel :: ExpandI -> Int -> Acc (Vector Int) -> Acc (Vector Int)
    -- kernel expand totalSize input = permute' const (fill (I1 (constant totalSize)) 0) $ expand sz get input
    --     where sz i      = i
    --           get e i   = T2 (Just_ $ I1 $ randIdx e) i
    --           randIdx _ = lift $ unsafePerformIO $ randomRIO (0, totalSize)

    -- -- Creates a histogram of how often a size is in the input
    -- szHistogramKernel :: ExpandI -> Int -> Acc (Vector Int) -> Acc (Vector Int)
    -- szHistogramKernel expand totalSize input = permute' (+) (fill (I1 (constant totalSize)) 0) $ expand sz get input
    --     where sz i      = i
    --           get e i   = ifThenElse (e - 1 A.== i) (T2 (Just_ $ I1 $ randIdx e i) 1) (T2 Nothing_ 0)
    --           randIdx e i = e
