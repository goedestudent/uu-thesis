{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

import Criterion.Main
import Criterion.Types
import qualified Criterion.Main.Options as O

import Prime
import ThreeD
import Bench.Micro
import Types
import BFS
import BfsInputs
import Utils (genericKernel)

import Data.Array.Accelerate.LLVM.PTX (run, run1, runN)
import Data.Array.Accelerate hiding ((++))

import Debug.Trace (trace)

config :: Config
config = defaultConfig {jsonFile = Just "benchmark.json", reportFile = Just "benchmark.html", timeLimit=0.01}

main :: IO ()
main = do
          test "micro1" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks1]
          test "micro2" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks2]
          test "micro3" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks3]
          test "micro4" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks4]
          test "micro5" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks5]
          test "micro6" $ [ [(runMicroBench kernel benchInput) Prelude.== (runMicroBench (runN $ genericKernel expand') benchInput) | (size, benchInput) <- benchInputs] | (strategyName, kernel) <- microBenchKernels, (benchName, benchInputs) <- microBenchmarks6]
          
          
          test "primes" $ 
            [ [(benchmark benchInput) Prelude.== ((benchPrimes expand') benchInput)] | (name, benchmark) <- realBenchmarks2, (size, benchInput) <- realBenchmarks2Inputs ]
  where 
        !microBenchmarks1 = [toIndexRand]
        !microBenchmarks2 = [toIndexRandWithLikeliness]
        !microBenchmarks3 = [toIndexRandWithLikeliness']
        !microBenchmarks4 = [smallSizes]
        !microBenchmarks5 = [bigSizes]
        !microBenchmarks6 = [filterSizes]
        
        !microBenchKernels = [(name, trace ("Creating microbenchmark kernel" ++ name) $ runN $ genericKernel strategy) | (name, strategy) <- strategiesWithName]
        !runMicroBench = \kernel -> (\(totalSizes, (input, defs, sizes, segments, elements)) -> kernel input defs sizes segments elements)
        !realBenchmarks2  = [(name, benchPrimes strategy) | (name, strategy) <- strategiesWithName3]
        !realBenchmarks2Inputs = benchPrimesInputs
        strategies  = [Basic, Blocks, BlockShuffle, MultiBlock 2, MultiBlock' 2, MultiBlock 16, MultiBlock' 16, MultiBlock 128, MultiBlock' 128, MultiBlock 256, MultiBlock' 256, MultiBlock 512, MultiBlock' 512]--[Basic, BlockShuffle]
        !strategiesWithName   = [(show strategy, expand strategy) | strategy <- strategies]
        !strategiesWithName3  = [(show strategy, expand strategy) | strategy <- strategies]

test :: String -> [[Bool]] -> IO ()
test name result = do 
    putStrLn (name ++ ": " ++ show allEqual ++ extraInfo)
    where allEqual = (Prelude.all id) $ listsEqual
          listsEqual = Prelude.map (Prelude.all id) result
          extraInfo = if allEqual then "" else show result


-- Precompute inputs, because we do not want them to be different for every different 'expand' kernel
benchPrimesInputs = [ (x, fromList (Z) [x]) | x <- [10::Int, 100, 1000, 10000, 100000, 1000000, 10000000]]
benchPrimes expand = f
      where !f      = run1 $ primesBenchmark expand

