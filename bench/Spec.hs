{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
-- stack bench --benchmark-arguments '--output=$benchmark.html'
-- stack bench --benchmark-arguments '--template json --output=$benchmark.json'

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

config' :: Config
config' = defaultConfig {jsonFile = Just "benchmark.json", reportFile = Just "benchmark.html", timeLimit=0.1}

config :: String -> Config
config name = defaultConfig {jsonFile = Just (benchname++".json"), reportFile = Just (benchname++".html"), timeLimit=5}
      where benchname = "benchmark-"++name

main :: IO ()
main = do
      --     analyse "micro1" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks1] | (strategyName, kernel) <- microBenchKernels ]
          
      --     analyse "micro2" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks2] | (strategyName, kernel) <- microBenchKernels ]
          
      --     analyse "micro3" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks3] | (strategyName, kernel) <- microBenchKernels ]
          
      --     analyse "micro4" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks4] | (strategyName, kernel) <- microBenchKernels ]
          
      --     analyse "micro5" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks5] | (strategyName, kernel) <- microBenchKernels ]

      --     analyse "micro6" $ 
      --       [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks6] | (strategyName, kernel) <- microBenchKernels ]

          analyse "micro7" $ 
            [ bgroup strategyName $ [bgroup benchName [ bench (show size) $ nf (runMicroBench kernel) benchInput | (size, benchInput) <- benchInputs] | (benchName, benchInputs) <- microBenchmarks7] | (strategyName, kernel) <- microBenchKernelsSklansky ]
          
      --     Only once, because the armadillo model takes very long in the No fusion kernel
      --     analyse' 1 "3d" $ 
      --       [ bgroup strategyName $ [benchmark realBenchmarksInputs strategy | benchmark <- realBenchmarks] | (strategyName, strategy) <- strategiesWithName2 ]
          
      --     analyse "primes" $ 
      --       [ bgroup strategyName $ [benchmark realBenchmarks2Inputs strategy | benchmark <- realBenchmarks2] | (strategyName, strategy) <- strategiesWithName3 ]
      --     analyse "primes-inefficient" $ 
      --       [ bgroup strategyName $ [benchmark realBenchmarks2'Inputs strategy | benchmark <- realBenchmarks2'] | (strategyName, strategy) <- strategiesWithName3 ]
      --     analyse "primes-balanced" $ 
      --       [ bgroup strategyName $ [benchmark realBenchmarks2''Inputs strategy | benchmark <- realBenchmarks2''] | (strategyName, strategy) <- strategiesWithName3 ]
          
      --     analyse "bfs" $ 
      --       [ bgroup strategyName $ [benchmark realBenchmarks3Inputs strategy | benchmark <- realBenchmarks3] | (strategyName, strategy) <- strategiesWithName4 ]
  where 
        !microBenchmarks1 = [toIndexRand]
        !microBenchmarks2 = [toIndexRandWithLikeliness]--, toIndexRandWithLikelinessSmaller]
        !microBenchmarks3 = [toIndexRandWithLikeliness']
        !microBenchmarks4 = [smallSizes]
        !microBenchmarks5 = [bigSizes]
        !microBenchmarks6 = [filterSizes]
        !microBenchmarks7 = [smallSizesN]
        
      --   !microBenchKernels = [(name, trace ("Creating microbenchmark kernel" ++ name) $ runN $ genericKernel strategy) | (name, strategy) <- strategiesWithName]
      --   !microBenchKernelsBinSearch = [(name, trace ("Creating microbenchmark kernel" ++ name) $ runN $ genericKernel strategy) | (name, strategy) <- strategiesWithNameBinSearch]
        !microBenchKernelsSklansky = [(name, trace ("Creating microbenchmark kernel" ++ name) $ runN $ genericKernel strategy) | (name, strategy) <- strategiesWithNameSklansky]
        !runMicroBench = \kernel -> (\(totalSizes, (input, defs, sizes, segments, elements)) -> kernel input defs sizes segments elements)
        

      --   !realBenchmarks   = [benchThreeD]
      --   !realBenchmarks2  = [benchPrimes]
      --   !realBenchmarks2'  = [benchPrimesInefficient]
      --   !realBenchmarks2''  = [benchPrimesBalanced]
      --   !realBenchmarks3  = [benchBFS]

      --   !realBenchmarksInputs = benchThreeDInputs
      --   !realBenchmarks2Inputs = benchPrimesInputs
      --   !realBenchmarks2'Inputs = Prelude.init $ Prelude.init $ benchPrimesInputs
      --   !realBenchmarks2''Inputs = realBenchmarks2'Inputs
      --   !realBenchmarks3Inputs = benchBFSInputs

        strategies  = [Basic, Blocks, Shuffle, MultiBlock 2, MultiBlock 16, MultiBlock 128, MultiBlock 256, MultiBlock 512]--[Basic, BlockShuffle]
        strategiesBinSearch  = [Basic, Blocks, Shuffle, MultiBlock 2, MultiBlock' 2, MultiBlock 16, MultiBlock' 16, MultiBlock 128, MultiBlock' 128, MultiBlock 256, MultiBlock' 256, MultiBlock 512, MultiBlock' 512]--[Basic, BlockShuffle]
        strategiesSklansky   = [Basic, Blocks, Shuffle, MultiBlock 2, MultiBlock 16, MultiBlock'' 16, MultiBlock 128, MultiBlock'' 128, MultiBlock 256, MultiBlock'' 256, MultiBlock 512, MultiBlock'' 512]--[Basic, BlockShuffle]
      --   !strategiesWithName   = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategies]
      --   !strategiesWithNameBinSearch   = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategiesBinSearch]
        !strategiesWithNameSklansky   = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategiesSklansky]
      --   !strategiesWithName2  = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategies]
      --   !strategiesWithName3  = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategies]
      --   !strategiesWithName4  = ("No fusion", expand') : [(show strategy, expand strategy) | strategy <- strategies]

analyse = analyse' 10
analyse' runs benchmarkName bgroup = 
      do
          runMode (O.RunIters config' runs O.Prefix [""]) bgroup
          defaultMainWith (config benchmarkName) bgroup

-- Precompute inputs, because we do not want them to be different for every different 'expand' kernel
benchPrimesInputs = [ (x, fromList (Z) [x]) | x <- [10::Int, 100, 1000, 10000, 100000, 1000000, 10000000]]
benchPrimes inputs expand = bgroup "primes "  [ bench (show i)  $ nf f xs  | (i, xs) <- inputs]
      where !f      = run1 $ primesBenchmark expand
benchPrimesInefficient inputs expand = bgroup "primes inefficient"  [ bench (show i)  $ nf f xs  | (i, xs) <- inputs]
      where !f      = run1 $ primesBenchmarkInefficient expand
benchPrimesBalanced inputs expand = bgroup "primes balanced"  [ bench (show i)  $ nf f xs  | (i, xs) <- inputs]
      where !f      = run1 $ primesBenchmarkBalanced expand

-- Precompute inputs, because we do not want them to be different for every different 'expand' kernel
benchThreeDInputs = [ (x, run $ orthographic $ fromFile x) | x <- ["teapot.obj", "dragon.obj", "armadillo.obj"]] --, "dragon_big.obj"]]
benchThreeD inputs expand = bgroup "threeD "  [ bench (show i)  $ nf f xs  | (i, xs) <- inputs]
      where !f      = run1 $ paintToCanvas expand

-- Precompute inputs, because we do not want them to be different for every different 'expand' kernel
benchBFSInputGraphs     = [exampleGraph, makeRegularGraph (300, 30), makeRegularGraph (300, 150), makeIrregularGraph 300 0.9 (0, 10) (3000, 3500), makeIrregularGraph 1000 0.85 (0, 10) (100, 200)]
benchBFSInputGraphNames = ["Example Graph", "300 nodes with 30 neighbours", "300 nodes with 150 neighbours", "300 with P=.9 0-10 neighbours, else 3000-3500 neighbours", "1000 with P=.85 0-10 neighbours, else 100-200 neighbours"]
benchBFSInputs = [ (benchBFSInputGraphNames Prelude.!! i, ((fromList (Z) [0::Int]), (sparsifyGraph x))) | (i, x) <- (Prelude.zip [0..] benchBFSInputGraphs)]
benchBFS inputs expand = bgroup "bfs "  [ bench i  $ nf f xs  | (i, xs) <- inputs]
      where !f = \(src, (sizes, values, offsets)) -> kernel src sizes values offsets
            !kernel = runN $ bfsBenchmark expand
            -- 9,3 bad, 100,15 good
