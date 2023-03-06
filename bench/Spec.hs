{-# LANGUAGE BangPatterns #-}
-- stack bench --benchmark-arguments '--output=$benchmark.html'

import Criterion.Main

import ThreeD
import BFS
import ObjParser

import Data.Array.Accelerate.LLVM.PTX (run, run1)
import Data.Array.Accelerate (use)

main :: IO ()
main = defaultMain [
  bgroup "render" [ bench "teapotNf"  $ nf  (run1 $ paintToCanvas . orthographic) teapot,
                    bench "teapotWhnf"  $ whnf  (run1 $ paintToCanvas . orthographic) teapot,
                    bench "dragonNf"  $ nf  (run1 $ paintToCanvas . orthographic) dragon,
                    bench "dragonWHNF"  $ whnf  (run1 $ paintToCanvas . orthographic) dragon,
                    bench "dragonWithT"  $ nfIO  (render . orthographic $ fromFile "dragon.obj") -- ,
                    -- bench "dragonWithTWHNF"  $ whnf  (run1 $ paintToCanvas . orthographic) (objToTriangles "dragon.obj")
                    -- bench "dragonWithT"  $ nfIO  (render . orthographic $ fromFile "dragon.obj")
                    -- bench "copy"  $ nfAppIO  (render . orthographic) copy
               ]--,
  -- bgroup "bfs" [
  --     bench "nf"   $ nfAppIO (putStrLn . show . run . bfs 0) graph,
  --     bench "whnf" $ whnfAppIO (putStrLn . show . run . bfs 0) graph
  --   ]
  ]
  where 
    !teapot = run $ fromFile "teapot.obj"
    !dragon = run $ fromFile "dragon.obj"
    !copy = fromFile "teapot copy.obj"
    !graph = makeGraph exampleGraph
