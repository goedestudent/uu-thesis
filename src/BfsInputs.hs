{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module BfsInputs where

import BFS
import System.Random

-- | Makes a square graph, where every node is connected to every other node. The order of neighbours is random.
makeFullyConnectedGraph :: Int -> [[Int]]
makeFullyConnectedGraph numNodes = snd $ foldr propagateShuffle (genNeighbours, []) regularUnshuffled
    where -- | Regular graph that has not yet been shuffled
          regularUnshuffled = [removeAt i $ take numNodes [0..] | i <- take numNodes [0..]]
          genNeighbours     = mkStdGen 42
          propagateShuffle xs (gen, ys)
                            = let (gen', xs') = shuffle xs gen
                              in  (gen', xs' : ys)

          -- | Shuffles the given list to a random order. Returns the new random generator and the shuffled list.
          shuffle [] gen = (gen, [])
          shuffle xs gen = let 
                                (i, gen') = randomR (0, length xs - 1) gen
                                fstHalf   = take (i) xs
                                sndHalf   = drop (i + 1) xs
                                (gen'', tail) = (shuffle (removeAt i xs) gen')
                           in (gen'', (xs !! i) : tail)
          -- | Removes the element at index i of the list
          removeAt i xs     = let
                                  firstHalf  = take (i) xs
                                  secondHalf = drop (i + 1) xs
                               in firstHalf ++ secondHalf

-- | Generates a regular graph, where the amount of neighbours for all nodes is set to a predefined number.
makeRegularGraph :: (Int, Int) -> [[Int]]
makeRegularGraph (numNodes, numNeighbours)
    | numNodes <= numNeighbours = error "Number of neighbours cannot be more then total number of nodes"
    | otherwise = map (take numNeighbours) (makeFullyConnectedGraph numNodes)


makeIrregularGraph :: Int -> Float -> (Int, Int) -> (Int, Int) -> [[Int]]
makeIrregularGraph numNodes prob1 range1 range2
    = zipWith (\size allNeighbours -> take size allNeighbours) sizes fullyConnected 

    where gen1Or2     = mkStdGen 333
          genRange1   = mkStdGen 137
          genRange2   = mkStdGen 12
          choose1     = round (prob1 * 10)
          choices1Or2 = randomRs (1::Int, 10::Int) gen1Or2
          range1s     = randomRs range1 genRange1
          range2s     = randomRs range2 genRange2
          ranges1And2 = zip range1s range2s
          sizes       = zipWith (\choose1Or2 (choice1, choice2) -> if choose1Or2 <= choose1 then choice1 else choice2) choices1Or2 ranges1And2

          fullyConnected = makeFullyConnectedGraph numNodes