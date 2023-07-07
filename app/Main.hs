{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- stack run -- +ACC -ddump-cc
module Main (main) where

main :: IO ()
main = putStrLn "Hello world :)"