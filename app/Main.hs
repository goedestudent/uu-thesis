{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Lib
import ThreeD
import ObjParser

import Debug.Trace
import Data.Time

import Data.Array.Accelerate.LLVM.PTX (run, run1)
import Data.Array.Accelerate (use)

main :: IO ()
main = do
    start <- getCurrentTime
    let !obj = run $ fromFile "dragon.obj"
    let !obj' = trace ("Obj loaded") obj
    let !obj1 = run $ fromFile "dragon1.obj"
    let !obj1' = trace ("Obj loaded") obj
    loaded <- getCurrentTime

    putStrLn $ show $ (diffUTCTime loaded start)
    let f = run1 (paintToCanvas . orthographic)
    let !x = f obj'
    end'  <- getCurrentTime
    putStrLn $ show $ (diffUTCTime end' loaded)

    start' <- getCurrentTime
    let !y = f obj1'
    end <- getCurrentTime

    putStrLn $ show $ (diffUTCTime end start')
    where !obj = run $ fromFile "teapot.obj"