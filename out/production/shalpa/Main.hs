{-# LANGUAGE RankNTypes #-}
module Main where

import Lib
import System.Environment
import Data.List
import System.IO

import NLib

main :: IO ()
main = do
    hSetEncoding stdout utf8
--    args' <- getArgs
--    let args = unlines args'
--    let argsl = translate args
--    hPutStrLn stdout $ argsl
--    hPutStrLn stdout $ decode argsl
    hPutStrLn stdout (show (toSString "фыа фыа нртл\n нгпормли ГРМП"))
