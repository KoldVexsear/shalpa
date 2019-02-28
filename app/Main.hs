{-# LANGUAGE RankNTypes #-}
module Main where

import Lib
import System.Environment
import Data.List
import System.IO

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    let argsl = unlines args
    hPutStrLn stdout $ translate argsl