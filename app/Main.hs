module Main where

import System.Environment (getArgs)

import Lib
import Cellout

main :: IO ()
main =  do
    args <- getArgs
    case args of
        [input] -> stripOutputIO input "no_output.ipynb"
        [input, output] -> stripOutputIO input output
        _ -> putStrLn "please specify at least the input filename"
