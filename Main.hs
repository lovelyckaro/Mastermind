module Main where

import Mastermind ( prettySolve )
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "Input the correct answer: "
  hFlush stdout
  l <- getLine
  prettySolve l