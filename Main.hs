module Main where

import Mastermind

main :: IO ()
main = do
  l <- getLine
  prettySolve l