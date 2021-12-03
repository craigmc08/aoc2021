module Lib
  ( run,
  )
where

import Solver
import System.Environment (getArgs)

run :: IO ()
run = do
  args <- getArgs
  input <- readFile (args !! 1)
  case head args of
    "1a" -> solve1a input
    "1b" -> solve1b input
    "2a" -> solve2a input
    "2b" -> solve2b input
    "3a" -> solve3a input
    "3b" -> solve3b input
    _ -> putStrLn "Not implemented"
