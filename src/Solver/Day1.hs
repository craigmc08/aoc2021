module Solver.Day1 (solve1a, solve1b) where

import Control.Monad ((>=>))

parse :: String -> [Int]
parse = map read . lines

solve1a :: String -> IO ()
solve1a =
  (pure . parse) >=> \input -> do
    -- Pairs of (prev, curr)
    let pairs = zip input (tail input)
    let result = length $ filter (\(prev, curr) -> curr > prev) pairs
    print result

solve1b :: String -> IO ()
solve1b =
  (pure . parse) >=> \input -> do
    let windowSums = map sum $ windows 3 input
    let pairs = zip windowSums (tail windowSums)
    let result = length $ filter (\(prev, curr) -> curr > prev) pairs
    print result

windows :: Int -> [a] -> [[a]]
windows n xs
  | length xs < n = []
  | otherwise = take n xs : windows n (tail xs)
