module Solver.Day2 (solve2a, solve2b) where

import Control.Monad ((>=>))

data Position = Position {horizontal :: Int, depth :: Int}

data Position' = Position' {horizontal' :: Int, depth' :: Int, aim' :: Int}

data Command = Forward Int | Down Int | Up Int

runCommand :: Position -> Command -> Position
runCommand (Position h d) (Forward n) = Position (h + n) d
runCommand (Position h d) (Down n) = Position h (d + n)
runCommand (Position h d) (Up n) = Position h (d - n)

runCommandB :: Position' -> Command -> Position'
runCommandB (Position' h d a) (Forward n) = Position' (h + n) (d + n * a) a
runCommandB (Position' h d a) (Down n) = Position' h d (a + n)
runCommandB (Position' h d a) (Up n) = Position' h d (a - n)

parseCommand :: String -> Command
parseCommand line = case words line of
  ["forward", n] -> Forward (read n)
  ["down", n] -> Down (read n)
  ["up", n] -> Up (read n)
  _ -> error "Invalid command"

parse :: String -> [Command]
parse = map parseCommand . lines

solve2a :: String -> IO ()
solve2a =
  pure . parse >=> \input -> do
    let (Position h d) = foldl runCommand (Position 0 0) input
    print $ h * d

solve2b :: String -> IO ()
solve2b =
  pure . parse >=> \input -> do
    let (Position' h d a) = foldl runCommandB (Position' 0 0 0) input
    print $ h * d
