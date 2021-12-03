module Solver.Day3 (solve3a, solve3b) where

import Control.Monad ((>=>))
import Data.List (transpose)

data Bit = Zero | One deriving (Eq, Show)

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

bitPlus :: Bit -> Int -> Int
bitPlus Zero n = n
bitPlus One n = n + 1

showBit :: Bit -> Char
showBit Zero = '0'
showBit One = '1'

bitsToInt :: [Bit] -> Int
bitsToInt = sum . zipWith (*) [2 ^ n | n <- [0 ..]] . reverse . map bitToInt

bitNot :: Bit -> Bit
bitNot Zero = One
bitNot One = Zero

mostCommon :: Bit -> [Bit] -> Bit
mostCommon tie bits
  | n * 2 > l = One
  | n * 2 < l = Zero
  | otherwise = tie
  where
    n = foldr bitPlus 0 bits
    l = length bits

parse :: String -> [[Bit]]
parse = map (map (\c -> if c == '1' then One else Zero)) . lines

solve3a :: String -> IO ()
solve3a =
  (pure . parse) >=> \input -> do
    let columns = transpose input
    let gammaBits = map (mostCommon One) columns
    let epsilonBits = map bitNot gammaBits
    let gammaStr = map showBit gammaBits
    let gamma = bitsToInt gammaBits
    let epsilonStr = map showBit epsilonBits
    let epsilon = bitsToInt epsilonBits
    putStrLn $ "Gamma: " ++ gammaStr ++ " (" ++ show gamma ++ ")"
    putStrLn $ "Epsilon: " ++ epsilonStr ++ " (" ++ show epsilon ++ ")"
    putStrLn $ "Product: " ++ show (gamma * epsilon)

solve3b :: String -> IO ()
solve3b =
  (pure . parse) >=> \input -> do
    let oxygenBits = run3b (map (mostCommon One) . transpose) input
    -- Tie breaker is one because it's notted immediately
    let co2Bits = run3b (map (bitNot . mostCommon One) . transpose) input
    let oxygenStr = map showBit oxygenBits
    let oxygen = bitsToInt oxygenBits
    let co2Str = map showBit co2Bits
    let co2 = bitsToInt co2Bits
    putStrLn $ "Oxygen: " ++ oxygenStr ++ " (" ++ show oxygen ++ ")"
    putStrLn $ "CO2: " ++ co2Str ++ " (" ++ show co2 ++ ")"
    putStrLn $ "Product: " ++ show (oxygen * co2)
  where
    run3b :: ([[Bit]] -> [Bit]) -> [[Bit]] -> [Bit]
    run3b getBits options = go (getBits options) options 0
      where
        go :: [Bit] -> [[Bit]] -> Int -> [Bit]
        go bits options n
          | length options == 1 = head options
          | otherwise =
            let options' = filter (\opt -> opt !! n == bits !! n) options
             in go (getBits options') options' (n + 1)
