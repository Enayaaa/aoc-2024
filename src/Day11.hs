module Day11 where

import qualified Data.Map as M
import Utils

parse :: String -> [Int]
parse inp = map read $ words inp

digits :: Int -> Int
digits n
    | n `div` 10 == 0 = 1
    | otherwise = 1 + digits (n `div` 10)

splitDigits :: Int -> (Int, Int)
splitDigits n = (n `div` half, n `mod` half)
  where
    half = 10 ^ (digits n `div` 2)

blink :: Int -> [Int]
blink n
    | n == 0 = [1]
    | digits n `mod` 2 == 0 = toList (splitDigits n)
    | otherwise = [n * 2024]

-- Evolve a map of stone counts
evolveCounts :: M.Map Int Int -> M.Map Int Int
evolveCounts counts = foldl processStone M.empty (M.toList counts)
  where
    processStone acc (stone, count) =
        foldl (\m newStone -> M.insertWith (+) newStone count m) acc (blink stone)

computeStones :: [Int] -> Int -> Int
computeStones initialStones blinks =
    let initialCounts = foldl (\m stone -> M.insertWith (+) stone 1 m) M.empty initialStones
        finalCounts = iterate evolveCounts initialCounts !! blinks
     in sum (M.elems finalCounts)

part1 :: String -> IO ()
part1 inp = do
    print $ computeStones (parse inp) 25

part2 :: String -> IO ()
part2 inp = do
    print $ computeStones (parse inp) 75
