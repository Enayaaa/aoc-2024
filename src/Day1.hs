module Day1 where

import Data.List
import Data.Function
import Data.Maybe
import Utils (fromList)

parseInput :: String -> ([Int], [Int])
parseInput inp = fromList $ transpose [map read $ words line | line <- lines $ inp] 

sortOnFst :: Ord a => [(a, b)] -> [(a, b)]
sortOnFst = sortBy (compare `on` fst)

part1 :: String -> IO ()
part1 inp = do
  let (a, b) = parseInput inp
  let a' = sortOnFst $ zip a [0..]
      b' = sortOnFst $ zip b [0..]
      diffs = map abs $ zipWith (-) (map fst a') (map fst b')
  putStrLn $ show $ sum diffs

histogram :: [Int] -> [(Int, Int)]
histogram xs = map (\x -> (head x, length x)) $ group $ sort xs

part2 :: String -> IO ()
part2 inp = do
  let (a, b) = parseInput inp
  let hist = histogram b
      sims = map (\x -> x * (fromMaybe 0 $ lookup x hist)) a
  putStrLn $ show $ sum sims
