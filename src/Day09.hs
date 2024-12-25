module Day09 where

import Utils
import Data.Char
import Debug.Trace
import Data.List

type ID = Int
data Block = Free | File ID deriving (Show, Eq, Ord)

data BlockType = F | S deriving (Show, Eq)

expandDisk :: String -> [Block]
expandDisk s = go F 0 s
  where
    go :: BlockType -> ID -> String -> [Block]
    go _ _ [] = []
    go F i (x:xs) = replicate (digitToInt x) (File i) ++ go S (i+1) xs
    go S i (x:xs) = replicate (digitToInt x) Free ++ go F i xs

compactDisk :: [Block] -> [Block]
compactDisk s = reverse $ go is (reverse is) []
  where
    is = zip [0..] (reverse s)

    go :: [(Int, Block)] -> [(Int, Block)] -> [Block] -> [Block]
    go (x@(i, c):xs) (y@(j, d):ys) acc
      | j < i = acc
      | c == Free = go xs (y:ys) acc
      | d == Free = go xs ys (c:acc)
      | otherwise = go (x:xs) ys (d:acc)

checksum :: [Block] -> Int
checksum s = sum [i * v | (i, File v) <- filter ((/= Free) . snd) $ zip [0..] s]

part1 :: String -> IO ()
part1 inp = do
  let expanded = expandDisk (strip inp)
      compacted = compactDisk expanded
  print $ checksum compacted

compress :: (Eq a, Ord a) => [a] -> [(Int, a)]
compress xs = map (\(x:xs) -> (length xs + 1, x)) $ group xs


part2 :: String -> IO ()
part2 inp = do
  let expanded = expandDisk (strip inp)
      compressed = compress expanded
  print inp
  print compressed
