module Day09 where

import Utils
import Data.Char
import Data.List
import Data.Function

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

moveLeft :: (Int, Block) -> [(Int, Block)] -> Maybe [(Int, Block)]
moveLeft b (x:_) | x == b = Nothing
moveLeft b@(size, _) (x@(sizeX, Free):xs)
  | sizeX >= size = let free = sizeX - size in Just $ if free > 0 then b:(free, Free):xs else b:xs
moveLeft b (x:xs) = fmap (x:) $ moveLeft b xs

freeRight :: (Int, Block) -> [(Int, Block)] -> [(Int, Block)]
freeRight e@(size, _) xs = reverse $ go (reverse xs)
  where
    go [] = []
    go (x:xs) | x == e = (size, Free):xs
    go (x:xs) = x:go xs

mergeFrees :: [(Int, Block)] -> [(Int, Block)]
mergeFrees = concatMap merge . groupBy ((==) `on` snd)
  where
    merge xs@((_, Free):_) = [(sum (map fst xs), Free)]
    merge xs = xs

compactDisk' :: [(Int, Block)] -> [(Int, Block)]
compactDisk' xs = foldr moveBlock xs xs
  where
    moveBlock x acc = case moveLeft x acc of
      Just xs -> mergeFrees $ freeRight x xs
      Nothing -> acc

decompress :: [(Int, Block)] -> [Block]
decompress = concatMap (\(size, b) -> replicate size b)

toString :: [Block] -> String
toString = map showBlock 
  where
    showBlock Free = '.'
    showBlock (File i)
      | i < 10 = intToDigit i
      | otherwise = '#'

part2 :: String -> IO ()
part2 inp = do
  let expanded = expandDisk (strip inp)
      compressed = compress expanded
      blocks = compactDisk' compressed
  print $ checksum $ decompress blocks
