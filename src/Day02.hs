module Day02 where

parse :: String -> [[Int]]
parse inp = map (map read . words) $ lines inp

isSafe :: [Int] -> Bool
isSafe lvs = (increasing || decreasing) && safeDiff
  where
    pairs = zip lvs (tail lvs)
    increasing = all (uncurry (>)) pairs
    decreasing = all (uncurry (<)) pairs
    safeDiff = all (\(x,y) -> let d = abs (x - y) in d >= 1 && d <= 3) pairs

part1 :: String -> IO ()
part1 inp = do
  let parsed = parse inp
  let safes = map isSafe parsed
  let res = length $ filter id safes
  print res

removeAt :: Int -> [a] -> [a]
removeAt i xs = let (ys,zs) = splitAt i xs in ys ++ (tail zs)

isSafeRelaxed :: [Int] -> Bool
isSafeRelaxed lvs = any (isSafe) candidates
  where
    candidates = [removeAt i lvs | (i,_) <- zip [0..] lvs]

part2 :: String -> IO ()
part2 inp = do
  let parsed = parse inp
  let safes = map isSafeRelaxed parsed
  let res = length $ filter id safes
  print res
