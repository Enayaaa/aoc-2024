module Day5 where

import Utils (splitOn, fromList)

parse :: String -> ([(Int, Int)], [[Int]])
parse inp = (rules, updates)
  where
    (sec1, _:sec2) = break (=="") $ lines inp
    rules          = map (fromList . map read . splitOn '|') sec1
    updates        = map (map read . splitOn ',') sec2

isValid :: [(Int, Int)] -> [Int] -> Bool
isValid _ [_]        = True
isValid rules (x:xs) = valid && isValid rules xs
  where
    valid = all (\y -> (x,y) `elem` rules) xs

getMid :: [a] -> a
getMid xs = xs !! (length xs `div` 2)

part1 :: String -> IO ()
part1 inp = do
  let (rules, updates) = parse inp
      validity         = map (isValid rules) updates
      validUpdates     = [xs | (valid, xs) <- zip validity updates, valid]
      validMids        = map getMid validUpdates
  print $ sum validMids

sortByRules :: [(Int, Int)] -> [Int] -> [Int]
sortByRules rules ys = bubbleSort [] ys []
  where
    bubbleSort :: [Int] -> [Int] -> [Int] -> [Int]
    bubbleSort [] [] acc   = acc
    bubbleSort [] [x] acc  = x:acc
    bubbleSort pre [x] acc = bubbleSort [] (reverse pre) (x:acc)
    bubbleSort _ [] _      = error "bubbleSort: unreachale"
    bubbleSort pre (x:y:xs) acc
      | (x,y) `elem` rules = bubbleSort (x:pre) (y:xs) acc
      | otherwise          = bubbleSort (y:pre) (x:xs) acc

part2 :: String -> IO ()
part2 inp = do
  let (rules, updates) = parse inp
      validity         = map (isValid rules) updates
      invalidUpdates   = [xs | (valid, xs) <- zip validity updates, not valid]
      sortedUpdates    = map (sortByRules rules) invalidUpdates
      mids             = map getMid sortedUpdates
  print $ sum mids
