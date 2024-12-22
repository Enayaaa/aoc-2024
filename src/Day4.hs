module Day4 where

printGrid :: [[Char]] -> IO ()
printGrid = mapM_ putStrLn

(!) :: [[a]] -> Int -> Int -> a
(!) xs i = (!!) (xs !! i)

countInvolvingX :: [[Char]] -> Int -> Int -> Int
countInvolvingX grid row col = length $ filter id [a, b, c, d, e, f, g, h]
  where
    rows = length grid
    cols = length $ head grid
    a = col + 3 < cols && (grid ! row) (col + 1) == 'M' && (grid ! row) (col + 2) == 'A'       && (grid ! row) (col + 3) == 'S'
    b = col - 3 >= 0   && (grid ! row) (col - 1) == 'M' && (grid ! row) (col - 2) == 'A'       && (grid ! row) (col - 3) == 'S'
    c = row + 3 < rows && (grid ! (row + 1)) col == 'M' && (grid ! (row + 2)) col == 'A'       && (grid ! (row + 3)) col == 'S'
    d = row - 3 >= 0   && (grid ! (row - 1)) col == 'M' && (grid ! (row - 2)) col == 'A'       && (grid ! (row - 3)) col == 'S'
    e = row + 3 < rows && col + 3 < cols                && (grid ! (row + 1)) (col + 1) == 'M' && (grid ! (row + 2)) (col + 2) == 'A' && (grid ! (row + 3)) (col + 3) == 'S'
    f = row - 3 >= 0   && col - 3 >= 0                  && (grid ! (row - 1)) (col - 1) == 'M' && (grid ! (row - 2)) (col - 2) == 'A' && (grid ! (row - 3)) (col - 3) == 'S'
    g = row + 3 < rows && col - 3 >= 0                  && (grid ! (row + 1)) (col - 1) == 'M' && (grid ! (row + 2)) (col - 2) == 'A' && (grid ! (row + 3)) (col - 3) == 'S'
    h = row - 3 >= 0   && col + 3 < cols                && (grid ! (row - 1)) (col + 1) == 'M' && (grid ! (row - 2)) (col + 2) == 'A' && (grid ! (row - 3)) (col + 3) == 'S'

part1 :: String -> IO ()
part1 inp = do
  let grid = lines inp
      rows = length grid
      cols = length $ head grid
      coords = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
      xCoords = filter (\(i, j) -> (grid ! i) j == 'X') coords
      xmases = map (\(i, j) -> countInvolvingX grid i j) xCoords
  print $ sum xmases

countInvolvingX_MAS :: [[Char]] -> Int -> Int -> Int
countInvolvingX_MAS grid row col = length $ filter id [validRange && a && c, validRange && a && d, validRange && b && c, validRange && b && d]
  where
    rows = length grid
    cols = length $ head grid
    validRange = row-1>=0 && col-1>=0 && row+1<rows && col+1<cols
    a = (grid ! (row-1)) (col-1) == 'M' && (grid ! (row+1)) (col+1) == 'S'
    b = (grid ! (row-1)) (col-1) == 'S' && (grid ! (row+1)) (col+1) == 'M'
    c = (grid ! (row+1)) (col-1) == 'M' && (grid ! (row-1)) (col+1) == 'S'
    d = (grid ! (row+1)) (col-1) == 'S' && (grid ! (row-1)) (col+1) == 'M'

part2 :: String -> IO ()
part2 inp = do
  let grid = lines inp
      rows = length grid
      cols = length $ head grid
      coords = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
      aCoords = filter (\(i, j) -> (grid ! i) j == 'A') coords
      xmases = map (\(i, j) -> countInvolvingX_MAS grid i j) aCoords
  print $ sum xmases
