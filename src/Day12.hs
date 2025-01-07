module Day12 where

import qualified Data.Map as M
import qualified Data.Set as S
import Utils
-- import Debug.Trace
traceShow = flip const

type Pos = (Int, Int)
type Grid = M.Map Pos Char
type Region = S.Set Pos

parse :: String -> Grid
parse inp = M.fromList [((r, c), x) | (r, l) <- zip [0..] (lines inp), (c, x) <- zip [0..] l]

showGrid :: Grid -> String
showGrid g = unlines [[M.findWithDefault '.' (r, c) g | c <- [0..maxC]] | r <- [0..maxR]]
  where
    (maxR, maxC) = maximum $ M.keys g

neighbors :: Pos -> [Pos]
neighbors (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

extractRegion :: Grid -> Pos -> Region
extractRegion g p = go [p] S.empty
  where
    x = g M.! p
    go [] acc = acc
    go (p:ps) acc
      | p `elem` acc = go ps acc
      | otherwise = go (neighs ++ ps) (S.insert p acc)
      where
        neighs = [n | n <- neighbors p, M.findWithDefault '?' n g == x]

exploreRegions :: Grid -> [Region]
exploreRegions g = go positions (S.empty) []
  where
    positions = M.keys g
    go :: [Pos] -> S.Set Pos -> [Region] -> [Region]
    go [] _ acc = acc
    go (p:ps) visited acc
      | p `S.member` visited = go ps visited acc
      | otherwise = go ps visited' (region : acc)
      where
        region = extractRegion g p
        visited' = foldr S.insert visited region

regionSize :: Region -> Int
regionSize = S.size

regionParameter :: Region -> Int
regionParameter region = foldl (\acc p -> acc + length (parameter p)) 0 region
  where
    parameter p = filter (`notElem` region) (neighbors p)

regionCost :: Region -> Int
regionCost region = regionSize region * regionParameter region

totalCost :: Grid -> Int
totalCost g = sum $ fmap regionCost (exploreRegions g)

part1 :: String -> IO ()
part1 inp = do
  let grid = parse $ strip inp
  print $ totalCost grid

type Edge = (Pos, Pos) -- An edge is defined by two positions (start and end)

-- -- Get the boundary edges of a region
-- boundaryEdges :: Region -> S.Set Edge
-- boundaryEdges region = S.fromList $ concatMap edgesForPosition region
--   where
--     edgesForPosition p =
--       [(p, x) | x <- neighbors p, x `S.notMember` region]

-- boundary :: Region -> S.Set Pos
-- boundary region = (S.fromList $ concatMap neighbors (S.toList region)) S.\\ region

data Dir = U | D | L | R deriving (Show, Eq)

upperLeftCorner :: Region -> Pos
upperLeftCorner region = minimum region

turnR :: Dir -> Dir
turnR R = D
turnR D = L
turnR L = U
turnR U = R

turnL :: Dir -> Dir
turnL R = U
turnL U = L
turnL L = D
turnL D = R

step :: Pos -> Dir -> Pos
step (r, c) U = (r - 1, c)
step (r, c) D = (r + 1, c)
step (r, c) L = (r, c - 1)
step (r, c) R = (r, c + 1)

regionSides :: Region -> Int
regionSides region = go startingPos R 0 startingPos False
  where
    startingPos = upperLeftCorner region

    go p dir acc prev moved
      | p == startingPos && dir == R && moved  = traceShow (acc, p, dir, "end") acc
      | step p left `S.member` region && prev /= p = traceShow (acc, p, dir, "turn left") $ go p left (acc + 1) p True
      | p' `S.member` region =  traceShow (acc, p, dir, "step") $ go p' dir acc p True
      | otherwise = traceShow (acc, p, dir, "turn right") $ go p right (acc + 1) p True
      where
        p' = step p dir
        left = turnL dir
        right = turnR dir

corners :: Region -> Int
corners region = length $ filter (\p -> length ((S.fromList $ neighbors p) `S.intersection` region) == 2) (S.toList region)

regionCost' :: Region -> Int
regionCost' region = regionSize region * regionSides region

totalCost' :: Grid -> Int
totalCost' g = sum $ fmap regionCost' (exploreRegions g)

part2 :: String -> IO ()
part2 inp = do
  let grid = parse $ strip inp
  -- print $ totalCost' grid
      region = extractRegion grid (2,3)
      square = S.fromList [(0,0), (0,1), (1,0), (1,1)] -- should be 4
      lShape = S.fromList [(0,0), (1,0), (2,0), (2,1)] -- should be 6
  print $ upperLeftCorner region 
  print $ regionSides region
  print $ totalCost' grid
  print $ corners square
  print $ corners lShape
  -- print $ regionSides square
  -- print $ regionSides lShape
