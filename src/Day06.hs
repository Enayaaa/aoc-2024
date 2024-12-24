module Day06 where

import Control.DeepSeq
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import System.IO.Unsafe
import Control.Arrow (second)

data Dir = U | D | L | R deriving (Show, Eq, Ord)

type Pos = (Int, Int)
type Grid = [[Char]]
type Visits = S.Set (Int, Int)

(!) :: Grid -> Pos -> Char
grid ! (r, c) = grid !! r !! c

markGrid :: Grid -> [Pos] -> Char -> Grid
markGrid grid ps ch = [ [ if (r, c) `elem` ps then ch else grid ! (r, c) | c <- [0 .. length (head grid) - 1] ] | r <- [0 .. length grid - 1] ]

isObstacle :: Grid -> Pos -> Bool
isObstacle grid pos = grid ! pos `notElem` ['.', '^']

walk :: Dir -> Pos -> Pos
walk U (r, c) = (r - 1, c)
walk D (r, c) = (r + 1, c)
walk L (r, c) = (r, c - 1)
walk R (r, c) = (r, c + 1)

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

isOutside :: Grid -> Pos -> Bool
isOutside grid (r, c) = r < 0 || r >= length grid || c < 0 || c >= length (head grid)

leavesGrid :: Grid -> Pos -> Dir -> Bool
leavesGrid grid pos dir = isOutside grid (walk dir pos)

canStep :: Grid -> Pos -> Bool
canStep grid p = not (isOutside grid p || isObstacle grid p)

traceRoute :: Grid -> Pos -> Dir -> Visits
traceRoute grid (r, c) dir
    | leavesGrid grid (r, c) dir = S.empty
    | otherwise =
        if leavesGrid grid newPos dir
            then S.fromList steps
            else S.fromList steps `S.union` (traceRoute grid newPos (turnRight dir))
  where
    steps = takeWhile (canStep grid) $ iterate (walk dir) (r, c)
    newPos = last steps

startingPos :: Grid -> Pos
startingPos grid =
    second fromJust
        . head
        . dropWhile (isNothing . snd)
        $ zip [0 ..]
        $ map col grid
  where
    col r = lookup '^' (zip r [0 ..])

printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn

part1 :: String -> IO ()
part1 inp = do
    let grid = lines inp
        res = traceRoute' grid (startingPos grid) U S.empty
    print $ S.size res

stumbles :: Grid -> Pos -> Dir -> Bool
stumbles grid pos dir = isObstacle grid (walk dir pos)

traceRoute' :: Grid -> Pos -> Dir -> Visits -> Visits
traceRoute' grid pos dir visited
  | leavesGrid grid pos dir = pos `S.insert` visited
  | stumbles grid pos dir = traceRoute' grid pos (turnRight dir) visited
  | otherwise = traceRoute' grid newPos dir (pos `S.insert` visited)
  where
    newPos = walk dir pos

type Visits' = S.Set (Pos, Dir)


-- loops :: Grid -> Pos -> Dir -> Visits' -> Bool
-- loops grid pos dir visited = (pos, dir) `S.member` visited

loops :: Grid -> Pos -> Dir -> Visits' -> Bool
loops grid pos dir visited
  | leavesGrid grid pos dir = False
  | (pos, dir) `S.member` visited = True
  | stumbles grid pos dir = loops grid pos (turnRight dir) visited
  | otherwise = loops grid newPos dir ((pos, dir) `S.insert` visited)
  where
    newPos = walk dir pos

traceRoute'' :: Grid -> Pos -> Dir -> Visits' -> Bool -> Pos -> Visits
traceRoute'' grid pos dir visited check pos'
  | leavesGrid grid pos dir = S.empty
  -- | (pos, dir) `S.member` visited = S.singleton pos'
  | stumbles grid pos dir = traceRoute'' grid pos right visited check pos'
  | check = (if isLoop then S.singleton newPos else S.empty) `S.union` traceRoute'' grid newPos dir visited' True pos'
  | otherwise = traceRoute'' grid newPos dir visited' False pos'
  where
    newPos = walk dir pos
    visited' = (pos,dir) `S.insert` visited

    right = turnRight dir
    grid' = markGrid grid [newPos] 'O'
    isLoop = {- traceShow (unsafePerformIO (printGrid grid')) $ traceShowId $  -}loops grid' (walk right pos) right S.empty

part2 :: String -> IO ()
part2 inp = do
    let grid = lines inp
    --     res = traceRoute' grid (startingPos grid) U S.empty
    -- print $ S.size res
        startPos = startingPos grid
        res = S.delete startPos $ traceRoute'' grid startPos U S.empty True startPos
        grid' = markGrid grid (S.toList res) 'O'
    printGrid grid'
    print $ S.size res

foo :: Grid -> Pos -> Dir -> Visits' -> Visits
foo grid pos dir visited
  | leavesGrid grid pos dir = S.empty
  | stumbles grid pos dir = foo grid pos (turnRight dir) visited
  | (pos, turnRight dir) `S.member` visited = S.singleton newPos
  | isLoop = S.insert newPos $ foo grid newPos dir ((pos, dir) `S.insert` visited)
  | otherwise = foo grid newPos dir ((pos, dir) `S.insert` visited)
  where
    newPos = walk dir pos

    grid' = markGrid grid [newPos] 'O'
    isLoop = loops grid' pos dir visited


part2' :: String -> IO ()
part2' inp = do
    let grid = lines inp
        startPos = startingPos grid
        res = foo grid startPos U S.empty
        grid' = markGrid grid (S.toList res) 'O'
    printGrid grid'
    print $ S.size res
    print $ startPos `S.member` res
