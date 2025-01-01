module Day10 where

import Data.Char
import Data.List
import qualified Data.Map as M

import Utils

type Grid = M.Map (Int, Int) Int
type Pos = (Int, Int)

type Size = (Int, Int)

parse :: String -> Grid
parse inp = M.fromList $ [((r, c), ord x - ord '0') | (r, row) <- zip [0 ..] (lines inp), (c, x) <- zip [0 ..] row]

showGrid :: Grid -> Size -> String
showGrid g s = unlines [[chr $ g M.! (r, c) + ord '0' | c <- [0 .. w - 1]] | r <- [0 .. h - 1]]
  where
    (h, w) = s

neighbors :: Pos -> [Pos]
neighbors (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

trailHeads :: Grid -> [Pos]
trailHeads g = [p | p <- M.keys g, g M.! p == 0]

followTrails :: Grid -> [[Pos]]
followTrails g = map (\p -> go p (-1)) $ trailHeads g
  where
    go p prev
        | p `M.notMember` g = []
        | g M.! p == 9 && prev == 8 = [p]
        | g M.! p == prev + 1 = concat [go p' (prev + 1) | p' <- neighbors p]
        | otherwise = []

part1 :: String -> IO ()
part1 inp = do
    let reached = map nub $ followTrails $ parse $ strip inp
        scores = map length reached
        score = sum scores
    print $ score

part2 :: String -> IO ()
part2 inp = do
    let reached = followTrails $ parse $ strip inp
        ratings = map length reached
        rating = sum ratings
    print $ rating
