module Day14 where

import Data.Function
import Data.List
import Debug.Trace
import Text.Parsec
import Utils

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Vel = (Int, Int)
type Robot = (Coord, Vel)

intP :: Parser Int
intP = do
    sign <- option "" (string "-")
    n <- many1 digit
    return $
        if sign == "-"
            then (-read n)
            else (read n)

tupleP :: Parser (Int, Int)
tupleP = (,) <$> intP <*> (char ',' *> intP)

lineP :: Parser (Coord, Vel)
lineP = (,) <$> (string "p=" *> tupleP <* space) <*> (string "v=" *> tupleP)

parseInput :: String -> [Robot]
parseInput s = case runParser (lineP `endBy` newline) () "" s of
    Left err -> error (show err)
    Right res -> res

gridSize = (101, 103)

countE :: (Eq a) => a -> [a] -> Int
countE x = length . filter (x ==)

showGrid :: [Robot] -> String
showGrid rs = unlines $ do
    let (gx, gy) = gridSize
    y <- [0 .. gy - 1]
    return $ do
        x <- [0 .. gx - 1]
        let c = countE (x, y) (map fst rs)
        if c > 0
            then "#"
            else " "

tick :: Robot -> Robot
tick ((x, y), v@(vx, vy)) = (((x + vx) `mod` gx, (y + vy) `mod` gy), v)
  where
    (gx, gy) = gridSize

quadrantsPopulation :: [Robot] -> (Int, Int, Int, Int)
quadrantsPopulation rs = foldl f (0, 0, 0, 0) $ filter (not . middle) rs
  where
    (gx, gy) = gridSize
    middle ((x, y), _) = x == gx `div` 2 || y == gy `div` 2
    f (q1, q2, q3, q4) ((x, y), _) =
        if x < gx `div` 2
            then
                if y < gy `div` 2
                    then (q1 + 1, q2, q3, q4)
                    else (q1, q2, q3 + 1, q4)
            else
                if y < gy `div` 2
                    then (q1, q2 + 1, q3, q4)
                    else (q1, q2, q3, q4 + 1)

safetyFactor :: (Num a) => (a, a, a, a) -> a
safetyFactor (q1, q2, q3, q4) = q1 * q2 * q3 * q4

part1 :: String -> IO ()
part1 inp = do
    let initialState = parseInput inp
        simulate = iterate (map tick)
        finalState = simulate initialState !! 100
    print . safetyFactor $ quadrantsPopulation finalState

robotX :: Robot -> Int
robotX ((x, _), _) = x

robotY :: Robot -> Int
robotY ((_, y), _) = y

maxHorizontalStraitLine :: [Robot] -> Int
maxHorizontalStraitLine rs = maximum $ map (maximum . hasStraitLine' 0 []) sorted
  where
    grouped = groupBy ((==) `on` robotY) $ sortBy (compare `on` robotY) rs
    sorted = map sort grouped

    hasStraitLine' :: Int -> [Int] -> [Robot] -> [Int]
    hasStraitLine' n maxes [] = n : maxes
    hasStraitLine' n maxes [r] = n : maxes
    hasStraitLine' n maxes (r1 : r2 : rs) =
        if abs (robotX r1 - robotX r2) <= 1
            then hasStraitLine' (n + 1) maxes (r2 : rs)
            else hasStraitLine' 0 (n : maxes) (r2 : rs)

part2 :: String -> IO ()
part2 inp = do
    let initialState = parseInput inp
        simulate = iterate (map tick)

        clearScreen = putStr "\ESC[2J"
        prettyPrint :: Int -> [Robot] -> IO ()
        prettyPrint i rs = do
            clearScreen
            putStrLn $ "******************************** After " ++ show i ++ " seconds ********************************"
            putStr $ showGrid rs
            _ <- getLine -- look at the current state, press enter to continue
            return ()

        candidates = filter ((> 10) . maxHorizontalStraitLine . snd) $ zip [0 ..] $ simulate initialState

    sequence_ (map (uncurry prettyPrint) candidates)
