module Day15 where

import qualified Data.Map as M
import Text.Parsec
import Utils

data Dir = U | D | L | R
    deriving (Show, Eq)

data Object = Wall | Box | Robot | Air
    deriving (Show, Eq)

type Coord = (Int, Int)
type Grid = M.Map Coord Object

gridP :: Parser Grid
gridP = do
    rows <- (many (wall <|> empty <|> box <|> robot) `endBy1` newline)
    let posObjPair = [((r, c), x) | (r, row) <- zip [0 ..] rows, (c, x) <- zip [0 ..] row]
    return $ M.fromList $ posObjPair
  where
    wall = Wall <$ char '#'
    empty = Air <$ char '.'
    box = Box <$ char 'O'
    robot = Robot <$ char '@'

inputP :: Parser (Grid, [Dir])
inputP = do
    g <- gridP
    skipMany newline
    dirs <- many1 ((up <|> down <|> left <|> right) <* many newline)
    return (g, dirs)
  where
    up = U <$ char '^'
    down = D <$ char 'v'
    left = L <$ char '<'
    right = R <$ char '>'

parseInput :: String -> (Grid, [Dir])
parseInput inp = case runParser inputP () "" inp of
    Left err -> error (show err)
    Right res -> res

showGrid :: Grid -> String
showGrid g = unlines $ do
    let (rows, columns) = maximum $ M.keys g
    r <- [0 .. rows]
    return $ do
        c <- [0 .. columns]
        case g M.! (r, c) of
            Wall -> "#"
            Air -> "."
            Box -> "O"
            Robot -> "@"

step :: Coord -> Dir -> Coord
step (r, c) U = (r - 1, c)
step (r, c) D = (r + 1, c)
step (r, c) L = (r, c - 1)
step (r, c) R = (r, c + 1)

cascade :: Grid -> Coord -> Dir -> Maybe Grid
cascade g p d = do
    let current = g M.! p
        p' = step p d
        move = do
            grid' <- cascade g p' d
            return $ M.insert p' current $ M.insert p Air grid'
    next <- p' `M.lookup` g
    case next of
        Wall -> Nothing
        Box -> move
        Robot -> move
        Air -> return $ M.insert p' current $ M.insert p Air g

lookupRobotPos :: Grid -> Coord
lookupRobotPos g = fst $ head $ filter ((== Robot) . snd) $ M.toList g

gpsCoords :: Coord -> Int
gpsCoords (r, c) = r * 100 + c

part1 :: String -> IO ()
part1 inp = do
    let
        (initialGrid, instructions) = parseInput inp
        initialRobotPos = lookupRobotPos initialGrid
        (finalGrid, _) = foldl f (initialGrid, initialRobotPos) instructions
        f (g, r) d = (g', r')
          where
            (g', r') = case cascade g r d of
                Just g'' -> (g'', lookupRobotPos g'')
                Nothing -> (g, r)
        res = sum . map (gpsCoords . fst) . M.toList . M.filter (== Box) $ finalGrid
    print res

data WideObject = Wall' | BoxL | BoxR | Robot' | Air'
    deriving (Show, Eq)

type WideGrid = M.Map Coord WideObject

widen :: Grid -> WideGrid
widen g = M.fromList $ concat $ do
    (r, c) <- M.keys g
    let obj = g M.! (r, c)
    return $ case obj of
        Wall -> [((r, 2 * c), Wall'), ((r, 2 * c + 1), Wall')]
        Box -> [((r, 2 * c), BoxL), ((r, 2 * c + 1), BoxR)]
        Robot -> [((r, 2 * c), Robot'), ((r, 2 * c + 1), Air')]
        Air -> [((r, 2 * c), Air'), ((r, 2 * c + 1), Air')]

showWideGrid :: WideGrid -> String
showWideGrid g = unlines $ do
    let (rows, columns) = maximum $ M.keys g
    r <- [0 .. rows]
    return $ do
        c <- [0 .. columns]
        case g M.! (r, c) of
            Wall' -> "#"
            Air' -> "."
            BoxL -> "["
            BoxR -> "]"
            Robot' -> "@"

cascade' :: WideGrid -> Coord -> Dir -> Maybe WideGrid
cascade' g p d = do
    let current = g M.! p
        p' = step p d
        moveSingle = do
            grid' <- cascade' g p' d
            return $ M.insert p' current $ M.insert p Air' grid'
    next <- p' `M.lookup` g
    case next of
        Wall' -> Nothing
        BoxL -> do
            if d `elem` [R, L]
                then moveSingle
                else do
                    let right = step p' R
                    grid' <- cascade' g p' d
                    grid'' <- cascade' grid' right d
                    return $ M.insert right Air' $ M.insert p' current $ M.insert p Air' grid''
        BoxR -> do
            if d `elem` [R, L]
                then moveSingle
                else do
                    let left = step p' L
                    grid' <- cascade' g p' d
                    grid'' <- cascade' grid' left d
                    return $ M.insert left Air' $ M.insert p' current $ M.insert p Air' grid''
        Robot' -> moveSingle
        Air' -> return $ M.insert p' current $ M.insert p Air' g

lookupRobotPosWide :: WideGrid -> Coord
lookupRobotPosWide g = fst $ head $ filter ((== Robot') . snd) $ M.toList g

part2 :: String -> IO ()
part2 inp = do
    let
        (initialGrid, instructions) = parseInput inp
        initialWideGrid = widen initialGrid
        initialRobotPos = lookupRobotPosWide initialWideGrid
        (finalGrid, _) = foldl f (initialWideGrid, initialRobotPos) instructions
        f (g, r) d = (g', r')
          where
            (g', r') = case cascade' g r d of
                Just g'' -> (g'', step r d)
                Nothing -> (g, r)
        res = sum . map (gpsCoords . fst) . M.toList . M.filter (== BoxL) $ finalGrid
    print res
