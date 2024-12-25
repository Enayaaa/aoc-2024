module Day08 where

import Text.Parsec
import Control.Arrow (first, second)
import Data.List
import Data.Function
import qualified Data.Set as S

type Pos = (Int, Int)
data Antenna = Antenna { pos :: Pos, frequency :: Char} deriving (Show, Eq)
data Grid = Grid { antennas :: [Antenna], rows :: Int, cols :: Int } deriving (Show)

type Parser a = Parsec String Pos a

incRow :: Pos -> Pos
incRow = first (+1)

incCol :: Pos -> Pos
incCol = second (+1)

zeroCol :: Pos -> Pos
zeroCol = second (const 0)

(-|-) :: Pos -> Pos -> Pos
(r1, c1) -|- (r2, c2) = (r1 - r2, c1 - c2)

(+|+) :: Pos -> Pos -> Pos
(r1, c1) +|+ (r2, c2) = (r1 + r2, c1 + c2)

antennaP :: Parser Antenna
antennaP =  Antenna <$> getState <*> (alphaNum <|> char '.')

rowP :: Parser ([Antenna], Int)
rowP = do
  as <- many (antennaP <* modifyState incCol)
  (_, c) <- getState
  return (filter ((/= '.') . frequency) as, c)

gridP :: Parser Grid
gridP = do
  parsed <- (rowP <* modifyState (zeroCol . incRow)) `endBy` (char '\n') 
  (rs, _) <- getState
  return $ Grid {antennas = concatMap fst parsed, rows = rs, cols = snd $ head parsed}

parseGrid :: String -> Grid
parseGrid s = case runParser gridP (0, 0) "" s of
  Left err -> error $ show err
  Right g -> g

showGrid :: Grid -> S.Set Pos -> String
showGrid (Grid as rs cs) antiNodes = unlines $ map showRow [0..rs-1]
  where
    showRow r = map (showAntenna r) [0..cs - 1]
    showAntenna r c = case findAntenna r c of
      Just a -> frequency a
      Nothing -> case S.member (r, c) antiNodes of
        True -> '#'
        False -> '.'
    findAntenna r c = find (\a -> pos a == (r, c)) as

getAntiNodes :: Grid -> ((Antenna, Antenna) -> [Pos]) -> S.Set Pos
getAntiNodes g pairAntiNodes = S.fromList $ concatMap genAntiNodes freqPairs 
  where
    as = antennas g

    groupByFreq = groupBy ((==) `on` frequency) . sortBy (compare `on` frequency)
    unique_pairs l = [(a, b) | (a:bs) <- tails l, b <- bs]

    freqPairs = map (unique_pairs) (groupByFreq as)

    genAntiNodes :: [(Antenna, Antenna)] -> [Pos]
    genAntiNodes = concatMap pairAntiNodes

insideGrid :: Grid -> (Int, Int) -> Bool
insideGrid (Grid _ rs cs) (r, c) = r >= 0 && r < rs && c >= 0 && c < cs

part1 :: String -> IO ()
part1 inp = do
  let grid = parseGrid inp

      pairAntiNodes (a, b) = 
        let diff = pos b -|- pos a
         in filter (insideGrid grid) [pos a -|- diff, pos b +|+ diff]

      antiNodes = getAntiNodes grid pairAntiNodes

  print $ S.size antiNodes

part2 :: String -> IO ()
part2 inp = do
  let grid = parseGrid inp

      pairAntiNodes (a, b) = 
        let diff = pos b -|- pos a
            left = takeWhile (insideGrid grid) $ iterate (-|- diff) (pos a)
            right = takeWhile (insideGrid grid) $ iterate (+|+ diff) (pos b)
         in left ++ right

      antiNodes = getAntiNodes grid pairAntiNodes

  print $ S.size antiNodes
