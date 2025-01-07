{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day13 where

import Data.Ratio
import Text.Parsec
import Utils

data Puzzle = Puzzle
    { buttonA :: (Integer, Integer) -- (x+?, y+?)
    , buttonB :: (Integer, Integer) -- (x+?, y+?)
    , prize :: (Integer, Integer) -- (x==?, y==?)
    }
    deriving (Show)

buttonP :: Char -> Parser (Integer, Integer)
buttonP l = do
    string ("Button " ++ [l, ':']) *> skipMany space
    x <- string "X+" *> many digit <* char ',' <* spaces
    y <- string "Y+" *> many digit <* newline
    return (read x, read y)

prizeP :: Parser (Integer, Integer)
prizeP = do
    string "Prize:" *> spaces
    x <- string "X=" *> many digit <* char ',' <* spaces
    y <- string "Y=" *> many digit <* newline
    return (read x, read y)

puzzleP :: Parser Puzzle
puzzleP = Puzzle <$> buttonP 'A' <*> buttonP 'B' <*> prizeP

parseInput :: String -> [Puzzle]
parseInput inp = case parse (puzzleP `endBy` (many newline)) "" inp of
    Left err -> error (show err)
    Right res -> res

type Row = [Rational]
type Matrix = [Row]

-- | Gaussian elimination
gauss :: Matrix -> Matrix
gauss m = fixlastrow $ foldl reduceRow m [0 .. length m - 1]
  where
    -- swap element at position a   with element at potion b.
    swap xs a b
        | a > b = swap xs b a
        | a == b = xs
        | a < b =
            let (p1, p2) = splitAt a xs
                (p3, p4) = splitAt (b - a - 1) (tail p2)
             in p1 ++ [xs !! b] ++ p3 ++ [xs !! a] ++ tail p4

    -- reduce row n
    reduceRow ml r =
        let
            -- first non-zero element on or below (r,r)
            firstnonzero = head $ filter (\x -> ml !! x !! r /= 0) [r .. length ml - 1]

            -- matrix with row swapped (if needed)
            matrix2 = swap ml r firstnonzero

            -- row to normalize
            row = matrix2 !! r

            -- normalized row
            row1 = map (/ row !! r) row

            -- subtract nr from row1 while multiplying by factor
            subrow nr = let k = nr !! r in zipWith (-) nr (map (* k) row1)

            -- matrix with row r reduced
            nextrows = map subrow $ drop (r + 1) matrix2
         in
            take r matrix2 ++ [row1] ++ nextrows

    fixlastrow m' =
        let
            lastrow = last m'
            z = last lastrow
            nz = last (init lastrow)
         in
            init m' ++ [init (init lastrow) ++ [1, z / nz]]

-- | Substitute the solution back into the matrix
substitute :: Matrix -> Row
substitute m = foldr next [last (last m)] (init m)
  where
    next row oldsol =
        let subpart = init $ drop (length m - length oldsol) row
            sol = last row - sum (zipWith (*) subpart oldsol)
         in sol : oldsol

-- | Solve a system of linear equations
solveLin :: Matrix -> Row
solveLin = substitute . gauss

isInteger :: Rational -> Bool
isInteger r = denominator r == 1

toMatrix :: Puzzle -> Matrix
toMatrix (Puzzle (xA, yA) (xB, yB) (xR, yR)) =
    [ [f xA, f xB, f xR]
    , [f yA, f yB, f yR]
    ]
  where
    f = toRational

computeCost :: Row -> Integer
computeCost xs@[a, b]
    | any (not . isInteger) xs = error "computeCost: not all values are integers"
    | otherwise = 3 * numerator a + 1 * numerator b

part1 :: String -> IO ()
part1 inp = do
    print
        . sum
        . map computeCost
        . filter (all isInteger)
        . map (solveLin . toMatrix)
        $ parseInput inp

adjustPrize :: Puzzle -> Puzzle
adjustPrize (Puzzle a b (x, y)) = Puzzle a b (x + adjustment, y + adjustment)
  where
    adjustment = 10000000000000

part2 :: String -> IO ()
part2 inp = do
    print
        . sum
        . map computeCost
        . filter (all isInteger)
        . map (solveLin . toMatrix . adjustPrize)
        $ parseInput inp
