{-# LANGUAGE MagicHash #-}

module Day07 where

import Text.Parsec
import Utils
import GHC.Integer.Logarithms ( integerLogBase# )
import GHC.Exts ( Int(..))
import Debug.Trace

data Equation = Equ {res :: Int, consts :: [Int]}
    deriving (Show)

parseEquation :: Parser Equation
parseEquation = do
    res <- many1 digit
    string ": "
    consts <- sepBy1 (many1 digit) (string " ")
    return $ Equ (read res) (map read consts)

parseEquations :: Parser [Equation]
parseEquations = do
    res <- sepBy1 parseEquation (char '\n')
    eof
    return res

parser :: String -> [Equation]
parser s = case parse parseEquations "" s of
    Left err -> error (show err)
    Right res -> res

genCombinations :: Int -> [a] -> [[a]]
genCombinations n xs = sequence (replicate n xs)

isPossible :: [Int -> Int -> Int] -> Equation -> Bool
isPossible ops (Equ res (n : ns)) = any ((== res) . applyOps) opss
  where
    opss = genCombinations (length ns) ops
    applyOps (f : fs) =
        let
            -- The constants are split into these variables and sublists
            --  [n , --------- ns ----------]
            --  [n , --------- ks -------, l]
            (ks, [l]) = splitAt (length fs) ns
            -- acccOp = n `f` k1 `g0` k2 `g1` ... `gn`
            accOp = foldl (\g (h, m) -> h (g m)) (f n) (zip fs ks)
         in
            accOp l -- n `f` k1 `g0` k2 `g1` ... `gn` l

part1 :: String -> IO ()
part1 inp = do
    let eqs = parser (strip inp)
        possibles = filter (isPossible [(+), (*)]) eqs
        sum = foldl (\acc (Equ r _) -> acc + r) 0 possibles
    print sum

concatOp :: Int -> Int -> Int
concatOp a b = a * 10^expon + b
  where expon = I# (integerLogBase# 10 (fromIntegral b)) + 1

part2 :: String -> IO ()
part2 inp = do
    let eqs = parser (strip inp)
        possibles = filter (isPossible [(+), (*), concatOp]) eqs
        sum = foldl (\acc (Equ r _) -> acc + r) 0 possibles
    print sum
