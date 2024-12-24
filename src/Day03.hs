{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Day03 where

import Text.Regex.TDFA
import Text.Parsec

mulOpRegex :: String
mulOpRegex = "(mul\\([0-9]+,[0-9]+\\))"

type Parser a = Parsec String () a

parseMulOp :: Parser (Int, Int)
parseMulOp = do
  string "mul("
  x <- many1 digit
  string ","
  y <- many1 digit
  string ")"
  return (read x, read y)

performMulOp :: String -> Int
performMulOp s = case parse parseMulOp "" s of
  Left err -> undefined
  Right (x, y) -> x * y

part1 :: String -> IO ()
part1 inp = do
  let muls = getAllTextMatches (inp =~ mulOpRegex) :: [String]
      values = map performMulOp muls
      res = sum values
  print res


mulDoDontRegex :: String
mulDoDontRegex = "(do\\(\\)|don't\\(\\)|mul\\([0-9]+,[0-9]+\\))"

runOps :: [String] -> Int
runOps ops = go True 0 ops
  where
    go _ acc [] = acc
    go _ acc ("do()":xs) = go True acc xs
    go _ acc ("don't()":xs) = go False acc xs
    go False acc (_:xs) = go False acc xs
    go True acc (mulOp:xs) = go True (acc + performMulOp mulOp) xs

part2 :: String -> IO ()
part2 inp = do
  let muls = getAllTextMatches (inp =~ mulDoDontRegex) :: [String]
      res = runOps muls
  print res
