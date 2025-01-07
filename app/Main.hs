module Main where

import Options.Applicative
import System.FilePath

import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import qualified Day03 (part1, part2)
import qualified Day04 (part1, part2)
import qualified Day05 (part1, part2)
import qualified Day06 (part1, part2')
import qualified Day07 (part1, part2)
import qualified Day08 (part1, part2)
import qualified Day09 (part1, part2)
import qualified Day10 (part1, part2)
import qualified Day11 (part1, part2)
import qualified Day12 (part1, part2)
import qualified Day13 (part1, part2)
import qualified Day14 (part1, part2)

data Options = Options
    { day   :: Int
    , part  :: Int
    , input :: Int
    }

options :: Parser Options
options = Options
    <$> option auto
        ( long "day"
       <> short 'd'
       <> metavar "DAY"
       <> help "Day of the puzzle" )
    <*> option auto
        ( long "part"
       <> short 'p'
       <> metavar "PART"
       <> help "Part of the puzzle" )
    <*> option auto
        ( long "input"
       <> short 'i'
       <> metavar "INPUT"
       <> value 0
       <> help "Input file number, 0 is for main input (NN.txt)" )

inputsDir :: FilePath
inputsDir = "inputs"

inputFile ::
    -- | Day
    Int ->
    -- | Input number, 0 is main input
    Int ->
    String
inputFile d 0
    | d < 10 = "0" ++ show d ++ ".txt"
    | otherwise = show d ++ ".txt"
inputFile d n
    | d < 10 = "0" ++ show d ++ "-" ++ show n ++ ".txt"
    | otherwise = show d ++ "-" ++ show n ++ ".txt"

main :: IO ()
main = do
    Options d p i <- execParser $ info (options <**> helper)
        ( fullDesc
       <> progDesc "Advent of Code 2024"
       <> header "Advent of Code 2024" )
    inp <- readFile $ inputsDir </> inputFile d i
    let (part1, part2) =
          case d of
            1 -> (Day01.part1, Day01.part2)
            2 -> (Day02.part1, Day02.part2)
            3 -> (Day03.part1, Day03.part2)
            4 -> (Day04.part1, Day04.part2)
            5 -> (Day05.part1, Day05.part2)
            6 -> (Day06.part1, Day06.part2')
            7 -> (Day07.part1, Day07.part2)
            8 -> (Day08.part1, Day08.part2)
            9 -> (Day09.part1, Day09.part2)
            10 -> (Day10.part1, Day10.part2)
            11 -> (Day11.part1, Day11.part2)
            12 -> (Day12.part1, Day12.part2)
            13 -> (Day13.part1, Day13.part2)
            14 -> (Day14.part1, Day14.part2)
            _ -> (const $ putStrLn "Invalid day", const $ putStrLn "Invalid day")
    case p of
        1 -> part1 inp
        2 -> part2 inp
        _ -> putStrLn "Invalid part number"
