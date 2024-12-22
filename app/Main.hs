module Main where

import Options.Applicative
import System.FilePath

import qualified Day1 (part1, part2)
import qualified Day2 (part1, part2)
import qualified Day3 (part1, part2)
import qualified Day4 (part1, part2)
import qualified Day5 (part1, part2)

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
            1 -> (Day1.part1, Day1.part2)
            2 -> (Day2.part1, Day2.part2)
            3 -> (Day3.part1, Day3.part2)
            4 -> (Day4.part1, Day4.part2)
            5 -> (Day5.part1, Day5.part2)
            _ -> undefined
    case p of
        1 -> part1 inp
        2 -> part2 inp
        _ -> putStrLn "Invalid part number"
