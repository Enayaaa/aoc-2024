module Utils where

import Text.Parsec (Parsec)
import qualified Data.Text as T (strip, pack, unpack)

type Parser a = Parsec String () a

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []    = []
splitOn c s     = h : splitOn c (drop 1 t)
  where
    (h, t) = break (==c) s

fromList :: [a] -> (a, a)
fromList [a, b] = (a, b)
fromList _      = error "fromList: list must have exactly two elements"

strip :: String -> String
strip = T.unpack . T.strip . T.pack

printGrid :: [String] -> IO ()
printGrid = mapM_ putStrLn
