module Utils where

second :: (b->c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

first :: (a->c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ []    = []
splitOn c s     = h : splitOn c (drop 1 t)
  where
    (h, t) = break (==c) s

fromList :: [a] -> (a, a)
fromList [a, b] = (a, b)
fromList _      = error "fromList: list must have exactly two elements"
