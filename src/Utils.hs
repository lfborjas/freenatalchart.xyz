module Utils where

between :: Ord a => (a, a) -> a -> Bool
between (begin, end) x =
  x >= begin && x<= end

maybeBetween :: Ord a => (a, a) -> a -> Maybe a
maybeBetween range x = 
  if between range x then Just x else Nothing

-- from: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs
