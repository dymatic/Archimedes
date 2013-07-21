module Archimedes.Sequence.Frequency(count,freqList,mostFrequent,qs) where

count :: (Eq a) => [a] -> a -> Int
count x c = sum $ map (\d -> if d == c then 1 else 0) x

qs :: [(a,Int)] -> [(a,Int)]
qs [] = []
qs ((a,x):xs) = lesser ++ [(a,x)] ++ greater
  where lesser  = qs [(y,z) | (y,z) <- xs, z <= x]
        greater = qs [(y,z) | (y,z) <- xs, z > x]

freqList :: (Eq a) => [a] -> [(a,Int)]
freqList x = map (\c -> (c,(count x c))) x

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

mostFrequent :: (Eq a) => [a] -> a 
mostFrequent x = fst . head . rev . qs $ freqList x
