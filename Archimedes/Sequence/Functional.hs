module Archimedes.Sequence.Functional(
	  filterBreak
        , count
        , unit
        , removeBreak
        , first) where

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak = takeWhile

removeBreak :: (a -> Bool) -> [a] -> [a]
removeBreak = dropWhile

-- There's identicaly named function in ‘Util’
count :: Eq a => [a] -> a -> Int
count xs c = length $ filter (== c) xs

unit :: [a -> Bool] -> a -> Int -> Bool
unit fs b c = count (map ($ b) fs) True >= c

first :: (a -> Bool) -> [a] -> a
first p (b:bs) = if p b then b else first p bs
