module Archimedes.Sequence.Clarify(
	positions
	,pos
	,contains) where

-- Local Functions
lengths :: (Eq a) => [a] -> a -> [Int]
lengths [] _ = []
lengths f@(b:d) e
            | b == e = (length f) : lengths (d) e
            | otherwise = lengths d e

positions :: (Eq a) => [a] -> a -> [Int]
positions x a = map (\c -> (length x) -c) $ lengths x a

pos :: (Eq a) => [a] -> a -> Int
pos a b = head $ (if (null x) then [0] else x)
    where x = positions a b

contains :: (Eq a)=> [a] -> [a] -> Bool
contains [] _ = False
contains x a
    | (take (length a) x) == a = True
    | otherwise = contains (tail x) a
