module Archimedes.Sequence.Functional(
	filterBreak) where

filterBreak :: (a -> Bool) -> [a] -> [a]
filterBreak _ [] = []
filterBreak f (x:xs)
	| f x = x : filterBreak f xs
	| otherwise = []
