module Archimedes.Common(
	inc
      , dec
      , sqr
      , flatten
      , look) where

dec :: Int -> Int
dec x = x - 1

inc :: Int -> Int
inc x = x + 1

sqr x = x * x

flatten :: [[a]] -> [a]
flatten = concat

look :: Eq a => [(a,b)] -> a -> b
look [] _ = error "Not contained within tupple"
look ((a, b):xs) c
  | a == c = b
  | otherwise = look xs c
