module Archimedes.Tasking.Algorithm(
    sort) where

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = lesser ++ [x] ++ greater
  where lesser  = sort [y | y <- xs, y <= x]
        greater = sort [y | y <- xs, y > x]
