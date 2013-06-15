module Archimedes.Tasking.Sanitize(
    allEq
  , sanitize) where

allEq :: (Eq a) => [a] -> a -> Bool
allEq x b = length x == (length $ (filter (== b) x))

sanitize :: [a] -> [(a -> Bool)] -> [a]
sanitize a b = [x | x <- a, (allEq (map (\c -> c x)b) True)] 
