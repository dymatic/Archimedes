module Archimedes.Tasking.Sanitize(
    allEq
  , sanitize) where

allEq :: Eq a => [a] -> a -> Bool
allEq x b = all (== b) x

sanitize :: [a] -> [a -> Bool] -> [a]
sanitize xs b = [ x | x <- xs, all ($ x) b ]
