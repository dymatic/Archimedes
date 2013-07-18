module Archimedes.Tasking.Replace(
         replace
       , replaceList) where

import Archimedes.Sequence.Manipulate
import Archimedes.Common

-- You really need to stop with tuples. You're throwing away (easy) currying
-- Consider if you wanted a partial function that always replaces 'X'. You'd
-- most like have to use the ‘curry’ function here.
replace :: Eq a => [a] -> (a, a) -> [a]
replace x (a, b) = map (\c -> if c == a then b else c) x

replaceList :: Eq a =>  [a] -> ([a], [a]) -> [a]
replaceList [] _ = []
replaceList x@(y:ys) (a,b)
  | mdep x == a = b ++ replaceList (sub x $ dec mlen) (a,b)
  | otherwise = y : replaceList ys (a,b)
    where mlen = length a
          mdep = take mlen
