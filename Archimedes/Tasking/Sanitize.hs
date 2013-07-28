module Archimedes.Tasking.Sanitize(
    allEq
  , sanitize) where

import Archimedes.Common
import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Manipulate

sanitize :: [a] -> [a -> Bool] -> [a]
sanitize xs b = [ x | x <- xs, all ($ x) b ]

command :: (Eq a, Eq b) => a -> b -> [(b,(a -> c))] -> c
command x y z = ((map snd z) !! (head $ positions (map fst z) y)) x

