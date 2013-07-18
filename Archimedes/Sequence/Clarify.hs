module Archimedes.Sequence.Clarify(
           contains
         , mostly
         , allEq
         , find) where

import Archimedes.Sequence.Functional
import Archimedes.Common

import Data.List (isInfixOf)
import Data.Map (fromList, (!))
-- Local Functions

contains :: Eq a => [a] -> [a] -> Bool
contains = isInfixOf

mostly :: Eq a => [a] -> a -> Bool
mostly xs b = let times = count xs b in times > (length xs - times)

allEq :: Eq a => [a] -> a -> Bool
allEq [] _ = True
allEq (x:xs) b = (x == b) && allEq xs b

find :: Eq a => [(a,b)] -> a -> b
find xs b = if b `notElem` map fst xs
                      then snd $ head xs
                      else find' xs
  where
    find' ((d, c):ys) = if d == b then c else find' ys
