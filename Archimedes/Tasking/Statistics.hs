module Archimedes.Tasking.Statistics(
    refPos
  , similarComposure
  , isSimilarWord
  , mostSimilarWord) where

import Archimedes.Sequence.Functional
import Archimedes.Sequence.Manipulate

import Archimedes.Tasking.Algorithm


refPos :: Eq a => [a] -> [b] -> a -> b
refPos a b c = b !! pos a c

score :: Eq a => [a -> a -> Bool] -> a -> a -> Int
score fl t1 t2 = length [x | x <- fl, x t1 t2]

-- EXPORTED FUNCTIONS -- there's no need to say these, we all see them at the top
similarComposure :: Eq a => [a] -> [a] -> [a] -> Bool
similarComposure a b c = abs (ws b - ws c) < 5
  where ws = sum . map (refPos a [1 .. length a])

evaluate :: String -> String -> Int
evaluate = score [ses, similarComposure all, similarLengths]
  where all = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'3'] ++ "[{]}\\|`~!@#$%^&*()_+-=,<.>/?"

hole = undefined
data Hole = Hole

isSimilarWord :: String -> String -> Bool
isSimilarWord x = (>= 2) . evaluate x

mostSimilarWord :: String -> [String] -> String
mostSimilarWord word list = refPos evalList list most
  where evalList = map (evaluate word) list
        most = minimum evalList

-- Helper Functions
ses :: String -> String -> Bool
ses a b = (head a == head b) && (last a == last b)

similarLengths :: [a] -> [a] -> Bool
similarLengths a b = (sc < l1) && (sc < l2)
  where l1 = length a
        l2 = length b
        sc = abs $ l1 - l2
