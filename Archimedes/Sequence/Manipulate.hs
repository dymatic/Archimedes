module Archimedes.Sequence.Manipulate(
       sub
     , to
     , after
     , positions
     , pos
     , after
     , before
     , between
     , rm
     , remove
     , replace
) where

import Archimedes.Common
import Archimedes.Sequence.Clarify
-- Exported Functions
sub :: [a] -> Int -> [a]
sub [] _ = []
sub (x:xs) y
    | (y == 0) = xs
    | otherwise = sub xs $ dec y

to :: [a] -> Int -> [a]
to [] _ = []
to _ 0 = []
to (x:xs) y = x : to xs (dec y)

after :: (Eq a) => [a] -> a -> [a]
after a b = sub a $ pos a b

before :: (Eq a) => [a] -> a -> [a]
before a b = to a $ pos a b

between :: (Eq a) => [a] -> (a,a) -> [a]
between x (a,b) = before (after x a) b

rm :: (Eq a) => [a] -> a -> [a]
rm x y = filter (/= y) x

remove :: (Eq a) => [a] -> [a] -> [a]
remove [] _ = []
remove x a
    | not (x `contains` a) = x
    | (take la x) == a = remove (sub x $ dec la) a
    | otherwise = (head x) : remove (tail x) a
  where la = (length a)

replace :: (Eq a) => [a] -> (a,a) -> [a]
replace x (a,b) = map (\c -> if c == a then b else c) x

