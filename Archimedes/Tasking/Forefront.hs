--Forefront is a Vanguard replacement. Modules are defined in resource blocks.
-- resource foo {
--  x = 3
--  y = 2
-- }
-- Forefront can parse this into a data structure that looks something like this:
-- [("foo",[("x",3),("y",2)])]

--Input to Forefront, in the form of foo.y, will return 2.

module Archimedes.Tasking.Forefront(
  readFF
    )where

import Archimedes.Sequence.Clarify -- For "Contains"
import Archimedes.Sequence.Functional -- For "Filterbreak"
import Archimedes.Sequence.Manipulate --For "afterlist"

readFF :: [String] -> (String,[(String,String)])
readFF (x:xs) = (name, resources)
  where name = (afterList (rmO x ' ') "resource ")
        sides = splitOn (compress xs) '='
        resources = map readResource xs
        readResource y = ((before y '='),after y '=')

--Intended for use on a whole file
readAllFF :: [String] -> [(String,[(String,String)])]
readAllFF [] = []
readAllFF (x:xs)
  | x `contains` "resource" = (readFF (x: (linesBetween (x:xs) ('{','}')))) : readAllFF xs
  | otherwise = readAllFF xs


--Helper Functions
linesBetween :: (Eq a) => [[a]] -> (a,a) -> [[a]]
linesBetween [] _ = []
linesBetween (x:xs) (a,b)
  | x `contains` [a] = filterBreak (\c -> (not $ contains c [b])) xs
  | otherwise = linesBetween xs (a,b)

--Remove multiple if equal to
rmO :: (Eq a) => [a] -> a -> [a]
rmO [] _ = []
rmO (x:xs) a 
  | x == a = x : (rmO (removeBreak (== a) xs) a)
  | otherwise = x : rmO xs a

--Removes all Multiples
rmM :: (Eq a) => [a] -> [a]
rmM [] = []
rmM (x:xs) = x : rmM (removeBreak (==x) xs)

--Every other element of a list
eO :: [a] -> Int -> [a]
eO [] _ = []
eO (x:xs) seed
  | seed == 0 = x : (eO xs 1)
  | otherwise = eO xs 0
                
compress :: [[a]] -> [a]
compress []  = []
compress (x:xs) = x ++ compress xs