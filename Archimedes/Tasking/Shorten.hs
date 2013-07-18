module Archimedes.Tasking.Shorten(
    mass
  , unmass) where

import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Functional
import Archimedes.Sequence.Clarify

import Archimedes.Common

mass :: String -> String
mass [] = []
mass (x:"") = x:""
mass a@(x:y:xs)
	| x == y = concat ["(", rl, [x], ")", mass $ removeBreak (== x) a]
        | otherwise = x : mass (y:xs)
  where rl = show $ length $ filterBreak (== x) a

unmass :: String -> String
unmass [] = []
unmass (x:"") = x:""
unmass a@('(':y:ys) = replicate nOf (last (before a ')')) ++ unmass (after a ')')
  where nOf = read $ tail $ init $ before a ')' -- read is not cool
unmass (x:ys) = x : unmass ys
