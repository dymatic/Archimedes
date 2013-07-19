module Archimedes.Tasking.Case(
    isCap
  , isLower
  , capitalize
  , lowerrize
  , toCap
  , toLower) where

import Archimedes.Sequence.Functional
import qualified Data.Char as DC

isCap :: Char -> Bool
isCap = DC.isUpper

isLower :: Char -> Bool
isLower = DC.isLower

capitalize :: String -> String
capitalize = map DC.toUpper

lowerrize :: String -> String
lowerrize = map DC.toLower

-- Why is this repeated yet again?
toCap :: String -> String
toCap = capitalize

toLower :: String -> String
toLower = lowerrize
