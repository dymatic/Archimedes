module Archimedes.Tasking.Vanguard(
    readVGLine
  , readVGProperty
  , readVGFile) where

import Archimedes.Sequence.Remove
import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Functional

prepend :: a -> [(a,a)] -> [(a,a,a)]
prepend _ [] = []
prepend x ((b,c):ys) = (x,b,c) : prepend x ys

linesBetween :: (Eq a) => [[a]] -> ([a],[a]) -> [[a]]
linesBetween (x:xs) (a,b)
  | x `contains` a = tail $  hf (x:xs) (a,b) True
  | otherwise = linesBetween xs (a,b)
    where hf (y:ys) (c,d) f
            | and [(y `contains` d), f] = []
            | (null ys) = []
            | otherwise = y : hf ys (c,d) True

readVGLine :: String -> String
readVGLine x = vgSanitize $ (afterList x "module ")

readVGProperty :: String -> (String,String)
readVGProperty x = ((before (removeBreak (==' ') x) ' '), (afterList x "is "))
                    
vgSanitize :: String -> String
vgSanitize x = (rm (rm x '{') '}')

--                          module variab value
readVGFile :: [String] -> [(String,String,String)]
readVGFile [] = [] -- No Hack needed
readVGFile (x:xs)
  | x `contains` "module" = (prepend mName (rm (map (readVGProperty) (linesBetween (x:xs) ("{","}"))) ("",""))) ++ readVGFile rest
  | otherwise = readVGFile xs
    where mName = readVGLine x
          rest = (removeBreak (\c -> c `contains` "}") xs)
          

