module Main where

import Euterpea
import Scales
melody = [
     d 5 hn, a 5 hn ,
     a 5 hn, d 5 qn, b 5 qn,
     b 5 hn, a 5 hn,
     g 5 hn, b 5 qn, d 5 qn]  --D A B G



allblues :: [Music Pitch]
allblues = repeatOctave (dblues qn)

front :: [Music Pitch]
front = take 8 allblues
back :: [Music Pitch]
back =  reverse   front

frontAndBack :: Music Pitch
frontAndBack = line $ front ++ drop 1 back

main :: IO ()
main = do print frontAndBack
          play frontAndBack

