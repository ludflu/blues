module Main where

import Euterpea ( Pitch, Music, en, line, play )
import Scales ( repeatOctave, dblues )
import EuterpeaToLilyPond ( exportToLily )


allblues :: [Music Pitch]
allblues = repeatOctave (dblues en)

front :: [Music Pitch]
front = take 7 allblues
back :: [Music Pitch]
back =  reverse  front

frontAndBack :: Music Pitch
frontAndBack = line $ front ++ drop 1 back


main :: IO ()
main = do
  print frontAndBack
  play frontAndBack
  exportToLily frontAndBack "output.llp"

