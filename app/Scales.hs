
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Scales where


import Data.Modular ( ℤ, type (/), unMod, toMod, ℤ, toMod )

import Euterpea
    ( Music,
      Pitch,
      wn,
      Primitive(Note),
      PitchClass,
      Music(Prim),
      shiftPitches,
      chord,
      absPitch,
      pitch,
      note,
      PitchClass(C, Cs, D, E, F, G, A, B) )

import qualified Data.Set as Set

data Mood = Major | Minor deriving Show

data ScaleDegree = Tonic | Supertonic | Mediant | Subdominant | Dominant | Submediant | Leading
 deriving (Eq, Ord, Enum, Show)

data Degree = I | II | III | IV | V | VI | VII
   deriving (Eq, Ord, Enum, Show)

newtype ScaleFamily = ScaleFamily [Interval] deriving Show
newtype Progression = Progression [(Degree,Mood)] deriving Show

type Interval = ℤ / 12
--type Interval = Integer

whole :: Interval
whole = 2
half :: Interval
half = 1

major= ScaleFamily [whole, whole, half, whole, whole, whole, half, whole]
minor= ScaleFamily [whole, half, whole, whole, half, whole, whole, whole]

moodToPattern :: Mood -> ScaleFamily
moodToPattern Major = major
moodToPattern Minor = minor

majorPentatonic= ScaleFamily [whole, whole, minorThird, whole, majorThird]
minorPentatonic= ScaleFamily [minorThird, whole, whole, minorThird, whole]

minorBlues = ScaleFamily [ minorThird, whole, half, half, minorThird, whole]

ionian =     ScaleFamily [whole, whole, half, whole, whole, whole, half]
dorian =     ScaleFamily [whole, half, whole, whole, whole, half, whole]
phrygian =   ScaleFamily [half, whole, whole, whole, half, whole,whole]
lydian =     ScaleFamily [whole, whole, whole, half, whole, whole, half]
mixolydian = ScaleFamily [whole, whole, half, whole, whole, half, whole]
aeolian =    ScaleFamily [whole, half, whole, whole, half, whole, whole]
locrian =    ScaleFamily [half, whole, whole, half, whole, whole, whole]

mixolydianPentatonic= ScaleFamily [whole, whole, minorThird, minorThird, whole]
phrygianPentatonic= ScaleFamily [half, whole, minorThird, half, minorThird]
diminishedPentatonic= ScaleFamily [whole, half, minorThird, half, minorThird]

unison :: Interval
unison = 0

minorSecond :: Interval
minorSecond = 1

majorSecond :: Interval
majorSecond = 2

minorThird :: Interval
minorThird = 3

majorThird :: Interval
majorThird = 4

perfectFourth :: Interval
perfectFourth = 5

tritone :: Interval
tritone = 6

augmentedFourth :: Interval
augmentedFourth = 6

diminishedFifth :: Interval
diminishedFifth = 6

perfectFifth :: Interval
perfectFifth = 7

minorSixth :: Interval
minorSixth = 8

majorSixth :: Interval
majorSixth = 9

minorSeventh :: Interval
minorSeventh = 10

majorSeventh :: Interval
majorSeventh = 11

octave :: Interval
octave  = 12

--leaving out the perfectFourth from the list of perfect and consonant Intervals
-- is that the right thing to do?
perfection :: Set.Set Interval
perfection = Set.fromList [unison, octave, perfectFifth]
dissonantIntervals :: Set.Set Interval
dissonantIntervals = Set.fromList [minorSecond, majorSecond, tritone, minorSeventh, majorSeventh]
consonantIntervals :: Set.Set Interval
consonantIntervals = Set.fromList [ unison, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, octave]

patternToSemitones :: [Interval] -> [Interval]
patternToSemitones pat = init $ scanl1 (+) (0:pat)

modToInt :: Interval -> Int
modToInt = fromIntegral . unMod

intToMod :: Int -> Interval
intToMod i = toMod $ fromIntegral i

-- given a scale pattern, a root note, and a duration, return the scale of notes
makeScale :: ScaleFamily -> Pitch -> Rational -> [Music Pitch]
makeScale (ScaleFamily ptn) p d = let f ap = note d (pitch (absPitch p + ap))
                                      semis = patternToSemitones ptn
                                      integerSemitones = map modToInt semis
                                   in map f integerSemitones

-- endlessly repeat the scale raising the octave each time
repeatOctave :: [Music Pitch] ->  [Music Pitch]
repeatOctave musiclist = let octaves = [0,12..]
                             shiftTargets = map (replicate (length musiclist) ) octaves
                             shifters = map shiftPitches $ concat shiftTargets
                          in zipWith ($) shifters $ cycle musiclist

makeMajorScale :: Pitch -> Rational -> [Music Pitch]
makeMajorScale = makeScale major

makeMinorScale :: Pitch -> Rational -> [Music Pitch]
makeMinorScale = makeScale minor

makeDiatonicScale :: Mood -> Pitch -> Rational -> [Music Pitch]
makeDiatonicScale Major = makeMajorScale
makeDiatonicScale Minor = makeMinorScale


cmajor :: Rational -> [Music Pitch]
cmajor = makeMajorScale (C,4)
dmajor :: Rational -> [Music Pitch]
dmajor = makeMajorScale (D,4)
emajor :: Rational -> [Music Pitch]
emajor = makeMajorScale (E,4)
fmajor :: Rational -> [Music Pitch]
fmajor = makeMajorScale (F,4)
gmajor :: Rational -> [Music Pitch]
gmajor = makeMajorScale (G,4)
amajor :: Rational -> [Music Pitch]
amajor = makeMajorScale (A,4)
bmajor :: Rational -> [Music Pitch]
bmajor = makeMajorScale (B,4)

cminor :: Rational -> [Music Pitch]
cminor = makeMinorScale (C,4)
dminor :: Rational -> [Music Pitch]
dminor = makeMinorScale (D,4)
eminor :: Rational -> [Music Pitch]
eminor = makeMinorScale (E,4)
fminor :: Rational -> [Music Pitch]
fminor = makeMinorScale (F,4)
gminor :: Rational -> [Music Pitch]
gminor = makeMinorScale (G,4)
aminor :: Rational -> [Music Pitch]
aminor = makeMinorScale (A,4)
bminor :: Rational -> [Music Pitch]
bminor = makeMinorScale (B,4)

cminorPent :: Rational -> [Music Pitch]
cminorPent = makeScale minorPentatonic (C,4)
cmajorPent :: Rational -> [Music Pitch]
cmajorPent = makeScale majorPentatonic (C,4)
cSmajorPent :: Rational -> [Music Pitch]
cSmajorPent = makeScale majorPentatonic (Cs,4)

cblues :: Rational -> [Music Pitch]
cblues = makeScale minorBlues (C,4)
dblues :: Rational -> [Music Pitch]
dblues = makeScale minorBlues (D,4)

getPitch :: Music Pitch -> (PitchClass,Int)
getPitch (Prim (Note d p)) = p

-- an index into the scale
getIndexes :: [Int] -> [Int]
getIndexes = map (`mod` 7)

--how many semitones to raise
getOctaveShift :: [Int] -> [Int]
getOctaveShift = map ((* 12) . (`div` 7))

calculateInterval' :: Pitch -> Pitch -> Interval
calculateInterval' p1  p2 = let i  = abs (absPitch p1 - absPitch p2)
                            in toMod $ fromIntegral i

calculateInterval :: [Pitch] -> [Pitch] -> [Interval]
calculateInterval = zipWith calculateInterval'


calculateAbsInterval' :: Pitch -> Pitch -> Int
calculateAbsInterval' p1  p2 = abs (absPitch p1 - absPitch p2)

calculateAbsInterval :: [Pitch] -> [Pitch] -> [Int]
calculateAbsInterval = zipWith calculateAbsInterval'


isConsonant :: Pitch -> Pitch -> Bool
isConsonant p1 p2 = Set.member (calculateInterval' p1 p2) consonantIntervals

calcConsanance' :: Music Pitch -> Music Pitch -> Bool
calcConsanance' m1 m2 = let p1 = getPitch m1
                            p2 = getPitch m2
                        in isConsonant p1 p2

calcConsanance :: [Music Pitch] -> [Music Pitch] -> [Bool]
calcConsanance = zipWith calcConsanance'


getNotes :: Pitch -> Mood -> Rational -> [Int] -> [Music Pitch]
getNotes key mood duration degrees = let scale = makeDiatonicScale mood key duration
                                         pindices = getIndexes degrees -- an index into the scale
                                         notes = map (scale !!) pindices
                                         shifterAmounts = getOctaveShift degrees --how many semitones to raise
                                         shifters = map shiftPitches shifterAmounts  --an array of pitch shifter functions
                                         shiftedNotes = zipWith ($) shifters notes
                                      in shiftedNotes

mkTriad :: Int -> [Int]
mkTriad n = [n, n+2, n+4]

mkSeventh :: Int -> [Int]
mkSeventh n = [n, n+2, n+4, n+6]

makeMajorChord :: Music Pitch -> Music Pitch
makeMajorChord p = chord [p, shiftPitches (modToInt (whole+whole)) p, shiftPitches (modToInt (whole+whole+half+whole)) p]

makeMinorChord :: Music Pitch -> Music Pitch
makeMinorChord p = chord [p, shiftPitches (modToInt (whole+half)) p, shiftPitches (modToInt (whole+half+whole+whole)) p]

makeChord :: Mood -> Music Pitch -> Music Pitch
makeChord Major = makeMajorChord
makeChord Minor = makeMinorChord

fromDegree :: Degree -> Int
fromDegree = fromEnum

makeChordProgression :: [(Degree,Mood)] -> Pitch -> ScaleFamily -> [Music Pitch]
makeChordProgression progression key scaleFamily = let scale = makeScale scaleFamily key wn
                                                       degrees = map fst progression
                                                       moods = map snd progression
                                                       notes = map (\degree -> scale !! fromDegree degree) degrees
                                                       prog = zip notes moods
                                                    in map (\(d,m) -> makeChord m d) prog



