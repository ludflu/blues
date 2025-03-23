module EuterpeaToLilyPond (convertToLilyPond) where

import Euterpea 
import Music.Lilypond qualified as LP
import Data.Ratio ((%))
import qualified Data.List as List

-- | Converts Euterpea Music data to LilyPond Music data
convertToLilyPond :: Music Pitch -> LP.Music
convertToLilyPond = musicToLilyPond

-- | Main conversion function for Music values
musicToLilyPond :: Music Pitch -> LP.Music
musicToLilyPond (Prim (Note d p)) = noteToLilyPond d p
musicToLilyPond (Prim (Rest d)) = restToLilyPond d
musicToLilyPond (m1 :+: m2) = LP.Sequential [musicToLilyPond m1, musicToLilyPond m2]
musicToLilyPond (m1 :=: m2) = LP.Simultaneous [musicToLilyPond m1, musicToLilyPond m2]
musicToLilyPond (Modify c m) = modifyToLilyPond c (musicToLilyPond m)

-- | Convert a note to LilyPond format
noteToLilyPond :: Dur -> Pitch -> LP.Music
noteToLilyPond d (pc, oct) = 
    LP.Note (pitchToLilyPond pc oct) (durationToLilyPond d) [] []

-- | Convert a rest to LilyPond format
restToLilyPond :: Dur -> LP.Music
restToLilyPond d = LP.Rest (durationToLilyPond d) [] []

-- | Convert a pitch to LilyPond format
pitchToLilyPond :: PitchClass -> Octave -> LP.Pitch
pitchToLilyPond pc oct = 
    let noteName = case pc of
            Cff -> LP.C
            Cf -> LP.Cis
            Dff -> LP.D
            Css -> LP.Cis
            C  -> LP.C
            Cs -> LP.Cis
            Df -> LP.Des
            D  -> LP.D
            Ds -> LP.Dis
            Ef -> LP.Ees
            Eff -> LP.Ees
            E  -> LP.E
            F  -> LP.F
            Fs -> LP.Fis
            Gf -> LP.Ges
            G  -> LP.G
            Gs -> LP.Gis
            Af -> LP.Aes
            A  -> LP.A
            As -> LP.Ais
            Bf -> LP.Bes
            B  -> LP.B
        -- Adjust octave (Euterpea uses different octave numbering than LilyPond)
        lilyOctave = oct - 4
    in LP.Pitch noteName Nothing lilyOctave

-- | Convert duration to LilyPond format
durationToLilyPond :: Dur -> LP.Duration
durationToLilyPond d =
    let (num, denom) = toRationalForm d
    in LP.Duration (fromIntegral num) (fromIntegral denom) Nothing []

-- | Convert a rational duration to a simple numerator/denominator form
toRationalForm :: Dur -> (Integer, Integer)
toRationalForm d =
    let r = toRational d
        num = numerator r
        denom = denominator r
    in (num, denom)

-- | Handle control modifications
modifyToLilyPond :: Control -> LP.Music -> LP.Music
modifyToLilyPond (Tempo r) m = 
    LP.Tempo (LP.TempoMM (fromRational r)) m
modifyToLilyPond (Transpose i) m =
    LP.Transposition (fromIntegral i) m
modifyToLilyPond (Instrument _) m = 
    -- LilyPond handles instruments differently, would need to be
    -- implemented based on specific needs
    m
modifyToLilyPond (KeySig pc mode) m =
    let tonality = case mode of
            Major -> LP.Major
            Minor -> LP.Minor
        key = pitchClassToLilyPondKey pc
    in LP.KeySignature key tonality m
modifyToLilyPond (Phrase attrs) m =
    foldr applyAttribute m attrs
  where
    applyAttribute :: PhraseAttribute -> LP.Music -> LP.Music
    applyAttribute (Dyn d) music = 
        let dynamic = case d of
                -- Map Euterpea dynamics to LilyPond dynamics
                -- These are approximations
                SF  -> LP.SforzandoPiano
                Ff  -> LP.ForteFortissimo
                Fff -> LP.ForteFortissimo
                F   -> LP.Forte
                MF  -> LP.MezzoForte
                MP  -> LP.MezzoPiano
                P   -> LP.Piano
                PP  -> LP.PianoPianissimo
                PPP -> LP.PianoPianissimo
        in LP.Dynamic dynamic music
    applyAttribute (Art a) music =
        let articulation = case a of
                Staccato _     -> LP.Staccato
                Legato _       -> LP.Legato
                Accent _       -> LP.Accent
                Marcato       -> LP.Marcato
                _             -> LP.Accent  -- Default for others
        in LP.Articulation articulation music
    applyAttribute _ music = music  -- Default case for other attributes

-- | Convert PitchClass to LilyPond key
-- pitchClassToLilyPondKey :: PitchClass -> LP.Key
pitchClassToLilyPondKey pc = case pc of
    C  -> LP.C
    Cs -> LP.Cis
    Df -> LP.Des
    D  -> LP.D
    Ds -> LP.Dis
    Ef -> LP.Ees
    E  -> LP.E
    F  -> LP.F
    Fs -> LP.Fis
    Gf -> LP.Ges
    G  -> LP.G
    Gs -> LP.Gis
    Af -> LP.Aes
    A  -> LP.A
    As -> LP.Ais
    Bf -> LP.Bes
    B  -> LP.B