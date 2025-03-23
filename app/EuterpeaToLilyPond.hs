module EuterpeaToLilyPond (convertToLilyPond) where

import Euterpea qualified as E
import Music.Lilypond qualified as LP
import Data.Ratio ((%))
import qualified Data.List as List

-- | Converts Euterpea Music data to LilyPond Music data
convertToLilyPond :: E.Music E.Pitch -> LP.Music
convertToLilyPond = musicToLilyPond

-- | Main conversion function for Music values
musicToLilyPond :: E.Music E.Pitch -> LP.Music
musicToLilyPond (E.Prim (E.Note d p)) = noteToLilyPond d p
musicToLilyPond (E.Prim (E.Rest d)) = restToLilyPond d
musicToLilyPond (m1 E.:+: m2) = LP.Sequential [musicToLilyPond m1, musicToLilyPond m2]
musicToLilyPond (m1 E.:=: m2) = LP.simultaneous (musicToLilyPond m1) (musicToLilyPond m2)
musicToLilyPond (E.Modify c m) = modifyToLilyPond c (musicToLilyPond m)

-- | Convert a note to LilyPond format
noteToLilyPond :: E.Dur -> E.Pitch -> LP.Music
noteToLilyPond d (pc, oct) = 
    LP.Note (pitchToLilyPond pc oct) (durationToLilyPond d) [] []

-- | Convert a rest to LilyPond format
restToLilyPond :: E.Dur -> LP.Music
restToLilyPond d = LP.Rest (durationToLilyPond d) [] []

-- | Convert a pitch to LilyPond format
pitchToLilyPond :: E.PitchClass -> E.Octave -> LP.Pitch
pitchToLilyPond pc oct = 
    let noteName = case pc of
            E.Cff -> LP.C
            E.Cf -> LP.Cis
            E.Dff -> LP.D
            E.Css -> LP.Cis
            E.C  -> LP.C
            E.Cs -> LP.Cis
            E.Df -> LP.Des
            E.D  -> LP.D
            E.Ds -> LP.Dis
            E.Ef -> LP.Ees
            E.Eff -> LP.Ees
            E.E  -> LP.E
            E.F  -> LP.F
            E.Fs -> LP.Fis
            E.Gf -> LP.Ges
            E.G  -> LP.G
            E.Gs -> LP.Gis
            E.Af -> LP.Aes
            E.A  -> LP.A
            E.As -> LP.Ais
            E.Bf -> LP.Bes
            E.B  -> LP.B
        -- Adjust octave (Euterpea uses different octave numbering than LilyPond)
        lilyOctave = oct - 4
    in LP.Pitch noteName Nothing lilyOctave

-- | Convert duration to LilyPond format
durationToLilyPond :: E.Dur -> LP.Duration
durationToLilyPond d =
    let (num, denom) = toRationalForm d
    in LP.Duration (fromIntegral num) (fromIntegral denom) Nothing []

-- | Convert a rational duration to a simple numerator/denominator form
toRationalForm :: E.Dur -> (Integer, Integer)
toRationalForm d =
    let r = toRational d
        num = numerator r
        denom = denominator r
    in (num, denom)

-- | Handle control modifications
modifyToLilyPond :: E.Control -> LP.Music -> LP.Music
modifyToLilyPond (E.Tempo r) m = 
    LP.Tempo (LP.TempoMM (fromRational r)) m
modifyToLilyPond (E.Transpose i) m =
    LP.Transposition (fromIntegral i) m
modifyToLilyPond (E.Instrument _) m = 
    -- LilyPond handles instruments differently, would need to be
    -- implemented based on specific needs
    m
modifyToLilyPond (E.KeySig pc mode) m =
    let tonality = case mode of
            E.Major -> LP.Major
            E.Minor -> LP.Minor
        key = pitchClassToLilyPondKey pc
    in LP.KeySignature key tonality m
modifyToLilyPond (E.Phrase attrs) m =
    foldr applyAttribute m attrs
  where
    applyAttribute :: E.PhraseAttribute -> LP.Music -> LP.Music
    applyAttribute (E.Dyn d) music = 
        let dynamic = case d of
                -- Map Euterpea dynamics to LilyPond dynamics
                -- These are approximations
                E.SF  -> LP.SforzandoPiano
                E.Ff  -> LP.ForteFortissimo
                E.Fff -> LP.ForteFortissimo
                E.F   -> LP.Forte
                E.MF  -> LP.MezzoForte
                E.MP  -> LP.MezzoPiano
                E.P   -> LP.Piano
                E.PP  -> LP.PianoPianissimo
                E.PPP -> LP.PianoPianissimo
        in LP.Dynamic dynamic music
    applyAttribute (E.Art a) music =
        let articulation = case a of
                E.Staccato _     -> LP.Staccato
                E.Legato _       -> LP.Legato
                E.Accent _       -> LP.Accent
                E.Marcato       -> LP.Marcato
                _             -> LP.Accent  -- Default for others
        in LP.Articulation articulation music
    applyAttribute _ music = music  -- Default case for other attributes

-- | Convert PitchClass to LilyPond key
-- pitchClassToLilyPondKey :: PitchClass -> LP.Key
pitchClassToLilyPondKey pc = case pc of
    E.C  -> LP.C
    E.Cs -> LP.Cis
    E.Df -> LP.Des
    E.D  -> LP.D
    E.Ds -> LP.Dis
    E.Ef -> LP.Ees
    E.E  -> LP.E
    E.F  -> LP.F
    E.Fs -> LP.Fis
    E.Gf -> LP.Ges
    E.G  -> LP.G
    E.Gs -> LP.Gis
    E.Af -> LP.Aes
    E.A  -> LP.A
    E.As -> LP.Ais
    E.Bf -> LP.Bes
    E.B  -> LP.B