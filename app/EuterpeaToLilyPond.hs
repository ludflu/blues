module EuterpeaToLilyPond (convertToLilyPond) where

import Euterpea qualified as E
import Music.Lilypond qualified as LP
-- import Data.Ratio ((%))
-- import qualified Data.List as List

-- | Converts Euterpea Music data to LilyPond Music data
convertToLilyPond :: E.Music E.Pitch -> LP.Music
convertToLilyPond = musicToLilyPond

-- | Main conversion function for Music values
musicToLilyPond :: E.Music E.Pitch -> LP.Music
musicToLilyPond (E.Prim (E.Note d p)) = noteToLilyPond d p
musicToLilyPond (E.Prim (E.Rest d)) = restToLilyPond d
musicToLilyPond (m1 E.:+: m2) = LP.Sequential [musicToLilyPond m1, musicToLilyPond m2]
musicToLilyPond (m1 E.:=: m2) = LP.simultaneous (musicToLilyPond m1) (musicToLilyPond m2)
-- musicToLilyPond (E.Modify c m) = modifyToLilyPond c (musicToLilyPond m)
musicToLilyPond (E.Modify c m) = musicToLilyPond m  -- Ignore control modifications for now

-- | Convert a note to LilyPond format
noteToLilyPond :: E.Dur -> E.Pitch -> LP.Music
noteToLilyPond d (pc, oct) = let n = LP.NotePitch (pitchToLilyPond pc oct)
                                 dur = Just $ durationToLilyPond d
                              in LP.Note (n Nothing) dur  []
    
    -- LP.Note (Just (durationToLilyPond d)) 
    -- LP.Note (LP.NotePitch (pitchToLilyPond pc oct)) (Just (durationToLilyPond d)) []

-- | Convert a rest to LilyPond format
restToLilyPond :: E.Dur -> LP.Music
restToLilyPond d = LP.Rest (Just $durationToLilyPond d) []

-- For double flat -2, flat -1, natural 0, sharp 1 and double sharp 2.

doubleFlat :: Int
doubleFlat = -2
flat = -1
natural = 0
doubleSharpsharp = 1
doubleSharp = 2
sharp = 1

pitchToLilyAccidental :: E.PitchClass -> LP.Accidental
pitchToLilyAccidental pc = case pc of
    E.Cs -> sharp
    E.Ds -> sharp
    E.Fs -> sharp
    E.Gs -> sharp
    E.As -> sharp
    E.Bs -> sharp
    E.Css -> doubleSharp
    E.Dss -> doubleSharp
    E.Fss -> doubleSharp
    E.Gss -> doubleSharp
    E.Ass -> doubleSharp
    E.Bss -> doubleSharp
    E.Cf -> flat
    E.Df -> flat
    E.Ef -> flat
    E.Gf -> flat
    E.Af -> flat
    E.Bf -> flat
    E.Cff -> doubleFlat
    E.Dff -> doubleFlat
    E.Eff -> doubleFlat
    E.Gff -> doubleFlat
    E.Aff -> doubleFlat
    E.Bff -> doubleFlat
    _    -> natural

pitchToLilyPitch :: E.PitchClass -> LP.PitchClass
pitchToLilyPitch pc = case pc of 
                        E.C -> LP.C
                        E.D -> LP.D
                        E.E -> LP.E
                        E.F -> LP.F
                        E.G -> LP.G
                        E.A -> LP.A
                        E.B ->  LP.B 
                        E.Cs -> LP.C
                        E.Ds -> LP.D
                        E.Fs -> LP.F
                        E.Gs -> LP.G
                        E.As -> LP.A
                        E.Bs ->  LP.B 
                        E.Css -> LP.C
                        E.Dss -> LP.D
                        E.Fss -> LP.F
                        E.Gss -> LP.G
                        E.Ass -> LP.A
                        E.Bss -> LP.B
                        E.Cf -> LP.C
                        E.Df -> LP.D
                        E.Ef -> LP.E
                        E.Gf -> LP.G
                        E.Af -> LP.A
                        E.Bf -> LP.B
                        E.Cff -> LP.C
                        E.Dff -> LP.D
                        E.Eff -> LP.E
                        E.Gff -> LP.G
                        E.Aff -> LP.A
                        E.Bff -> LP.B
                        E.Fff -> LP.F
                        E.Ff -> LP.F
                        E.Es -> LP.E
                        E.Ess -> LP.E

-- | Convert a pitch to LilyPond format
pitchToLilyPond :: E.PitchClass -> E.Octave -> LP.Pitch
pitchToLilyPond pc oct = 
    let noteName = pitchToLilyPitch pc
        accid = pitchToLilyAccidental pc
        -- Adjust octave (Euterpea uses different octave numbering than LilyPond)
        lilyOctave = oct - 4
    in LP.Pitch (noteName, accid, lilyOctave)

-- | Convert duration to LilyPond format
durationToLilyPond :: E.Dur -> LP.Duration
durationToLilyPond d = LP.Duration (toRational d)


-- | Handle control modifications
-- modifyToLilyPond :: E.Control -> LP.Music -> LP.Music
-- modifyToLilyPond (E.Tempo r) m = 
--     LP.Tempo (LP.TempoMM (fromRational r)) m
-- modifyToLilyPond (E.Transpose i) m =
--     LP.Transposition (fromIntegral i) m
-- modifyToLilyPond (E.Instrument _) m = 
--     -- LilyPond handles instruments differently, would need to be
--     -- implemented based on specific needs
--     m
-- modifyToLilyPond (E.KeySig pc mode) m =
--     let tonality = case mode of
--             E.Major -> LP.Major
--             E.Minor -> LP.Minor
--         key = pitchClassToLilyPondKey pc
--     in LP.KeySignature key tonality m
-- modifyToLilyPond (E.Phrase attrs) m =
--     foldr applyAttribute m attrs
--   where
--     applyAttribute :: E.PhraseAttribute -> LP.Music -> LP.Music
--     applyAttribute (E.Dyn d) music = 
--         let dynamic = case d of
--                 -- Map Euterpea dynamics to LilyPond dynamics
--                 -- These are approximations
--                 E.SF  -> LP.SforzandoPiano
--                 E.Ff  -> LP.ForteFortissimo
--                 E.Fff -> LP.ForteFortissimo
--                 E.F   -> LP.Forte
--                 E.MF  -> LP.MezzoForte
--                 E.MP  -> LP.MezzoPiano
--                 E.P   -> LP.Piano
--                 E.PP  -> LP.PianoPianissimo
--                 E.PPP -> LP.PianoPianissimo
--         in LP.Dynamic dynamic music
--     applyAttribute (E.Art a) music =
--         let articulation = case a of
--                 E.Staccato _     -> LP.Staccato
--                 E.Legato _       -> LP.Legato
--                 E.Accent _       -> LP.Accent
--                 E.Marcato       -> LP.Marcato
--                 _             -> LP.Accent  -- Default for others
--         in LP.Articulation articulation music
--     applyAttribute _ music = music  -- Default case for other attributes

-- | Convert PitchClass to LilyPond key
-- pitchClassToLilyPondKey :: PitchClass -> LP.Key
-- pitchClassToLilyPondKey pc = case pc of
--     E.C  -> LP.C
--     E.Cs -> LP.Cis
--     E.Df -> LP.Des
--     E.D  -> LP.D
--     E.Ds -> LP.Dis
--     E.Ef -> LP.Ees
--     E.E  -> LP.E
--     E.F  -> LP.F
--     E.Fs -> LP.Fis
--     E.Gf -> LP.Ges
--     E.G  -> LP.G
--     E.Gs -> LP.Gis
--     E.Af -> LP.Aes
--     E.A  -> LP.A
--     E.As -> LP.Ais
--     E.Bf -> LP.Bes
--     E.B  -> LP.B