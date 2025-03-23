module EuterpeaToLilyPond (convertToLilyPond,exportToLily) where

import Euterpea qualified as E
import Music.Lilypond qualified as LP
import Text.Pretty ( Pretty(pretty), runPrinter )


convertToLilyPond :: E.Music E.Pitch -> LP.Music
convertToLilyPond = musicToLilyPond

musicToLilyPond :: E.Music E.Pitch -> LP.Music
musicToLilyPond (E.Prim (E.Note d p)) = noteToLilyPond d p
musicToLilyPond (E.Prim (E.Rest d)) = restToLilyPond d
musicToLilyPond (m1 E.:+: m2) = LP.Sequential [musicToLilyPond m1, musicToLilyPond m2]
musicToLilyPond (m1 E.:=: m2) = LP.simultaneous (musicToLilyPond m1) (musicToLilyPond m2)
musicToLilyPond (E.Modify _ m) = musicToLilyPond m  -- Ignore control modifications for now

noteToLilyPond :: E.Dur -> E.Pitch -> LP.Music
noteToLilyPond d (pc, oct) = let n = LP.NotePitch (pitchToLilyPond pc oct)
                                 dur = Just $ durationToLilyPond d
                              in LP.Note (n Nothing) dur  []
    
restToLilyPond :: E.Dur -> LP.Music
restToLilyPond d = LP.Rest (Just $ durationToLilyPond d) []


flat::Int
flat = -1

doubleFlat :: Int
doubleFlat = -2

natural :: Int
natural = 0

sharp::Int   
sharp = 1

doubleSharp :: Int
doubleSharp = 2

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

pitchToLilyPond :: E.PitchClass -> E.Octave -> LP.Pitch
pitchToLilyPond pc octave = 
    let noteName = pitchToLilyPitch pc
        accidentals = pitchToLilyAccidental pc
    in LP.Pitch (noteName, accidentals, octave)

durationToLilyPond :: E.Dur -> LP.Duration
durationToLilyPond d = LP.Duration (toRational d)

exportToLily :: E.Music E.Pitch -> FilePath -> IO ()
exportToLily music path = let src = runPrinter $ pretty $ convertToLilyPond music
                           in writeFile path src


