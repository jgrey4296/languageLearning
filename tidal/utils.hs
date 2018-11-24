import qualified Sound.Tidal.Chords as Chords
import qualified Sound.Tidal.Scales as Scales
import Sound.Tidal.Context
import Sound.Tidal.MIDI.Context


-- Invert a Chord 
inversion i c = result
  where looped = cycle c
        offsets = zip looped (inversion_offsets i c)
        dropped = (take (length c) . drop i) offsets
        result = map (\(a,b)->a + b) dropped

inversion_offsets i c = offset
  where len_c = length c
        reps = map (take len_c . repeat) [0..(max len_c (1+i `div` len_c))]
        folded = foldl (++) [] reps
        offset = map (* 12) folded


mkChord s p = Chords.enchord [s] p 0

mkBass p = p - 12


mkChordAndBass s p1 p2 = stack [ mkChord s p1, mkBass p2 ]
