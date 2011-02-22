// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
// Song player

// notes = midi valued stream of notes sampled at 64Hz
// chords = midi valued stream of base notes for major chords sampled at 64Hz
// music = mono output sampled at 48kHz

import "midiscale.dsl"
import "harmonics.dsl"
import "chord.dsl"
import "adsr.dsl"

model (music) = song(notes, chords)
// Set sampling rates for inputs and outputs
iterator n_in with {discrete, sample_period=1/64}
iterator n_out with {discrete, sample_period=1/48e3}

input notes with {iter=n_in, halt_when_exhausted}
input chords with {iter=n_in, halt_when_exhausted}

// Hold the last note/chord when released, ADSR does fade out
state last_note = 0
state last_chord = 0
equation last_note = {notes when notes > 0, last_note otherwise}
equation last_chord = {chords when chords > 0, last_chord otherwise}

// Produce the freqencies of the base notes for the MIDI notes and chords
submodel midiscale ms_note with {note=last_note}
submodel midiscale ms_chord with {note=last_note}

// Produce the full harmonics for notes and chords
submodel harmonics melody with {fundamental=ms_note.frequency}
submodel chord c1 with {tonic=ms_chord.frequency}

// Create separate ADSR envelopes for notes and chords
submodel adsr adsr_note with {key=notes}
submodel adsr adsr_chord with {key=chords}

// Combine notes and chords into a single chanel mono output
output music[n_out] =  0.5*(melody.y * adsr_note.amplitude) + 0.5*(c1.y * adsr_chord.amplitude)

solver = forwardeuler
solver.dt = 1/48e3

end