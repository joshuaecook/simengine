// Copyright (C) 2010 Simatra Modeling Technologies
// MIDI scale converter

// note = MIDI note number
// frequency = output frequency of the corresponding MIDI not number

model (frequency) = midiscale(note)
  input note with {iter=n, halt_when_exhausted}
  
  // Produces a note frequency for a note in the MIDI scale based on A4 (the 57th MIDI note) set to 440 Hz
  // Valid notes are in the range 5 to 123 (20Hz to 19.9kHz base frequencies)
  // The MIDI scale is 12-TET meaning the octave is broken into 12 equistant parts on a logarithmic scale

  equation frequency = {440 * (2^(1/12))^(note - 57) when note > 3 and note < 124, 0 otherwise}
  
  output frequency
end