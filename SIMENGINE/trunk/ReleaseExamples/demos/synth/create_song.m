% create_song - create a song model that will synthesize music
% based on an input stream of MIDI notes
%
% notes = streaming input of MIDI notes
% chords = streaming input of MIDI chord base notes
% music = mono stream of 48kHz PCM values that can be played directly to
%         a sound card
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_song()

% Instantiate a song model
m = Model ('song');

n_in = Iterator('n_in', 'discrete', 'sample_frequency', 64);
n_out = Iterator('n_out', 'discrete', 'sample_frequency', 48e3);

notes = m.input('notes', 'iter', n_in, 'stop');
chords = m.input('chords', 'iter', n_in, 'stop');

% Hold the last note/chord when released, ADSR does fade out
last_note = m.state('last_note', 0, 'iter', n_in);
last_chord = m.state('last_chord', 0, 'iter', n_in);

m.recurrenceequ(last_note, piecewise(notes, notes > 0, last_note))
m.recurrenceequ(last_chord, piecewise(chords, chords > 0, last_chord))

% Produce the freqencies of the base notes for the MIDI notes and chords
ms_note = m.submodel(create_midiscale);
ms_note.note = last_note;
ms_chord = m.submodel(create_midiscale);
ms_chord.note = last_chord;

% Produce the full harmonics for notes and chords
melody = m.submodel(create_harmonics(n_out));
melody.fundamental = ms_note.frequency;

the_chords = m.submodel(create_chord(n_out));
the_chords.tonic = ms_chord.frequency;

% Create separate ADSR envelopes for notes and chords
adsr_note = m.submodel(create_adsr(n_in, n_out));
adsr_note.key = notes;

adsr_chord = m.submodel(create_adsr(n_in, n_out));
adsr_chord.key = chords;

% Combine notes and chords into a single chanel mono output
music = 0.5 * ((melody.y * adsr_note.amplitude) + ...
               (the_chords.y * adsr_chord.amplitude));

% Create the output for the synthesized PCM waveform data
m.output(music, 'iter', n_out);

end