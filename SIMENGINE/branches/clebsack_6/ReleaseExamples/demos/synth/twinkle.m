% Copyright (C) 2010 Simatra Modeling Technologies
% Sample driver for song.dsl that plays 'Twinkle, twinkle, little star'

function twinkle()
% The song model samples inputs at 64Hz, the definitions provided
% below scale a quarter note such that there are 120 beats per
% minute. The ones() denote that the note is actively being
% sounded, the zeros() indicate that the note should be allowed to decay.
q = [ones(1,24) zeros(1,8)]; % Quarter note
h = [ones(1,56) zeros(1,8)]; % Half note
w = [ones(1,120) zeros(1,8)]; % Whole note

% Base note to set the Key.  The value 50 is the MIDI note for D4.
note = 50;
% Base note for the chords.  Play the chords an octave lower (12 half steps).
chord = note - 12;

% 'Twinkle, twinkle ...' melody phrase of the song
twinkle = [note*q note*q (note+7)*q (note+7)*q ...
           (note+9)*q (note+9)*q (note+7)*h ...
           (note+5)*q (note+5)*q (note+4)*q (note+4)*q ...
           (note+2)*q (note+2)*q note*h];

% 'Up a-bove the world so high, ...' melody phrase
diamond = [(note+7)*q (note+7)*q (note+5)*q (note+5)*q ...
           (note+4)*q (note+4)*q (note+2)*h ...
           (note+7)*q (note+7)*q (note+5)*q (note+5)*q ...
           (note+4)*q (note+4)*q (note+2)*h];

% The same two phrases above but the chords
chord_twinkle = [chord*w ...
                 (chord+5)*h chord*h ...
                 (chord+5)*h chord*h ...
                 (chord+7)*h chord*h];

chord_diamond = [chord*h (chord+5)*h ...
                 chord*h (chord+7)*h ...
                 chord*h (chord+5)*h ...
                 chord*h (chord+7)*h];

% The full song repeats the first phrase
i.notes = {[twinkle diamond twinkle]};
i.chords = {[chord_twinkle chord_diamond chord_twinkle]};

% Run the notes and chords through the song model. The time 24
% must be greater than or equal to the length of the song to prevent
% truncation. The song will stop automatically when the notes or
% chords inputs run out.
o = simex('song.dsl', 24, i);

% Play the resulting audio through the sound device, grabbing only
% the second column of the output (first column is the time values
% from simulation).  The resultant audio is sampled at 48kHz and
% should be played with the same frequency to produce accurate tones.
sound(o.music(:,2), 48000);

end

