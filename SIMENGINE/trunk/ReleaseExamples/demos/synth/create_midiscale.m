% create_midiscale - create a MIDI scale converter
%
% note = MIDI note number
% frequency = output frequency of the corresponding MIDI note number
%
% Produces a note frequency for a note in the MIDI scale based on A4 (the 57th MIDI note) set to 440 Hz
% Valid notes are in the range 5 to 123 (20Hz to 19.9kHz base frequencies)
% The MIDI scale is 12-TET meaning the octave is broken into 12 equistant parts on a logarithmic scale
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_midiscale(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 1);
end

% Instantiate a midiscale model
m = Model('midiscale');

% Create the inputs
note = m.input('note', 'iter', n);

% Equations
frequency = piecewise(440 * (2^(1/12))^(note - 57), note > 3 & note < 124, 0);

% Create an output frequency
m.output(frequency)

end