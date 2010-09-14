% create_chord - create a chord synthesizer from multi-tone harmonics
%
% tonic = frequency of base note of chord
% third, fifth = half steps above base note for additional chord notes
% y = output waveform of note with chord harmonics superimposed
%
% Notes are generated in 12-TET, see create_midiscale.m for more
% information
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

function m = create_chord(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a chord model

m = Model('chord');

% Create the inputs
tonic = m.input('tonic', 110); % 110Hz = A2
third = m.input('third', 4);   % major third
fifth = m.input('fifth', 7);   % major fifth

% Instantiate the submodels
root = m.submodel(create_harmonics(n));
root.fundamental = tonic;

three = m.submodel(create_harmonics(n));
three.fundamental = (2^(1/12))^third * tonic;

five = m.submodel(create_harmonics(n));
five.fundamental = (2^(1/12))^fifth * tonic;

octave = m.submodel(create_harmonics(n));
octave.fundamental = 2*tonic;

% Harmonic superposition
y = 1/4 * (root.y + three.y + five.y + octave.y);

% Create an output for y, the generated wave
m.output(y)

end