% create_lfo - create a Low Frequency Oscillator
%
% w = frequency
% phi = phase angle
% y = output waveform, which is set to 0 if frequency is above 20kHz
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
%
function m = create_lfo(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a lfo model
m = Model('lfo');

% Create the inputs
w = m.input('w');
phi = m.input('phi', 0);

% Define the equations
t = Exp(n); % convert the iterator 'n' into a 't' that can be used in calculations
r = 2*t*pi;
y = piecewise(sin(w * r + phi), w <= 2e4 | w > 0, ...
              0);

% Create an output for y, the generated wave
m.output(y);
          
end