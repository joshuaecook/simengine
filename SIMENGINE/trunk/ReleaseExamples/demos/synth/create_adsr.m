% create_adsr - create an ADSR Envelope (attack, decay, sustain, release)
%
% key = whether key is currently pressed or not, evaluated as a boolean
% attackSlope = how fast the peak is reached
% decaySlope = how fast the sustainLevel is reached after peaking
% sustainLevel = magnitude of signal after decay phase
% releaseSlope = how fast signal returns to 0 when key is released
% amplitude = output signal of signal strength
%
% Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
 
function m = create_adsr(n_in, n_out)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n_in = Iterator('n_in', 'discrete', 'sample_frequency', 64);
    n_out = Iterator('n_out', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate adsr model
m = Model('adsr');

% Create the inputs
key = m.input('key', 'iter', n_in, 'stop');
attackSlope = m.input('attackSlope', 15 * n_out.dt);
decaySlope = m.input('decaySlope', -2 * n_out.dt);
sustainLevel = m.input('sustainLevel', 0.75);
releaseSlope = m.input('releaseSlope', -5 * n_out.dt);

% Create the states
peaked = m.state('peaked', 0, 'iter', n_out);
amplitude = m.state('amplitude', 0, 'iter', n_out);

% Define the equations
peak = 0.95;

m.recurrenceequ(peaked, piecewise(0, key == 0, ...
                                  1, amplitude >= peak, ...
                                  peaked));

attacking = piecewise(1, (key ~= 0 & amplitude < peak & peaked == 0), ...
                      0);
sustaining = piecewise(1, (key ~= 0 & peaked == 1 & amplitude <= sustainLevel), ...
                       0);
decaying = piecewise(1, (key ~= 0 & (amplitude >= peak | peaked == 1) & ~sustaining), ...
                     0);
releasing = piecewise(1, key == 0 & amplitude > 0, ...
                      0);

dy = piecewise(attackSlope, attacking, ...
               0, sustaining, ...
               releaseSlope, releasing, ...
               decaySlope, decaying, ...
               0);

m.recurrenceequ(amplitude, amplitude + dy);

m.update(amplitude, 0.01, 'when', amplitude < 0.01);
m.update(amplitude, peak, 'when', amplitude > peak);
m.update(amplitude, sustainLevel, 'when', decaying & amplitude < sustainLevel);

m.output(amplitude, 'iter', n_out);

end
