% create_harmonics - create a Harmonic generator from Low Frequency
% Oscillators
%
% fundamental = frequency of fundamental tone
% y = output waveform of note with harmonics superimposed

function m = create_harmonics(n)

if nargin == 0
    % Create a discrete iterator if one is not passed into the function
    n = Iterator('n', 'discrete', 'sample_frequency', 48e3);
end

% Instantiate a harmonics model

m = Model('harmonics');

% Create the inputs
fundamental = m.input('fundamental', 55);

% Instantiate the submodels
NUM_HARMONICS = 10;

% Fundamental tone
fundamental_tone = m.submodel(create_lfo(n));
fundamental_tone.w = fundamental;

upper_harmonics = m.submodel(create_lfo(n), NUM_HARMONICS-1);
lower_harmonics = m.submodel(create_lfo(n), NUM_HARMONICS-1);

for multiple = 2:NUM_HARMONICS
  % Upper harmonics
  upper_harmonics{multiple-1}.w = fundamental * multiple;
  % Lower harmonics
  lower_harmonics{multiple-1}.w = fundamental / multiple;
end

% Harmonic superposition
y = 1/2 * fundamental_tone.y;
% Add each successive harmonic pair with a lesser amplitude, such that
% the total amplitude is <= 1
for multiple = 2:NUM_HARMONICS
  y = y + ((0.5 * (1/(2^multiple))) * (upper_harmonics{multiple-1}.y + ...
                                       lower_harmonics{multiple-1}.y));
end

% Create an output for y, the generated wave
m.output(y)

end