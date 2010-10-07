%
% spike_detector - Create spike events from input voltage trace data
%
% Copyright 2010 Simatra Modeling Technologies

function m = create_spike_detector()

m = Model('spike_detector');

% set up a discrete iterator with steps of 0.01 milliseconds
i = Iterator('discrete', 'sample_period', 1);

% Define a sampled input for the voltage trace
Vm = m.input('Vm', 0, 'iter', i, 'halt');

% Detect a spike
spike_occurred = (Vm(i) > 20) & (Vm(i) < Vm(i-1)) & (Vm(i-1) >= Vm(i-2));

m.output('events', i, 'when', spike_occurred);

end
