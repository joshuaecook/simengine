% create_pd_events - return a model object for the Pyloric Dilator STG
%
% Generate a PD model that can save spike information
%
% Lobster STG pacing neuron model
% Derived from Prinz et al, J Neurophysiol, December 2003
% Copyright 2008-2010 Simatra Modeling Technolgies, L.L.C.
%
function m = create_pd_events

% Evaluate with the existing pd model
pd_mdl = create_pd;

% Use the time iterator from the pd model
t = pd_mdl.timeIterator;
dt = t.dt;

% Create a new top-level model
m = Model('pd_events', t);

% Instantiate the pd model as a submodel
pd = m.submodel(pd_mdl);

% Define the output data start time
start_data_time = m.input('start_data_time', 10000);
pd.start_data_time = start_data_time;

% Promote the remaining inputs
m.input(pd, '-all');

% Pull out the voltage potential from the submodel
% wrap in a m.equ call since simEngine can not handle temporal references
% from submodel outputs
Vm = m.equ(pd.Vm);

% Define three metrics in state variables
last_spike_height = m.state('last_height', 0, 'iter', t);
last_spike_time = m.state('last_time', 0, 'iter', t);
last_spike_area = m.state('last_area', 0, 'iter', t);

% Define recurrence equations for each quantity
m.recurrenceequ(last_spike_area, ...
    piecewise(0, Vm(t-1) < -40 & Vm(t-2) >= -40, ...
    last_spike_area + (Vm(t-1)+40)*(dt/1000), Vm(t-1) >= -40 & Vm(t-2) <= -15, ...
    last_spike_area + 20*(dt/1000), Vm(t-1) > -15, ...
    last_spike_area));
m.recurrenceequ(last_spike_time, ...
                piecewise(m.time, Vm < Vm(t-1) & Vm(t-1) > Vm(t-2),...
                          last_spike_time));
m.recurrenceequ(last_spike_height, ...
                piecewise(Vm(t-1), Vm < Vm(t-1) & Vm(t-1) > Vm(t-2),...
                          last_spike_height));

% The start time for a simulation was defined as an input in create_pd, so
% just refer to it by name:
start_data_time = Exp('start_data_time');

% Create a condition for outputting data
output_data = (Vm < -40 & Vm(t-1) >= -40 & m.time > start_data_time);

m.output('metrics', last_spike_time, last_spike_height, last_spike_area, 'when', output_data);

end
