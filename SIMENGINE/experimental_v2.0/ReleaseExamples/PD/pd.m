% pd - model and simulate the STG pyloric dilator
%
% Lobster STG pacing neuron model
% Derived from Prinz et al, J Neurophysiol, December 2003
% Copyright 2008-2010 Simatra Modeling Technolgies, L.L.C.
%
function pd

% We'll create two plots, one with the basic pd model showing just the
% neuron in action, then we'll follow with a simulation of the pd model
% with the spikes saved

% We don't output until after t=10000
stop_time = 11500;

% Instantiate the two models
pd = create_pd;
pd_events = create_pd_events;

% Perform the two simulations
out1 = simex(pd, stop_time);
out2 = simex(pd_events, stop_time);

% Create the plot
figure,
subplot(2,1,1);
simplot(out1);
title('PD Model');
ylabel('Membrane Potential (Vm)');

subplot(2,1,2);
simplot(out2.Vm, [out2.metrics(:,2) out2.metrics(:,3)], 'o');
title('PD Events Model - see the captured spike data');
ylabel('Membrane Potential (Vm)');
xlabel('Time (ms)');

end