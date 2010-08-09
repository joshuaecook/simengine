% BRK
%   Demo model showing the Booth, Rinzel, and Kiehn Motoneuron
%   Model (1997, J Neurophys)
%
% Copyright 2010 Simatra Modeling Technologies
%
function brk

% Set parameters for the simulation protocol
max_Iapp = 25;     % nA/cm^2
end_time = 11000;  % ms

% Produce the input train, which is effectively a up and down
% current ramp
points = [0 0; 500 0; end_time/2 max_Iapp; end_time-500 0; end_time 0];
n = 0:end_time;
inp.Iext = {interp1(points(:,1),points(:,2), n)};

% Execute the simulation of the BRK motoneuron model
o = simex('brk_inputs.dsl', end_time, inp);

% Plot the voltage traces from the neural model
figure(1);
subplot(2,2,1);
simplot(o.V);
axis([0 end_time -80 60]);
title('Voltage Trace (with simulated 5-HT)');
ylabel('Voltage (mV)');
legend('V_s', 'V_d');

% Plot the input current as read by the model
subplot(2,2,3);
simplot(o.Iapp, 'r');
axis([0 end_time -5 30]);
title('Input Current Waveform');
xlabel('Time (ms)');
ylabel('Current (nA/cm^2)');

% Now, resampling the output somatic voltage and reformat 
% as an input to the spike detector model
% The spike detector model uses a normalized timestep of zero, so we need
% to rescale based on the timestep that will adequately show the spikes -
% 0.01 ms
dt = 0.01;
n2 = 0:dt:end_time;
inp2.Vm = {interp1(o.V(:,1),o.V(:,2),n2,'linear','extrap')};
o2 = simex('spikes.dsl', end_time/dt, inp2);

% Compute interspike intervals and spiking frequency from the spike data
spike_times = (o2.events(:,1)*dt)'; % rescale based on dt used when generating the input stream Vm
interspike_intervals = [inf diff(spike_times)]; % Units in ms
spike_frequency = 1000./interspike_intervals; % Units in Hz

% Sub-sample the input waveform to match the spike times
sampled_input = interp1(o.Iapp(:,1), o.Iapp(:,2), spike_times);

% Plot the spike frequencies as a function of input current.  Using
% different colors and different markers to distinguish between the
% rising current and the falling current
subplot(1,2,2);
rising = find(spike_times <= end_time/2);
falling = find(spike_times > end_time/2);
plot(sampled_input(rising), spike_frequency(rising), 'o', sampled_input(falling), spike_frequency(falling), '.')
title('F-I Curve')
xlabel('Current (nA/cm^2)');
ylabel('Spike Frequency (Hz)');
legend('Rising', 'Falling', 'Location', 'NorthWest')

end