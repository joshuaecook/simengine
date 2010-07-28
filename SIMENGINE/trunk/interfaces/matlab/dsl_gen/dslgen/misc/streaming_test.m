function streaming_test

% Create a streaming model test
m = Model('streaming');

% Create a discrete iterator (fs = 1)
n = Iterator('discrete', 'sample_period', 1);

% Define an input x that will be streaming and will cause the simulation to
% stop when there is no more data
x = m.input('x', 0, 'iter', n, 'stop');

% Compute a n point moving averager
N = 50;
y = m.equ(conv(x(n), (1/N)*ones(1,N)));

% Output both the original and the filtered output
m.output('y', x, y);

% Perform the simulation
t = 0:0.01:100;
x_inputs = sin(2*pi*0.05*t) + rand(1,length(t));
o = simex(m, 10000, struct('x', x_inputs));

% Plot the result
simplot(o);
title(['Sine wave smoothed with ' num2str(N) ' point moving average'])

end