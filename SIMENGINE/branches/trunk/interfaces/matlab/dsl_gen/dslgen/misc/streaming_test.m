function streaming_test

% Create a streaming model test
m = Model('streaming');

% Create a discrete iterator (fs = 1)
n = Iterator('n', 'discrete', 'sample_period', 1);

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
tic
o = simex(m, 10000, struct('x', x_inputs));
disp(['Finished serial streaming simulation in ' num2str(toc) ' seconds']);

% Plot the result
figure(1);
simplot(o);
title(['Sine wave smoothed with ' num2str(N) ' point moving average'])

% Now perform a parallel test
fs = 0.01:0.01:0.4;
x_inputs = cell(1,length(fs));
for i=1:length(fs)
    x_inputs{i} = sin(2*pi*fs(i)*t) + 0.5*rand(1,length(t));
end
tic
o = simex(m, 10000, struct('x', {x_inputs}));
disp(['Finished parallel streaming simulation in ' num2str(toc) ' seconds']);
figure(2);
for i=1:4
    subplot(2,2,i)
    simplot(o(i))
    title(['Sine wave with fs=' num2str(fs(i)) ' smoothed']);
end

end