% Kalman Filter 
% Creates a Kalman filter that converges into an estimate of a constant value
% x over 50 iterations. 
%
% Example adapted from http://www.scipy.org/Cookbook/KalmanFiltering
% written by Andrew D. Straw, which was a Python implementation of an
% example given by Greg Welch & Gary Bishop
% (http://www.cs.unc.edu/~welch/kalman/kalmanIntro.html)
%
% Copyright 2010 Simatra Modeling Technologies
function kalman()

n = Iterator('discrete', 'sample_period', 1);
m = Model('kalman', n);
n_iter = 50;
x = -0.37737;

z = m.random('normal', 'mean', x, 'stddev', 0.1);

Q = m.input('Q',1e-5); % process variance
R = m.input('R',0.1^2); % measurement variance

% two states
xhat = m.state(0);
P = m.state(1);
test = m.state(1);

% time update
xhatminus = xhat;
Pminus = P+Q;

% measurement update
K = Pminus/(Pminus+R);
m.recurrenceequ(xhat, xhatminus+K*(z-xhatminus));
m.recurrenceequ(P, (1-K)*Pminus);
m.recurrenceequ(test, 1+test);

% define the outputs
m.output(z);
m.output(xhat);
m.output(x);
m.output(Pminus);

% run the simulation
o = simex(m, n_iter);

% plot the results
subplot(2,1,1);
%plot(o.z(:,1), o.z(:,2), 'k+', o.xhat(:,1), o.xhat(:,2), 'b-', o.x(:,1), o.x(:,2), 'g');
simplot(o.z, 'k+', o.xhat, 'b-', o.x, 'g');
legend('noisy measurements', 'a posteri estimate', 'truth value');
xlabel('Iteration');
ylabel('Voltage');
title('Kalman Filter Example');

subplot(2,1,2);
plot(o.Pminus(2:end,1), o.Pminus(2:end,2));
legend('a priori error estimate');
xlabel('Iteration');
ylabel('Voltage^2');


end