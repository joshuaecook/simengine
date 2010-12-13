% FN - FitzHugh-Nagumo neuron model on the GPU
%
% FitzHugh-Nagumo model of a simplified Hodgkin-Huxley neuron model
% Derived from FitzHugh R. (1955, 1961) and Nagumo J., et al. (1962)
%
% Copyright 2007-2010 Simatra Modeling Technolgies
%
function fn_gpu()

% Create the model
m = create_fn();

% Create a parameter set
input.I = num2cell(0:0.5:4);

% Simulate
o = m.simex(100, input, '-gpu', '-float');

% Plot
figure,
simplot(o(:).u);
title('FitzHugh-Nagumo Model');
%legend('u', 'w');

end

function m = create_fn()

m = Model('fn');

b0 = m.input('b0', 2);
b1 = m.input('b1', 1.5);
e = m.input('e', 0.1);
I = m.input('I', 2);

u = m.state(0);
w = m.state(0);

m.diffequ(u, u - u^3 / 3 - w + I);
m.diffequ(w, e * (b0 + b1 * u - w));

m.output(u);
m.output(w);

end