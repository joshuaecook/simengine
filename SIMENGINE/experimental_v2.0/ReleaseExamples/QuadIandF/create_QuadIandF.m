% create_QuadIandF - Izhikevich Quadratic Integrate and Fire
%
% Izhikevich, E.M., Simple Model of Spiking Neurons, IEEE Trans on Neural Networks, Nov 2003
% Adapted for use with simEngine
% 
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C

function m = create_QuadIandF

% create a model object
m = Model('IandF');
m.solver = 'forwardeuler';
m.dt = 0.1;

% there are four parameters of this model
a = m.input('a', 0.02);
b = m.input('b', 0.2);
c = m.input('c', -65);
d = m.input('d', 2);

% plus one current input that must be specified
I = m.input('I');

% and two states
v = m.state(-65); % membrane potential
u = m.state(0);   % membrane recovery variable

% now, define the differential equations
m.diffequ(v, 0.04*v^2 + 5*v + 140 - u + I);
m.diffequ(u, a*(b*v - u));

% when a threshold is met, we need to reset the states.  this is
% done with an update equation
threshold = v >= 30;
m.update(v, c, 'when', threshold);
m.update(u, u + d, 'when', threshold);

% the output of the model is the membrane potential
m.output(v);

end