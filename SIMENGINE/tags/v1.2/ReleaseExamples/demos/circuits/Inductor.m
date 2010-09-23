% Inductor - create a inductor element
%
% Model:
%  inputs: V1, V2, L
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = Inductor(t)

% Create a voltage source and assign it the passed in iterator
m = Model('Inductor', t);

% Add the voltage node inputs and a default resistance
L = m.input('L', 1e-9); % 1 nH by default
V1 = m.input('V1');
V2 = m.input('V2');

% Compute the inductance current by computing an integral of the voltage
% difference and scaling by the inverse of the reciprocal
% V = L*I'
% I = (1/L)*Int[V(t),t]
integral_V = m.state(0);
m.diffequ(integral_V, V2-V1);
I = (1/L)*integral_V;

% Define the output
m.output(I);

end