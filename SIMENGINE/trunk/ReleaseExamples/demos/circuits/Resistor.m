% Resistor - create a resistor element
%
% Model:
%  inputs: V1, V2, R
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = Resistor(t)

% Create a voltage source and assign it the passed in iterator
m = Model('Resistor', t);

% Add the voltage node inputs and a default resistance
R = m.input('R', 100); % 100 Ohm by default
V1 = m.input('V1');
V2 = m.input('V2');

% Return the current computed by Ohms law
I = (V2-V1)/R;

% Define the output
m.output(I);

end