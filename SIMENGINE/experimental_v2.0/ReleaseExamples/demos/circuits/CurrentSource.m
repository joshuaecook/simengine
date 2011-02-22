% CurrentSource - create a current source
%
% Model:
%  inputs: V1, V2, Is
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = CurrentSource(t)

% Create a voltage source and assign it the passed in iterator
m = Model('CurrentSource', t);

% Add the voltage node inputs, even though they won't be used
Is = m.input('Is', 0.1); % 100 mA by default
V1 = m.input('V1');
V2 = m.input('V2');

% Return the current directly irrespective of the voltages
I = Is;

% Define the output
m.output(I);

end