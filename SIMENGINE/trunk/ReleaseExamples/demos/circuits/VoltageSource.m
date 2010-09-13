% VoltageSource - create a voltage source Model object
%
% Model:
%  inputs: V1, V2, Vs
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = VoltageSource(t)

% Create a voltage source and assign it the passed in iterator
m = Model('VoltageSource', t);

% Add the three inputs
Vs = m.input('Vs', 10); % default 10 V
V1 = m.input('V1');
V2 = m.input('V2');

% Create a small internal resistance (10 mOhm)
Rinternal = 0.01;
      
% Create a state for I, have it adjust as necessary to produce the desired voltage
I = m.state(0);
m.diffequ(I, ((V2-V1)-Vs)/Rinternal);

% Define the output
m.output(I);

end