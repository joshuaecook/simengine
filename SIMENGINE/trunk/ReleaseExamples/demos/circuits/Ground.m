% Ground - create a ground source
%
% Model:
%  inputs: V1
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = Ground(t)

% Create a voltage source and assign it the passed in iterator
m = Model('Ground', t);

% Add the voltage node to tie to ground
V1 = m.input('V1');

% Create a small internal resistance (10 mOhm)
Rinternal = 0.01;
      
% Create a state for I, have it adjust as necessary to produce the desired voltage
I = m.state(0);
m.diffequ(I, -V1/Rinternal);

% Define the output
m.output(I);

end