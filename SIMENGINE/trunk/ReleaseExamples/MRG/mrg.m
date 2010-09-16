% mrg - MRG double axon myelinated cable model
%
% McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
% Publication: Modeling the Excitability of Mammalian Nerve Fibers:
%              Influence of Afterpotentials on the Recovery Cycle
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%
function mrg

% Create the model with a set number of segments
numSegments = 3;
m = createMRGAxon(numSegments);

% Simulate the model
out = simex(m, 200);

% Plot the results
simplot(out.Vm);

end