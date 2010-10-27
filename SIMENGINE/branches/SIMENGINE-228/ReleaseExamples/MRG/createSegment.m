% createSegment - segment model from the MRG double axon myelinated cable
% model
%
% McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
% Publication: Modeling the Excitability of Mammalian Nerve Fibers:
%              Influence of Afterpotentials on the Recovery Cycle
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%
function m = createSegment(t_imp, t_exp)

if nargin == 0
    % First define the global iterators
    dt = 0.001;
    t_imp = Iterator('t_imp', 'solver', 'linearbackwardeuler', 'dt', dt);
    t_exp = Iterator('t_exp', 'solver', 'forwardeuler', 'dt', dt);
end

% Create the submodels
node = createNode(t_imp, t_exp);
flut = createFlut(t_imp);
mysa = createMysa(t_imp);
stin = createStin(t_imp);


m = Model('segment');

Istim = m.input('Istim', 0);
Isegmental_L = m.input('Isegmental_L', 0);
Isegmental_R = m.input('Isegmental_R', 0);
Iperiaxonal_R = m.input('Iperiaxonal_R', 0);
VextLeft = m.input('VextLeft', 0);
VextRight = m.input('VextRight', 0);

nodeA = m.submodel(node);
nodeA.with('diameter', 1.9, 'length', 1, 'Istim', Istim, 'Isegmental', Isegmental_L, 'Vext', VextLeft);
mysaA = m.submodel(mysa);
mysaA.with('diameter', 1.9, 'length', 3, 'Vext', VextLeft + (VextRight - VextLeft)*3/497);
flutA = m.submodel(flut);
flutA.with('diameter', 3.4, 'length', 35, 'Vext', VextLeft + (VextRight - VextLeft)*38/497);
stins = m.submodel(stin, 1, 6);
stins.with('diameter', 3.4, 'length', 70, 'Vext', VextLeft + (VextRight - VextLeft)*(108+i*70)/497);
flutB = m.submodel(flut);
flutA.with('diameter', 3.4, 'length', 35, 'Vext', VextLeft + (VextRight - VextLeft)*493/497);
mysaB = m.submodel(mysa);
mysaB.with('diameter', 1.9, 'length', 3, 'Isegmental_axonal', Isegmental_R, 'Isegmental_periaxonal', Iperiaxonal_R, 'Vext', VextLeft + (VextRight - VextLeft)*496/497);

function join1(c1, c2)
  %function to join node to another section
  c1.Iaxonal = (c1.V - c2.V)/((c1.Raxonal + c2.Raxonal)/2);
  c1.Iperiaxonal = (c1.Vp - c2.Vp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2);
end

function join2(c1, c2, c3)
  %function to join a section to 2 other sections
  c1.Iaxonal = (c1.V - c2.V)/((c1.Raxonal + c2.Raxonal)/2) + (c1.V - c3.V)/((c1.Raxonal + c3.Raxonal)/2);
  c1.Iperiaxonal = (c1.Vp - c2.Vp)/((c1.Rperiaxonal + c2.Rperiaxonal)/2)+(c1.Vp-c3.Vp)/((c1.Rperiaxonal + c3.Rperiaxonal)/2);
end

join1(nodeA, mysaA);
join2(mysaA, nodeA, flutA);
join2(flutA, mysaA, stins{1});
join2(stins{1}, flutA, stins{2})
for i=2:(numStins-1)
    join2(stins{i}, stins{i-1}, stins{i+1});
end
join2(stins{numStins}, stins{numStins-1}, flutB);
join2(flutB, stins{numStins}, mysaB);
join1(mysaB, flutB);

m.output('VmAxonal_L', nodeA.V);
m.output('VmAxonal_R', mysaB.V);
m.output('VmPeriaxonal_R', mysaB.Vp);
m.output('Raxonal_L', nodeA.Raxonal);
m.output('Raxonal_R', mysaB.Raxonal);
m.output('Rperiaxonal_L', nodeA.Rperiaxonal);
m.output('Rperiaxonal_R', mysaB.Rperiaxonal);

end
