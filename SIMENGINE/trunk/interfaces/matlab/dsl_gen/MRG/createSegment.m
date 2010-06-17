function m = createSegment(node, mysa, flut, stin)

m = Model('segment');

Istim = m.input('Istim', 0);
Isegmental_L = m.input('Isegmental_L', 0);
Isegmental_R = m.input('Isegmental_R', 0);
Iperiaxonal_R = m.input('Iperiaxonal_R', 0);
VextLeft = m.input('VextLeft', 0);
VextRight = m.input('VextRight', 0);

nodeA = m.submodel(node);
nodeA.diameter = 1.9; nodeA.length = 1; nodeA.Isegmental = Isegmental_L; nodeA.Vext = VextLeft;
mysaA = m.submodel(mysa);
mysaA.diameter = 1.9; mysaA.length=3; mysaA.Vext = VextLeft + (VextRight - VextLeft)*3/497;
flutA = m.submodel(flut);
flutA.diameter = 3.4; flutA.length=35; flutA.Vext = VextLeft + (VextRight - VextLeft)*38/497;
numStins = 6;
stins = cell(1,numStins);
for i=1:numStins
    stins{i} = m.submodel(stin);
    stins{i}.diameter = 3.4; stins{i}.length=35; stins{i}.Vext = VextLeft + (VextRight - VextLeft)*(108+i*70)/497;
end
flutB = m.submodel(flut);
flutB.diameter = 3.4; flutB.length=35; flutB.Vext = VextLeft + (VextRight - VextLeft)*493/497;
mysaB = m.submodel(mysa);
mysaB.diameter = 1.9; mysaB.length=3; mysaB.Vext = VextLeft + (VextRight - VextLeft)*496/497;
mysaB.Isegmental_axonal = Isegmental_R; mysaB.Isegmental_periaxonal = Iperiaxonal_R;

nodeA.Istim = Istim;

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
