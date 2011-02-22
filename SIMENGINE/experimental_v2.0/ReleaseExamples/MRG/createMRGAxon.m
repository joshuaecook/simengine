% createMRGAxon - MRG double axon myelinated cable model
%
% McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
% Publication: Modeling the Excitability of Mammalian Nerve Fibers:
%              Influence of Afterpotentials on the Recovery Cycle
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%
function m = createMRGAxon(numSegments)

% if you don't specify a number of segments, just set it to a default value
if nargin == 0
    numSegments = 2;
end

% Create the submodels
m_node = createNode();
m_flut = createFlut();
m_mysa = createMysa();
m_stin = createStin();
m_segment = createSegment(m_node, m_mysa, m_flut, m_stin);

% =====================================================================
% MRG Axon
% ======================================================================
    
% Create the shell of a new model call MRGAxon
m = Model('MRGAxon');

% Add the auto iterator - this will automatically determine the appropriate
% iterators for the model
m.solver = 'auto';
m.dt = 0.001;

% Define all the inputs
Istim = m.input('Istim', 20);
Vext = cell(1,numSegments+1);
for i=1:numSegments
    Vext{i} = m.input(['Vext' num2str(i)],0);
end
% add the node external input
Vext{end} = m.input(['Vext' num2str(numSegments+1)],0);

% Now instantiate the segment submodels
segments = cell(1,numSegments);
for i=1:numSegments
    segments{i} = m.submodel(m_segment);
    % add the inputs to the submodels
    segments{i}.VextLeft = Vext{i};
    segments{i}.VextRight = Vext{i+1};    
end

% Add a final node
node = m.submodel(m_node);
node.Vext = Vext{end};

% Define the connect functions
    function join(s1, s2)
        s1.Isegmental_R = (s1.VmAxonal_R - s2.VmAxonal_L)/((s1.Raxonal_R + s2.Raxonal_L)/2);
        s1.Iperiaxonal_R = (s1.VmPeriaxonal_R)/((s1.Rperiaxonal_R + s2.Rperiaxonal_L)/2);
        s2.Isegmental_L = (s2.VmAxonal_L - s1.VmAxonal_R)/((s1.Raxonal_R + s2.Raxonal_L)/2); 
    end
    function joinNode(segment, node)
        segment.Isegmental_R = (segment.VmAxonal_R - node.V)/((segment.Raxonal_R + node.Raxonal)/2);
        segment.Iperiaxonal_R = (segment.VmPeriaxonal_R)/((segment.Rperiaxonal_R + node.Rperiaxonal)/2);
        node.Isegmental = -(segment.VmAxonal_R - node.V)/((segment.Raxonal_R + node.Raxonal)/2);
    end

% Connect up all the segments but the last
for i=1:(numSegments-1)
    join(segments{i}, segments{i+1});
end
% Connect up the last segment to the node and the last node
if numSegments > 0
    joinNode(segments{end}, node);
end

% Assign Istim to the middle segment
if numSegments > 0
    middle_segment = ceil(numSegments/2);
    segments{middle_segment}.Istim = Istim;
else
    node.Istim = Istim;
end

% Create spike outputs
spikes = cell(1,numSegments);
for i=1:numSegments
    Vm = m.equ(segments{i}.VmAxonal_L);
    t = m.timeIterator;
    spikes{i} = m.equ((Vm(t-1) > Vm(t-2)) & ...
                      (Vm(t-1) > Vm(t)) & ...
                      (Vm(t-1) > -25));
end

% Add outputs
if numSegments > 0
    outputList = cell(1, numSegments);
    for i=1:numSegments
        outputList{i} = segments{i}.VmAxonal_L;
    end
else
    outputList = node.V;
end
m.output('Vm', outputList);

m.output('spikes', spikes, 'when', any(spikes{:}));

end



