function m = createMRGAxon(numSegments)

% Store the PWD - would like a workaround for this
[file_path] = fileparts(which('createMRGAxon'));


% First define the global iterators
dt = 0.001;
t_imp = Iterator('t_imp', 'solver', 'linearbackwardeuler', 'dt', dt);
t_exp = Iterator('t_exp', 'solver', 'forwardeuler', 'dt', dt);

% Create the submodels
m_node = createNode(t_imp, t_exp);
m_flut = createFlut(t_imp);
m_mysa = createMysa(t_imp);
m_stin = createStin(t_imp);
m_segment = createSegment(m_node, m_mysa, m_flut, m_stin);

% =====================================================================
% MRG Axon
% ======================================================================
    
% Create the shell of a new model call MRGAxon
m = Model('MRGAxon');

% Define all the inputs
Istim = m.input('Istim', 20);
Vext = cell(1,numSegments+1);
for i=1:numSegments
    Vext{i} = m.input(['Vext' num2str(i)],0);
end
% add the node external input
Vext{end} = m.input(['Vext' num2str(numSegments+1)],0);

% Now instantiate the segment submodels
segment_dsl = fullfile(file_path, 'segment.dsl');
segments = cell(1,numSegments);
for i=1:numSegments
    segments{i} = m.submodel(m_segment);
    % add the inputs to the submodels
    segments{i}.VextLeft = Vext{i};
    segments{i}.VextRight = Vext{i+1};    
end

% Add a final node
node_dsl = fullfile(file_path, 'node.dsl');
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
    spikes{i} = m.equ((Vm(t_imp-1) > Vm(t_imp-2)) & ...
                      (Vm(t_imp-1) > Vm(t_imp)) & ...
                      (Vm(t_imp-1) > -25));
end

% Add outputs
outputList = cell(1, numSegments);
for i=1:numSegments
    outputList{i} = segments{i}.VmAxonal_L;
end
m.output('Vm', outputList);

% Create helper function to reduce a cell array of boolean expressions to
% one expression - replace with the ANY command when vectorized.
    function er = reduction_or(cell_list)
        if ~isempty(cell_list)
            er = cell_list{1};
            for i=2:length(cell_list)
                er = er | cell_list{i};
            end
        else
            er = Exp(1);
        end
    end
m.output('spikes', spikes, 'when', reduction_or(spikes));

end



