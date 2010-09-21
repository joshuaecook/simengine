% Leech heartbeat timing network model (timing network model)
% From Hill et al, 2001, J. Comp. Neuro
% Copyright 2007-2010 Simatra Modeling Technolgies
% Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)

function m = createTimingNetwork(t)

if nargin == 0
  % Create a temporal iterator if one is not passed into the function
  t = Iterator('t', 'continuous', 'solver', 'ode23');
end

% Instantiate the timing network model
m = Model('timingNetwork', t);

% Create the inputs
stimR4 = m.input('stimR4', 0);

% For this model, we need an input for each of the initial voltage
% values
HNL3_Vm0 = m.input('HNL3_Vm0', -45);
HNR3_Vm0 = m.input('HNR3_Vm0', -45);
HNL4_Vm0 = m.input('HNL4_Vm0', -45);
HNR4_Vm0 = m.input('HNR4_Vm0', -45);
HNL1_Vm0 = m.input('HNL1_Vm0', -45);
HNR1_Vm0 = m.input('HNR1_Vm0', -45);

% Instantiate submodels
HNL3 = m.submodel(createHN34);
HNR3 = m.submodel(createHN34);

HNL4 = m.submodel(createHN34);
HNR4 = m.submodel(createHN34);

HNL1 = m.submodel(createHN12);
HNR1 = m.submodel(createHN12);

synapseR3L3 = m.submodel(createSynapse);
synapseL3R3 = m.submodel(createSynapse);

synapseR4L4 = m.submodel(createSynapse);
synapseL4R4 = m.submodel(createSynapse);

synapseR1R3 = m.submodel(createSynapse);
synapseR3R1 = m.submodel(createSynapse);

synapseR1R4 = m.submodel(createSynapse);
synapseR4R1 = m.submodel(createSynapse);

synapseL1L3 = m.submodel(createSynapse);
synapseL3L1 = m.submodel(createSynapse);

synapseL1L4 = m.submodel(createSynapse);
synapseL4L1 = m.submodel(createSynapse);

% Connect submodels together
% The 'with' keyword allows multiple inputs to be specified to the
% same submodel in a single statement.
HNL3.with('gleak', 11, 'Eleak', -62.5, 'Vm0', HNL3_Vm0);
HNR3.with('gleak', 11, 'Eleak', -62.4, 'Vm0', HNR3_Vm0);

HNL4.with('gleak', 11, 'Eleak', -62.5, 'Vm0', HNL4_Vm0);
HNR4.with('gleak', 11, 'Eleak', -62.4, 'Vm0', HNR4_Vm0);

HNL1.Vm0 = HNL1_Vm0;
HNR1.Vm0 = HNR1_Vm0;


synapseR3L3.with('Vpre', HNR3.Vm, 'Vpost', HNL3.Vm, 'gSyn', 60);
synapseL3R3.with('Vpre', HNL3.Vm, 'Vpost', HNR3.Vm, 'gSyn', 60);

synapseR4L4.with('Vpre', HNR4.Vm, 'Vpost', HNL4.Vm, 'gSyn', 60);
synapseL4R4.with('Vpre', HNL4.Vm, 'Vpost', HNR4.Vm, 'gSyn', 60);

synapseR1R3.with('Vpre', HNR1.Vm, 'Vpost', HNR3.Vm, 'gSyn', 16);
synapseR3R1.with('Vpre', HNR3.Vm, 'Vpost', HNR1.Vm, 'gSyn', 6, ...
                 'tRise', 0.01, 'tFall', 0.055);

synapseR1R4.with('Vpre', HNR1.Vm, 'Vpost', HNR4.Vm, 'gSyn', 16);
synapseR4R1.with('Vpre', HNR4.Vm, 'Vpost', HNR1.Vm, 'gSyn', 6, ...
                 'tRise', 0.01, 'tFall', 0.055);

synapseL1L3.with('Vpre', HNL1.Vm, 'Vpost', HNL3.Vm, 'gSyn', 16);
synapseL3L1.with('Vpre', HNL3.Vm, 'Vpost', HNL1.Vm, 'gSyn', 6, ...
                 'tRise', 0.01, 'tFall', 0.055);

synapseL1L4.with('Vpre', HNL1.Vm, 'Vpost', HNL4.Vm, 'gSyn', 16);
synapseL4L1.with('Vpre', HNL4.Vm, 'Vpost', HNL1.Vm, 'gSyn', 6, ...
                 'tRise', 0.01, 'tFall', 0.055);

HNL3.ISyn = synapseR3L3.ISyn + synapseL1L3.ISyn;
HNR3.ISyn = synapseL3R3.ISyn + synapseR1R3.ISyn;

HNL4.ISyn = synapseR4L4.ISyn + synapseL1L4.ISyn;
HNR4.ISyn = synapseL4R4.ISyn + synapseR1R4.ISyn;

HNL1.ISyn = synapseL4L1.ISyn + synapseL3L1.ISyn;
HNR1.ISyn = synapseR4R1.ISyn + synapseR3R1.ISyn;

HNR4.IStim = stimR4;

% Outputs
m.output('VmL3', HNL3.Vm);
m.output('VmR3', HNR3.Vm);
m.output('VmL4', HNL4.Vm);
m.output('VmR4', HNR4.Vm);
m.output('VmL1', HNL1.Vm);
m.output('VmR1', HNR1.Vm);

end
