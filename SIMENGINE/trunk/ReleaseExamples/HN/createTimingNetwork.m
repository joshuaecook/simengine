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

HNL3 = m.submodel(createHN34);
HNL3.gleak = 11;
HNL3.Eleak = -62.5;

HNR3 = m.submodel(createHN34);
HNL3.gleak = 11;
HNL3.Eleak = -62.4;

HNL4 = m.submodel(createHN34);
HNL3.gleak = 11;
HNL3.Eleak = -62.5;

HNR4 = m.submodel(createHN34);
HNL3.gleak = 11;
HNL3.Eleak = -62.4;


HNR1 = m.submodel(createHN12);
HNL1 = m.submodel(createHN12);

synapseR3L3 = m.submodel(createSynapse);
synapseR3L3.Vpre = HNR3.Vm;
synapseR3L3.Vpost = HNL3.Vm;
synapseR3L3.gSyn = 60;
synapseL3R3 = m.submodel(createSynapse);
synapseL3R3.Vpre = HNL3.Vm;
synapseL3R3.Vpost = HNR3.Vm;
synapseL3R3.gSyn = 60;

synapseR4L4 = m.submodel(createSynapse);
synapseR4L4.Vpre = HNR4.Vm;
synapseR4L4.Vpost = HNL4.Vm;
synapseR4L4.gSyn = 60;
synapseL4R4 = m.submodel(createSynapse);
synapseL4R4.Vpre = HNL4.Vm;
synapseL4R4.Vpost = HNR4.Vm;
synapseL4R4.gSyn = 60;

synapseR1R3 = m.submodel(createSynapse);
synapseR1R3.Vpre = HNR1.Vm;
synapseR1R3.Vpost = HNR3.Vm;
synapseR1R3.gSyn = 16;
synapseR3R1 = m.submodel(createSynapse);
synapseR3R1.Vpre = HNR3.Vm;
synapseR3R1.Vpost = HNR1.Vm;
synapseR3R1.gSyn = 6;
synapseR3R1.tRise = 0.01;
synapseR3R1.tFall = 0.055;

synapseR1R4 = m.submodel(createSynapse);
synapseR1R4.Vpre = HNR1.Vm;
synapseR1R4.Vpost = HNR4.Vm;
synapseR1R4.gSyn = 16;
synapseR4R1 = m.submodel(createSynapse);
synapseR4R1.Vpre = HNR4.Vm;
synapseR4R1.Vpost = HNR1.Vm;
synapseR4R1.gSyn = 6;
synapseR4R1.tRise = 0.01;
synapseR4R1.tFall = 0.055;

synapseL1L3 = m.submodel(createSynapse);
synapseL1L3.Vpre = HNL1.Vm;
synapseL1L3.Vpost = HNL3.Vm;
synapseL1L3.gSyn = 16;
synapseL3L1 = m.submodel(createSynapse);
synapseL3L1.Vpre = HNL3.Vm;
synapseL3L1.Vpost = HNL1.Vm;
synapseL3L1.gSyn = 6;
synapseL3L1.tRise = 0.01;
synapseL3L1.tFall = 0.055;

synapseL1L4 = m.submodel(createSynapse);
synapseL1L4.Vpre = HNL1.Vm;
synapseL1L4.Vpost = HNL4.Vm;
synapseL1L4.gSyn = 16;
synapseL4L1 = m.submodel(createSynapse);
synapseL4L1.Vpre = HNL4.Vm;
synapseL4L1.Vpost = HNL1.Vm;
synapseL4L1.gSyn = 6;
synapseL4L1.tRise = 0.01;
synapseL4L1.tFall = 0.055;


HNL3.ISyn = synapseR3L3.ISyn + synapseL1L3.ISyn;
HNR3.ISyn = synapseL3R3.ISyn + synapseR1R3.ISyn;

HNL4.ISyn = synapseR4L4.ISyn + synapseL1L4.ISyn;
HNR4.ISyn = synapseL4R4.ISyn + synapseR1R4.ISyn;

HNL1.ISyn = synapseL4L1.ISyn + synapseL3L1.ISyn;
HNR1.ISyn = synapseR4R1.ISyn + synapseR3R1.ISyn;

HNR4.IStim = stimR4;

m.output('VmL3', HNL3.Vm);
m.output('VmR3', HNR3.Vm);
m.output('VmL4', HNL4.Vm);
m.output('VmR4', HNR4.Vm);
m.output('VmL1', HNL1.Vm);
m.output('VmR1', HNR1.Vm);

end
