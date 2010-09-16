% createMysa - mysa model from the MRG double axon myelinated cable
% model
%
% McIntyre, Richardson, & Grill (J Neurophysiol 87:995-1006, 2001)
% Publication: Modeling the Excitability of Mammalian Nerve Fibers:
%              Influence of Afterpotentials on the Recovery Cycle
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%
function m_mysa = createMysa(t_imp)

m_mysa = Model('mysa');

%Geometric Parameters
length = m_mysa.input('length', 3);
diameter = m_mysa.input('diameter', 1.9);
fiberDiameter = m_mysa.input('fiberDiameter', 5.7);
numLamella = m_mysa.input('numLamella', 80);
Vext = m_mysa.input('Vext', 0);

paspace = 0.002;

%Electrophysiologic Parameters
rhoa = 7e5;
Ra = rhoa*(1/(diameter/fiberDiameter)^2)/10000;
cm = 2*diameter/fiberDiameter;

g_pas = 0.001*diameter/fiberDiameter;
e_pas = -80;

xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)));
xg = 0.001/(numLamella*2);
xc = 0.1/(numLamella*2);

Iaxonal = m_mysa.input('Iaxonal', 0);
Iperiaxonal = m_mysa.input('Iperiaxonal', 0);
Isegmental_axonal = m_mysa.input('Isegmental_axonal', 0);
Isegmental_periaxonal = m_mysa.input('Isegmental_periaxonal', 0);


%State Declarations
Vmem = m_mysa.state(-80, 'iter', t_imp);
Vmp = m_mysa.state(0, 'iter', t_imp);

% Model Equations

%Geometric and membrane parameters of compartment
SA = (3.1415926*fiberDiameter*length)/1e8;

m_mysa.output('Raxonal', (4*(length/10000)*Ra)/(pi*(fiberDiameter/10000)^2)/1e9);
m_mysa.output('Rperiaxonal', xraxial*(length/10000)/1e3);

Cm = cm*SA*1e6;
Cp = xc*SA*1e6;
GPas = g_pas*SA*1e9;
Gmyelin = xg*SA*1e9;

%Calculate Passive currents
IPas = GPas*(Vmem - e_pas);
Imyelin = Gmyelin*Vmp;
ICmem = -IPas - Iaxonal - Isegmental_axonal;
ICmyelin = ICmem + IPas - Imyelin - Iperiaxonal - Isegmental_periaxonal;

%Membrane potential differential equation
m_mysa.diffequ(Vmem, (1/Cm)*(ICmem))
m_mysa.diffequ(Vmp, (1/(Cp))*(ICmyelin))
Vm = Vmem + Vmp;

m_mysa.output('V', Vm+Vext);
m_mysa.output('Vp', Vmp+Vext);


end