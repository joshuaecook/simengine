% =====================================================================
% Flut
% ======================================================================

function m_flut = createFlut(t_imp)

m_flut = Model('flut');

%Geometric Parameters
length = m_flut.input('length', 35);
diameter = m_flut.input('diameter', 3.4);
fiberDiameter = m_flut.input('fiberDiameter', 5.7);
numLamella = m_flut.input('numLamella', 80);
Vext = m_flut.input('Vext', 0);

paspace = 0.004;

%Electrophysiologic Parameters
rhoa = 7e5;
Ra = rhoa*(1/(diameter/fiberDiameter)^2)/10000;
cm = 2*diameter/fiberDiameter;

g_pas = 0.0001*diameter/fiberDiameter;
e_pas = -80;

xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)));
xg = 0.001/(numLamella*2);
xc = 0.1/(numLamella*2);

Iaxonal = m_flut.input('Iaxonal', 0);
Iperiaxonal = m_flut.input('Iperiaxonal', 0);


%State Declarations
Vmem = m_flut.state(-80, 'iter', t_imp);
Vmp = m_flut.state(0, 'iter', t_imp);

% Model Equations

%Geometric and membrane parameters of compartment
SA = (3.1415926*fiberDiameter*length)/1e8;

m_flut.output('Raxonal', (4*(length/10000)*Ra)/(3.1415926*(fiberDiameter/10000)^2)/1e9);
m_flut.output('Rperiaxonal', xraxial*(length/10000)/1e3);

Cm = cm*SA*1e6;
Cp = xc*SA*1e6;
GPas = g_pas*SA*1e9;
Gmyelin = xg*SA*1e9;

%Calculate Passive currents
IPas = GPas*(Vmem - e_pas);
Imyelin = Gmyelin*Vmp;
ICmem = -IPas - Iaxonal;
ICmyelin = ICmem + IPas - Imyelin - Iperiaxonal;

%Membrane potential differential equation
m_flut.diffequ(Vmem, (1/Cm)*(ICmem))
m_flut.diffequ(Vmp, (1/(Cp))*(ICmyelin))
Vm = Vmem + Vmp;

m_flut.output('V', Vm+Vext);
m_flut.output('Vp', Vmp+Vext);


end