% =====================================================================
% Node
% ======================================================================

function m_node = createNode(t_imp, t_exp)

m_node = Model('node');

length = m_node.input('length', 1);
diameter = m_node.input('diameter', 1.9);
Isegmental = m_node.input('Isegmental', 0);
Vext = m_node.input('Vext', 0);

paspace = 0.002;

rhoa = 7e5;
Ra = rhoa/10000;
cm = 2;
g_pas = 0.007;
e_pas = -90;

xraxial = (rhoa*.01)/(pi*((((diameter/2)+paspace)^2)-((diameter/2)^2)));
gNaF = 3.0;
gKs = 0.08;
gNaP = 0.01;

ENa = 50;
EK = -90;

Istim = m_node.input('Istim', 0);
Iaxonal = m_node.input('Iaxonal', 0);
Iperiaxonal = m_node.input('Iperiaxonal', 0);

Vm = m_node.state(-80, 'iter', t_imp);
m  = m_node.state(0.0732093,'iter', t_exp);
h  = m_node.state(0.620695, 'iter', t_exp);
p  = m_node.state(0.202604, 'iter', t_exp);
s  = m_node.state(0.0430299, 'iter', t_exp);

Vmp = 0;

SA = (3.1415926*diameter*length)/1e8;

m_node.output('Raxonal',(4*(length/10000)*Ra)/(3.1415926*(diameter/10000)^2)/1e9);
m_node.output('Rperiaxonal',xraxial*(length/10000)/1e3);

Cm = cm*SA*1e6;
GPas = g_pas*SA*1e9;

GNaF = gNaF*SA*1e9;
GKa = gKs*SA*1e9;
GNaP = gNaP*SA*1e9;

%Equations for fast sodium current (pA)
alpha_m = (6.57 * (Vm + 21.4))/(1 - exp(-(Vm + 21.4)/10.3));
beta_m = (0.304 * (-(Vm +25.7)))/(1 - exp((Vm + 25.7)/9.16));
alpha_h = (0.34 * (-(Vm + 114)))/(1 - exp((Vm + 114)/11));
beta_h = 12.6/(1 + exp(-(Vm + 31.8)/13.4));
m_node.diffequ(m, alpha_m*(1 - m) - beta_m*m);
m_node.diffequ(h, alpha_h*(1 - h) - beta_h*h);
INaF = GNaF*m*m*m*h*(Vm - ENa);

%Equations for persistant sodium current (pA)
alpha_p = (0.0353 * (Vm + 27))/(1 - exp(-1*(Vm + 27)/10.2));
beta_p = (0.000883 * (-(Vm + 34)))/(1 - exp((Vm + 34)/10));
m_node.diffequ(p, alpha_p*(1-p) - beta_p*p);
INaP = GNaP*p*p*p*(Vm - ENa);

%Equations for slow potassium current (pA)
alpha_s = 0.3/(1 + exp(-(Vm + 53)/5));
beta_s = 0.03/(1 + exp(-(Vm + 90)));
m_node.diffequ(s, alpha_s*(1 - s) - beta_s*s);
IK = GKa*s*(Vm - EK);

%Leak current (pA)
IPas = GPas*(Vm - e_pas);

%summation of membrane currents
ICmem = Istim - IPas - Iaxonal - Isegmental - INaF - INaP -IK;

%Membrane potential differential equation
m_node.diffequ(Vm, (1/Cm)*(ICmem));

m_node.output('V', Vm+Vext);
m_node.output('Vp', Vmp+Vext);

end