%% Booth, Rinzel, & Kiehn Motoneuron Model

%%
%

function brk()

%%
% <html><hr></html>

%% Introduction
% The Booth, Rinzel, and Kiehn (BRK) motoneuron model is a two
% compartment vertebrate motoneuron demonstrating
% $Ca^{2+}$-dependent spiking, plateau potentials, and
% bistability when under pharmacological treatment.  The ionic
% currents are distributed in a somatic and dendritic compartment.
% Ionic currents follow Hodgkin-Huxley kinetics and include a fast
% inactivating $Na^{+}$ channel, $K^+$ delayed
% rectifier, N-type $Ca^{2+}$ channels, a
% $Ca^{2+}$-dependent $K^+$ channel, and a persistent
% L-type $K^{+}$ channel in the dendrite.  There are a total of
% 11 state variables, which include two voltage states, two calcium
% concentration states, and seven gating variables.

%%
% <html><hr></html>

%% Model Description
% The brk demo is split into two model files.  The first one,
% *create_brk_inputs.m*, generates the neural spiking behavior.  The second
% model, *create_spike_detector.m*, is cascaded with the first and computes spike
% times from a continuous voltage trace.

%%
% <html><hr></html>

%% Motoneuron - *create_brk_inputs.m*
% The model, *brk_inputs*, is defined with one input, *Iext*, and two
% outputs, *Iapp* and *V*.  The input to the model is a time-varying
% sampled input, with input samples generated every one millisecond.
% The output, *Iapp*, is a sampled variant of *Iext* where each time
% point is one that was used by the simulation.  The voltage output,
% *V*, is a combination of two voltages, the somatic voltage, *Vs*, and
% the dendritic voltage, *Vd*.  For high performance, a variable time
% step solver, *ode23*, is employed which provides 2nd-order accuracy
% with adaptive step control based on a 3rd-order term.
%
% The somatic compartment contains a fast inactivating $Na^+$
% channel, a delayed rectifier, an N-type $Ca^{2+}$ channel, the
% external input, a leakage current, and a coupling current.  The
% dendritic compartment contains no fast $Na^+$ or $K^+$
% currents, but instead contains an N-type $Ca^{2+}$ current, a
% $Ca^{2+}$-dependent $K^+$ channel, and a leakage and
% coupling current.
%
% The voltage equations are as follows:
%
% $V_{\rm{s}}' = \frac{1}{C} \left( Iext - INa_{\rm{s}} - IK_{\rm{s}} -
% ICa_{\rm{s}} - Ileak_{\rm{s}}- Icoupling_{\rm{s}} \right)$
%
% $V_{\rm{d}}' = \frac{1}{C} \left( - IK_{\rm{d}} - ICa_{\rm{d}} -
% Ileak_{\rm{d}} - Icoupling_{\rm{d}} \right)$
%
% <latex>
% \begin{eqnarray*}
% V_{\rm{s}}' &=& \frac{1}{C} \left( Iext - INa_{\rm{s}} - IK_{\rm{s}} -
% ICa_{\rm{s}} - Ileak_{\rm{s}}- Icoupling_{\rm{s}} \right) \\
% V_{\rm{d}}' &=& \frac{1}{C} \left( - IK_{\rm{d}} - ICa_{\rm{d}} -
% Ileak_{\rm{d}} - Icoupling_{\rm{d}} \right)
% \end{eqnarray*}
% </latex>
%
% The voltage output, *V*, is the combination of both the somatic and
% dendritic voltages.  The output current, *Iapp*, is a resampled
% version of *Iext*, where the output is given for every value of *t*.
% The following is a complete listing of the *create_brk_inputs.m* model.

%
% Booth, Rinzel, & Kiehn Motoneuron Model (J Neurophysiol 78:3371-3384, 1997)
% Publication: Compartmental Model of Vertebrate Motoneurons for Ca2+-Dependent Spiking
%              and Plateau Potentials Under Pharmacological Treatment
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%

function m = create_brk_inputs()

% Define the solver for the model
t = Iterator('continuous', 'solver', 'ode23', 'dt', 0.01);
i = Iterator('discrete', 'sample_period', 1);

% Create the model with the defined iterator
m = Model('brk', t);

% Conductances (mS/cm^2)
GNa = 120;
GK_dr = 100;
GCa_NS = 14;
GCa_ND = .03;
GK_CaS = 3.136; % canonical: 5
GK_CaD = 0.69;  % canonical: 1.1
GCa_L = 0.33;
gleak = 0.51;

% Static parameters
C = 1;
gc = 0.1;      % coupling conductance (mS/cm^2)
p = 0.1;
Kd = 0.2;      % uM
f = 0.01;      % percent free to bound Ca
alpha = 0.009; % mol/C/um
kca = 2;       % Ca removal rate

% Half Activation voltages in mV, Slopes in MV, Time Constants in milliseconds
Vhm = -35;
Sm = -7.8;
Vhh = -55;
Sh = 7;
Vhn = -28;
Sn = -15;
VhmN = -30;
SmN = -5;
VhhN = -45;
ShN = 5;
VhmL = -40;
SmL = -7;
TaumN = 4;
TauhN = 40;
TaumL = 40;

% Reversal potentials in mV
ENa = 55;
EK = -80;
ECa = 80;
Eleak = -60;
  
% State Variable Declaration
Vs = m.state(-60);
Vd = m.state(-60);
h = m.state(0.9);
n = m.state(0);
mnS = m.state(0);
hnS = m.state(0.9);
mnD = m.state(0);
hnD = m.state(0.9);
ml = m.state(0);
CaS = m.state(0);
CaD = m.state(0);

% External Current
Iext = m.input('Iext', 0, 'iter', i, 'hold');
  
% Steady state values
Tauh = 30/(exp((Vs+50)/15)+exp(-(Vs+50)/16));
Taun = 7/(exp((Vs+40)/40)+exp(-(Vs+40)/50));
minf = 1/(1+exp((Vs-Vhm)/Sm));
hinf = 1/(1+exp((Vs-Vhh)/Sh));
ninf = 1/(1+exp((Vs-Vhn)/Sn));
mnSinf = 1/(1+exp((Vs-VhmN)/SmN));
hnSinf = 1/(1+exp((Vs-VhhN)/ShN));
mnDinf = 1/(1+exp((Vd-VhmN)/SmN));
hnDinf = 1/(1+exp((Vd-VhhN)/ShN));
mlinf = 1/(1+exp((Vd-VhmL)/SmL));
    
% Current values
INaS = GNa*minf^3*h*(Vs-ENa);
IKS = (GK_dr*n^4 + GK_CaS*CaS/(CaS+Kd))*(Vs-EK);
ICaS = GCa_NS*mnS^2*hnS* (Vs-ECa);
IleakS = gleak*(Vs-Eleak);
IcouplingS = gc/p*(Vs-Vd);
IKD = GK_CaD*CaD/(CaD+Kd)*(Vd-EK);
ICaD = (GCa_ND*mnD^2*hnD+GCa_L*ml)*(Vd-ECa);
IleakD = gleak*(Vd-Eleak);
IcouplingD = gc/(1-p)*(Vd-Vs);

% Differential equations
m.diffequ(h, (hinf-h)/Tauh);
m.diffequ(n, (ninf-n)/Taun);
m.diffequ(mnS, (mnSinf-mnS)/TaumN);
m.diffequ(hnS, (hnSinf-hnS)/TauhN);
m.diffequ(mnD, (mnDinf-mnD)/TaumN);
m.diffequ(hnD, (hnDinf-hnD)/TauhN);
m.diffequ(ml, (mlinf-ml)/TaumL);
m.diffequ(CaS, f*(-alpha*ICaS-kca*CaS));
m.diffequ(CaD, f*(-alpha*ICaD-kca*CaD));
m.diffequ(Vs, 1/C*(Iext-INaS-IKS-ICaS-IleakS-IcouplingS));
m.diffequ(Vd, 1/C*(-IKD-ICaD-IleakD-IcouplingD));

% Output definition
m.output('V', Vs, Vd);
m.output('Iapp', Iext, 'iter', t);

end


%%
% <html><hr></html>

%% Spike Detector - *create_spike_detector.m*
% The *spike_detector* model reads in voltage trace data and creates output
% events whenever a spike is registered.  The following is the complete
% code listing for the *create_spike_detector* model.

%
% spike_detector - Create spike events from input voltage trace data
%
% Copyright 2010 Simatra Modeling Technologies

function m = create_spike_detector()

m = Model('spike_detector');

% set up a discrete iterator with steps of 0.01 milliseconds
i = Iterator('discrete', 'sample_period', 1);

% Define a sampled input for the voltage trace
Vm = m.input('Vm', 0, 'iter', i, 'halt');

% Detect a spike
spike_occurred = (Vm(i) > 20) & (Vm(i) < Vm(i-1)) & (Vm(i-1) >= Vm(i-2));

m.output('events', i, 'when', spike_occurred);

end

%%
% An input, *Vm*, is defined as an input with iterator *i*.  This
% iterator simply means that *Vm* is sampled discretely according to the
% definition of iterator *i*, a discrete iterator with a sample period
% of 0.01 milliseconds.  The spike_detector model expects that *Vm* will be a
% continuous stream of data with a sampling rate of 100 kHz.  The flag,
% *halt*, tells simEngine to finish the simulation of
% *spike_detector* when there is no more data available for processing.
%
% Spikes are determined according to three criteria.  First, *Vm* has to
% exceed 20 mV.  Sub-threshold oscillations will not count as a recorded
% spike.  Second, the current value of *Vm* has to be less than the
% value one iteration earlier, depicted as Vm(i-1), meaning that it is
% decreasing in value.  Finally, the previous value of *Vm* has to be
% greater than or equal to the value two steps before.  The second and
% third conditions, when satisfied, indicate that a local maxima has
% been reached.
%
% When this condition is true, the *spike_occurred* variable is set.
% This is used in an output condition, such that the output, *events*,
% will be generated only at the times that the *spike_occurred* variable
% is true.

%%
% <html><hr></html>

%% MATLAB processing
% The following example demonstrates the invocation of simEngine twice, once for the compilation and simulation of
% *create_brk_inputs.m* and then for the compilation and simulation of *create_spike_detector.m*.  A up and down current ramp is
% generated in MATLAB and used as the input stimulus for *brk_inputs*.  The external current in *brk_inputs* was defined
% as a time varying input with a discrete iterator and sample period of 1 ms.
%
% In MATLAB, we generate an up and down ramp by defining just the inflection points and using interpolation to fill in the remaining points.


% Set parameters for the simulation protocol
max_Iapp = 25;     % nA/cm^2
end_time = 11000;  % ms

% Produce the input train, which is effectively a up and down
% current ramp
points = [0 0; 500 0; end_time/2 max_Iapp; end_time-500 0; end_time 0];
n = 0:end_time;
inp.Iext = {interp1(points(:,1),points(:,2), n)};

% Execute the simulation of the BRK motoneuron model
o = simex(create_brk_inputs, end_time, inp);

% Plot the voltage traces from the neural model
subplot(2,1,1);
simplot(o.V);
axis([0 end_time -80 60]);
title('Voltage Trace (with simulated 5-HT)');
ylabel('Voltage (mV)');
legend('V_s', 'V_d');

% Plot the input current as read by the model
subplot(2,1,2);
simplot(o.Iapp, 'r');
axis([0 end_time -5 30]);
title('Input Current Waveform');
xlabel('Time (ms)');
ylabel('Current (nA/cm^2)');

%%

% Now, resampling the output somatic voltage and reformat 
% as an input to the spike detector model
% The spike detector model uses a normalized timestep of zero, so we need
% to rescale based on the timestep that will adequately show the spikes -
% 0.01 ms
dt = 0.01;
n2 = 0:dt:end_time;
inp2.Vm = {interp1(o.V(:,1),o.V(:,2),n2,'linear','extrap')};
o2 = simex(create_spike_detector, end_time/dt, inp2);

% Compute interspike intervals and spiking frequency from the spike data
spike_times = (o2.events(:,1)*dt)'; % rescale based on dt used when generating the input stream Vm
interspike_intervals = [inf diff(spike_times)]; % Units in ms
spike_frequency = 1000./interspike_intervals; % Units in Hz

% Sub-sample the input waveform to match the spike times
sampled_input = interp1(o.Iapp(:,1), o.Iapp(:,2), spike_times);

% Plot the spike frequencies as a function of input current.  Using
% different colors and different markers to distinguish between the
% rising current and the falling current
subplot(1,1,1);
rising = find(spike_times <= end_time/2);
falling = find(spike_times > end_time/2);
plot(sampled_input(rising), spike_frequency(rising), 'o', sampled_input(falling), spike_frequency(falling), '.');
title('F-I Curve')
xlabel('Current (nA/cm^2)');
ylabel('Spike Frequency (Hz)');
legend('Rising', 'Falling', 'Location', 'NorthWest')

%%
% <html><hr></html>

%% Adapting this example
% This example of the BRK model and a spike detector can be applied to a general class of problems combining a dynamical
% system modeled by differential equations and a post process analysis routine.  For other simulations, different neural
% models or analysis files can easily be swapped out.
%
% If you are using the Standard or Professional edition of simEngine, both the model and the analysis can be included as
% part of one model.  This is because the Standard and Professional editions support multiple iterators per model, where
% *t* is the continuous iterator used in *brk_inputs* and *i* is the discrete iterator used in *spike_detector*.

%%
% <html><hr></html>

%% References
% Booth V., Rinzel, J., Kiehn, O., Compartmental Model of Vertebrate Motoneurons for $Ca^{2+}$-Dependent Spiking and Plateau Potentials Under Pharmacological Treatment, *J Neurophys*, vol. 78, no. 6, pp. 3371-85, 1997

end