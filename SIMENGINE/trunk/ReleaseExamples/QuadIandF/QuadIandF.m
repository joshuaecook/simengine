% QuadIandF - Izhikevich Quadratic Integrate and Fire
%
% Izhikevich, E.M., Simple Model of Spiking Neurons, IEEE Trans on Neural Networks, Nov 2003
% Adapted for use with simEngine
% 
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C

function QuadIandF

% create a Model object for the neuron
m = create_QuadIandF;
m.solver = 'forwardeuler';
m.dt = 0.1;

% simulate this model
input.I = 10;
out = simex(m, 200, input);

% plot the results
simplot(out);
title('Single Quadratic Integrate and Fire')
ylabel('Membrane potential');
xlabel('Time');

% now, run the demo which will display 6 different variants
out = simex(create_demo, 200, input);
figure;
subplot(2,3,1);
simplot(out.v_RS);
title('Regular Spiking');

subplot(2,3,2);
simplot(out.v_IB);
title('Intrinsically Bursting');

subplot(2,3,3);
simplot(out.v_CH);
title('Chattering');

subplot(2,3,4);
simplot(out.v_FS);
title('Fast Spiking');

subplot(2,3,5);
simplot(out.v_LTS);
title('Low Threshold Spiking');

subplot(2,3,6);
simplot(out.v_RN);
title('Resonator');

end

% create_demo - instantiate several different variants of the
% Izhikevich neuron
function m = create_demo

% first, create a model object for the neuron
neuron = create_QuadIandF;

% now, we're going to create a new top-level model for the demos
m = Model('demo');

% there will be one common input
I = m.input('I');

regular_spiking = m.submodel(neuron);
regular_spiking.I = I;
regular_spiking.d = 8;

intrinsically_bursting = m.submodel(neuron);
intrinsically_bursting.I = I;
intrinsically_bursting.c = -55;
intrinsically_bursting.d = 4;

chattering = m.submodel(neuron);
chattering.I = I;
chattering.c = -50;
chattering.d = 2;

fast_spiking = m.submodel(neuron);
fast_spiking.I = I;
fast_spiking.a = 0.1;

low_threshold_spiking = m.submodel(neuron);
low_threshold_spiking.I = I;
low_threshold_spiking.b = 0.25;

resonator = m.submodel(neuron);
resonator.I = I;
resonator.a = 0.1;
resonator.b = 0.26;

% define the outputs of the system
m.output('v_RS', regular_spiking.v);
m.output('v_IB', intrinsically_bursting.v);
m.output('v_CH', chattering.v);
m.output('v_FS', fast_spiking.v);
m.output('v_LTS', low_threshold_spiking.v);
m.output('v_RN', resonator.v);

end