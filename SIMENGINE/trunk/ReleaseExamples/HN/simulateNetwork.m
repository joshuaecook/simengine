% build the Timing Network model using DIESEL for Matlab - return a
% model object
net = createTimingNetwork;

% randomize the initial state vectors
parameters.HNL3_Vm0 = rand*10-55;
parameters.HNR3_Vm0 = rand*10-55;
parameters.HNL4_Vm0 = rand*10-55;
parameters.HNR4_Vm0 = rand*10-55;
parameters.HNL1_Vm0 = rand*10-55;
parameters.HNR1_Vm0 = rand*10-55;

%set up a good initial state vector
[o, initState, ~] = simex(net, 100, parameters);

figure, simplot(o);

%run the model with default parameters
data = simex(net, 200, '-resume', initState);

t = data.VmL1(:,1)-100;

figure,
subplot(6,1,1)
plot(t, data.VmL1(:,2), 'r')
axis([0 100 -60 5]);

subplot(6,1,2)
plot(t, data.VmL3(:,2), 'k')
axis([0 100 -60 5]);

subplot(6,1,3)
plot(t, data.VmR3(:,2), 'k')
axis([0 100 -60 5]);

subplot(6,1,4)
plot(t, data.VmL4(:,2), 'b')
axis([0 100 -60 5]);

subplot(6,1,5)
plot(t, data.VmR4(:,2), 'b')
axis([0 100 -60 5]);

subplot(6,1,6)
plot(t, data.VmR1(:,2), 'r')
axis([0 100 -60 5]);