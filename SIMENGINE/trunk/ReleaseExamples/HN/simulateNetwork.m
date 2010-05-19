%compile the hco.dsl model and save model interface data
modelInfo = simex('timingNetwork.dsl');

%set up a good initial state vector
initState = modelInfo.defaultStates;
initState(initState == -45) = rand(1, length(initState(initState == -45))) ...
    *10 - 55;
[~, initState, ~] = simex('timingNetwork.dsl', 100, '-resume', initState);

%run the model with default parameters
data = simex('timingNetwork.dsl', 200, '-resume', initState);

t = data.VmL1(:,1)-100;

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