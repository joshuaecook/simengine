function simulateHCO()

% Create the hco model
hco = createHCO;

%%
% Next, the model can be executed by calling simex with a simulation
% time parameter after the model name, and the resulting data can be
% plotted using the simplot command:

data1 = simex(hco, 100);
figure,
simplot(data1)
xlabel('Time (s)')
ylabel('V_m (mV)')
title('HCO model with default state (no HCO activity)')

%%
% This simulation, however, does not show half-center activity because
% the two hn34 models are "state-locked"; their output is identical
% because their model equations and initial state are identical.  In
% order to correct this, we need to create a different set of initial
% conditions for the half-center oscillator.  There are two ways to do
% this.  First, we can set the *Iext* parameter of one of the cells to a
% different value, then run the model for enough time for the cells to
% reach a different state.  We can then save the final state vector
% using simex:

% set a stimulus current to R4, simulate and save the final state
parameters.stimR4 = 1;
[ignore, finalState, finalTime] = simex(hco, 100, parameters);

% save the final state of the previous simulation as our new initial state
initialState = finalState;
% run simulation with default parameters and new initial conditions
data2 = simex(hco, 100, '-resume', initialState);

%%
% When we run simex using the '-resume' option, we start the simulation
% with the initial conditions stored in *initialState*.  An alternative
% method to alter the initial state is to include an input parameter that
% is used as an initial state value, as is done in *exploreEleakGleak.m*.
% Regardless as to the approach used, we should now see the two cells of
% the network enter half-center activity.

figure
simplot(data2)
xlabel('Time (s)')
ylabel('V_m (mV)')
title('HCO model with modified initial state (HCO activity)')

% simulate the model over the Eleak-gleak parameter space
EleakValues = -65:0.5:-55;
gleakValues = 4:.5:12;
tic
[cellData, HCOData] = exploreEleakGleak(EleakValues, gleakValues);
disp([num2str(2*length(EleakValues)*length(gleakValues)) ' simulations ' ...
    'took ' num2str(toc) ' seconds to complete']);
    
figure

subplot(1,2,1)
surf(EleakValues, gleakValues, cellData);
view(0,90)
title('Burtsing activity region for single cell')
xlabel('gleak (nS)')
ylabel('Eleak (mV)')

subplot(1,2,2)
surf(EleakValues, gleakValues, HCOData);
view(0,90)
title('Burtsing activity region for half-center')
xlabel('gleak (nS)')
ylabel('Eleak (mV)')

end