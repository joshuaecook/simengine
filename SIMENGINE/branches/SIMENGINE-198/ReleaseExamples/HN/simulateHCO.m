%compile the hco.dsl model and save model interface data
modelInfo = simex('hco.dsl');

%run the model with default states and paramters
%output will NOT show half-center activity
data1 = simex('hco.dsl', 100);
figure,
simplot(data1)
xlabel('Time (s)')
ylabel('V_m (mV)')
title('HCO model with default state (no HCO activity)')

%set a stimulus current to R4, simulate and save the final state
parameters.stimR4 = 1;
[~, finalState, finalTime] = simex('hco.dsl', 100, parameters);

%save the final state of the previous simulation as our new initial state
initialState = finalState;
%run simulation with default parameters and new initial conditions
data2 = simex('hco.dsl', 100, '-resume', initialState);
figure
simplot(data2)
xlabel('Time (s)')
ylabel('V_m (mV)')
title('HCO model with modified initial state (HCO activity)')

%simulate the model over the Eleak-gleak parameter space
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