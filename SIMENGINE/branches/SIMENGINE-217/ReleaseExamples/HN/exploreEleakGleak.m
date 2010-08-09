function [singleCellData, HCOData] = exploreEleakGleak(EleakValues, gleakValues)
%sweeps the hn34 single cell and hco models over the Eleak gleak parameter space

parameters.Eleak = num2cell(repmat(EleakValues, 1, length(gleakValues)));
tmp = repmat(gleakValues, length(EleakValues), 1);
parameters.gleak = num2cell(tmp(:)');

data = simex('hn34.dsl', 100, parameters);

activity = zeros(length(gleakValues), length(EleakValues));

for i = 1:length(gleakValues),
    for j = 1:length(EleakValues),
        idx = (i-1)*length(EleakValues)+j;      
        t = data(idx).Vm(:,1);
        Vm = data(idx).Vm(:,2);
        peakIdx = find(Vm(2:end-1) > Vm(3:end) & Vm(2:end-1) > Vm(1:end-2) & Vm(2:end-1) > -20);
        spikeTimes = t(peakIdx + 1);
        spikeTimes = spikeTimes(spikeTimes > 50);
        if isempty(spikeTimes),
          activity(i,j) = 0;  %neuron is silent
        elseif min(diff(spikeTimes)) > 0.95*mean(diff(spikeTimes))
          activity(i,j) = 1;  %neuron is spiking
        else
            activity(i,j) = 2;  %neuron is bursting
        end
    end
end

singleCellData = activity;

modelData = simex('hco.dsl');
initConditions = modelData.defaultStates;
initConditions(17) = -55;

data = simex('hco.dsl', 100, parameters, '-resume', initConditions);

activity = zeros(length(gleakValues), length(EleakValues));

for i = 1:length(gleakValues),
    for j = 1:length(EleakValues),
        idx = (i-1)*length(EleakValues)+j;      
        t = data(idx).VmL3(:,1);
        VmL = data(idx).VmL3(:,2);
        VmR = data(idx).VmR3(:,2);
        peakIdxL = find(VmL(2:end-1) > VmL(3:end) & VmL(2:end-1) > VmL(1:end-2) & VmL(2:end-1) > -20);
        peakIdxR = find(VmR(2:end-1) > VmR(3:end) & VmR(2:end-1) > VmR(1:end-2) & VmR(2:end-1) > -20);
        spikeTimesL = t(peakIdxL + 1);
        spikeTimesL = spikeTimesL(spikeTimesL > 50);
        spikeTimesR = t(peakIdxR + 1);
        spikeTimesR = spikeTimesR(spikeTimesR > 50);
        if isempty(spikeTimesL) && isempty(spikeTimesR),
          activity(i,j) = 0;  %both neurons are silent
        elseif min(diff(spikeTimesL)) > 0.95*mean(diff(spikeTimesL)) | ...
            min(diff(spikeTimesR)) > 0.95*mean(diff(spikeTimesR)),
          activity(i,j) = 1;  %one neuron is spiking
        else
          activity(i,j) = 2;  %neurons are bursting
        end
    end
end

HCOData = activity;

end
