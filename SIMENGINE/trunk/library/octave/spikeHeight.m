% spikeHeight - calculates the spike height of a single spike
% trace by taking the difference between the maximum value and the 
% minimum value prior to the spike
function y = spikeHeight(trace)

maximum = max(trace(2:length(trace)));
index_of_maximum = find(trace==maximum);
y = maximum - min(trace(1:index_of_maximum(1)));

end
