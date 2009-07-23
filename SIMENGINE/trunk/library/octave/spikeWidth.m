% spikeWidth - use the 50% of spike height threshold to
% determine the spike width.  This only works if the simulation
% includes one spike
function y = spikeWidth(t, trace)

% first grab the spike height
maximum = max(trace(2:length(trace)));
index_of_maximum = find(trace==maximum);
minimum =  min(trace(1:index_of_maximum(1)));
spikeHeight = maximum - minimum;

% use a threshold function to determine where to measure spike
% width - arbitrarily choose the 50% line
threshold = spikeHeight / 2 + minimum;

% grab the indices where the spike crosses the threshold
before_min_index = max(find(trace(1:index_of_maximum) < threshold));
timeA = t(before_min_index);
valA = trace(before_min_index);
timeB = t(before_min_index+1);
valB = trace(before_min_index+1);
slope = (valB - valA)/(timeB - timeA);
y_int = (timeB*valA - timeA*valB)/(timeB - timeA);
time1 = (threshold - y_int)/slope;

% look after the spike
after_min_index = max(find(trace(index_of_maximum:length(trace)) > ...
                           threshold)) + index_of_maximum;
timeA = t(after_min_index);
valA = trace(after_min_index);
timeB = t(after_min_index-1);
valB = trace(after_min_index-1);
slope = (valB - valA)/(timeB - timeA);
y_int = (timeB*valA - timeA*valB)/(timeB - timeA);
time2 = (threshold - y_int)/slope;

% find the time differences
y = time2 - time1;


end
