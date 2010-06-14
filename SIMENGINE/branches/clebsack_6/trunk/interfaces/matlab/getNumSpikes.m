% GETNUMSPIKES - return the spike count
function count = getNumSpikes(o)

if isfield(o, 'Vs')
  output = 'Vs';
elseif isfield(o, 'Vm')
  output = 'Vm';
elseif isfield(o, 'V')
  output = 'V';
elseif isfield(o, 'u')
  output = 'u';
else
  error('No voltage output found');
end  
  
time = o.(output)(:,1);
trace = o.(output)(:,2);


if length(trace) >= 3 
  high = max(trace);
  low = min(trace);
  half = (high+low)/2;
  thresholded_trace = trace;
  thresholded_trace(find(trace < half)) = 0;
  
  d1 = diff(thresholded_trace);
  vals = zeros(1,length(trace));
  for i=2:(length(d1))
    vals(i) = (d1(i-1) > 0 && d1(i) < 0);
  end
  y = zeros(1,sum(vals));
  index = 1;
  for i=1:(length(vals))
    if vals(i)      
      y(index) = trace(i);
      index = index + 1;
    end
  end    
  
else
  y = [];
end

count = length(y);

end