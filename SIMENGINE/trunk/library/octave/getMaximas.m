% getMaximas.m - returns all the values of the maximas found
% in the trace
function y = getMaximas(trace)

if length(trace) >= 3 
  d1 = diff(trace);
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
    
endfunction
