%function [y] = withinPercent(x, value, p);
%
% returns true if x is within +/- p% of value
function [y] = withinPercent(x, value, p);

above = value * (1 + p/100);
below = value * (1 - p/100);

y = x <= above & x >= below;

end
