%function [y] = withinRange(x, below, above);
%
% returns true if x is within +/- p% of value
function [y] = withinRange(x, below, above);

y = x <= above & x >= below;

end
