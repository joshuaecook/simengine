% PIECEWISE - create a piecewise function in MATLAB
%
% Usage:
%   PIECEWISE(VALUE1, CONDITION1 [,VALUE2, CONDITION2 [...]], OTHERWISE);
%
% Description:
%   MATLAB natively includes an if statement to choose between different
%   code paths.  However, there is no apparent functional way to 
%   conditionally choose expressions.  In C, there is the "? :" operator,
%   which is a ternary operation choosing between two different options.
%   Here, this piecewise function offers multiple options, where the value
%   is succeeded by the condition. The last argument is the default case if
%   no other condition holds true.
%
% Examples:
%
%   % the absolute value function
%   y = piecewise(-x, x<0,...
%                  x);
%
%   % the atan2 function (2 argument arctangent)
%   y = piecewise(atan(y/x),     x > 0,            ...
%                 pi+atan(y/x),  y >= 0 && x < 0,  ...
%                 -pi+atan(y/x), y < 0 && x < 0,   ...
%                 pi/2,          y > 0 && x == 0,  ...
%                 -pi/2,         y < 0 && x == 0,  ...
%                 NaN); % otherwise undefined
%
% See also EXP/PIECEWISE
%
% Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
% Website: www.simatratechnologies.com
% Email: support@simatratechnologies.com
%
function r = piecewise(varargin)

% Check arguments first
isOdd = @(x)(mod(x,2)==1);
if ~isOdd(nargin)
    error('Simatra:piecewise', 'Piecewise function expects to take in an odd number of arguments.');
end

% Also check to make sure that every even second argument is a logical type
for i=2:2:nargin
    if ~islogical(varargin{i})
        error('Simatra:piecewise', 'Every second argument must be of logical type.');
    end
    % this simple function only supports scalar logicals right now
    if ~isscalar(varargin{i})
        error('Simatra:piecewise', 'All conditions must be scalar types.');
    end        
end

% Now, perform the check
r = varargin{end};
for i=2:2:nargin
    if varargin{i}
        r = varargin{i-1};
        break;
    end
end

end