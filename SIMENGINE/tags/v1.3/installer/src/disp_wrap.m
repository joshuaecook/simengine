% DISP_WRAP line wrap printer
% Usage:
%   DISP_WRAP(STRING)
%   DISP_WRAP(STRING, COLS)
%
% Copyright 2010 Simatra Modeling Technologies
%
function [varargout] = disp_wrap(str, varargin)

% set the default number of columns
try
    size = get(0, 'CommandWindowSize');
    cols = size(1) - 3; % Subtract a few as a buffer
    rows = size(2);
catch
    cols = 80;
end

% otherwise, use the number of columns passed into p
if nargin > 2
    error('Simatra:p', 'Only two arguments supported to p')
elseif nargin == 2
    cols = varargin{1};
end

% split the string up into lines
lines = strread(str, '%s', 'delimiter', sprintf('\n'));

% perform the wrapping
match = ['(.{1,' num2str(cols) '})(\s+)'];
wrapped_lines = cell(length(lines),1);
for i=1:length(lines)
    if isempty(lines{i})
        wrapped_lines{i} = sprintf('\n');
    else
        wrapped_lines{i} = [strtrim(regexprep([lines{i} ' '], match, '$1\n')) sprintf('\n')];
    end
end
wrapped_str = strtrim([wrapped_lines{:}]);

% if there's an output argument, save the output, otherwise display it
if nargout == 1
    varargout{1} = wrapped_str;
else
    disp(wrapped_str)
end

end
