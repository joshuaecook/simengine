% H = SIMPLOT(VARARGIN) plots simulation structure or matrices in the same form as the
% general plot command.  If simplot encounters a matrix, the first column
% is set aside as a time vector for each subsequent column.
%
% EXAMPLE
%
% m = buildEngine('mymodel.dsl')
% s = mymodel(100);
% simplot(s);
%
function varargout = simplot(varargin)

if nargin == 0 
    error('Simatra:simplot:argumentError', 'Need at least one argument to simplot');
    return
end

% grab the current axes
gcf;
h = gca;
% if hold is not set, clear the figure
%if strcmp(get(h, 'nextplot'),'replace')
%    clf;
%end

% initialize the plotting args
plotargs = {};

% go through each of the arguments..
for i=1:length(varargin)
   item = varargin{i};
   if isnumeric(item)
       dims = size(item);
       if dims(2) == 1 
           plotargs = append(plotargs, item);
       else
           for j=2:dims(2)
               plotargs = append(plotargs, item(:,1));
               plotargs = append(plotargs, item(:,j));
           end
       end
   elseif ischar(item)
       plotargs = append(plotargs, item);
   elseif isstruct(item)
       fields = fieldnames(item);
       for j=1:length(fields)
           field = item.(fields{j});
           if isnumeric(field)
               dims = size(field);
               if dims(2) == 1
                   plotargs = append(plotargs, 1:dims(1));
                   plotargs = append(plotargs, field);
               else
                   for j=2:dims(2)
                       plotargs = append(plotargs, field(:,1));
                       plotargs = append(plotargs, field(:,j));
                   end
               end
           end   
       end
   end
end


% plot the new set of generated arguments
h = plot(plotargs{:});

if nargout > 0
    varargout{1} = h;
end

end

function y = append(x, i)

    y = x;
    y{length(x)+1} = i;

end
