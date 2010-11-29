% NEIGHBORS - Return all the neighbors of a given index into an array
%
% Usage:
%   LIST = NEIGHBORS(MATRIX, INDEX1 [, INDEX2 [, ...]] [, 'radius', INT)
%
% Description:
%   Returns a list of all the neighbors to the given matrix within a set
%   radius of the point
%
% Examples:
%   >> neighbors(10:10:100, 4)
%   [30 50]
%   >> neighbors(10:10:100, 10)
%   [90]
%   >> neighbors(magic(3), 2,2)
%   [8 1 6 3 7 4 9 2]
%
% Copyright 2010 Simatra Modeling Technologies
function varargout = neighbors(m, varargin)

% set the radius - this should be come a property in the future
radius = 1;
args = varargin;
if length(args) > 2 && ischar(args{end-1}) && strcmp(args{end-1}, 'radius') 
    radius = args{end};
    args = args(1:(end-2));
end

% grab the indices
indices = cell2mat(args);

% error checking
if ~isnumeric(radius) || ~isscalar(radius)
    error('Simatra:neighbors', 'All indices must be numeric scalars');
elseif radius <= 0
    error('Simatra:neighbors', 'Radius must be positive and greater than zero');
end
if ndims(m) ~= length(indices) 
    error('Simatra:neighbors', 'Number of indices passed in must match the number of dimensions of the matrix');
else
    for i=1:ndims(m)
        if ~isnumeric(indices(i)) || ~isscalar(indices(i))
            error('Simatra:neighbors', 'All indices must be numeric scalars');
        
        elseif indices(i) < 1 || indices(i) > size(m,i)
            error('Simatra:neighbors', 'Index %d is not within the range of (1,%d)', indices(i), size(m,i));
            
        end 
    end
end

% create a submatrix of all values that can possibly be included
subset = cell(length(indices), 1);
for i=1:length(indices)
    fwd = ceil(indices(i) + radius);
    bwd = ceil(indices(i) - radius);
    if fwd > size(m,i)
        fwd = size(m,i);
    end
    if bwd < 1
        bwd = 1;
    end
    subset{i} = bwd:1:fwd;
end

% now, take a smaller section of the matrix
all_indices = combinations(subset{:});
sub_m = reshape(m(subset{:}), length(all_indices), 1);
center = cell2mat(args);
center_mat = ones(length(all_indices),1)*center;

% test all the indices - find the Euclidean distance
distances = sqrt(sum((all_indices-center_mat).^2,2));
valid_indices = find(distances>0 & distances<=radius);
n = (sub_m(valid_indices))';

% return arguments
varargout{1} = n;
if nargout == 2
    varargout{2} = distances(valid_indices)';
end

end


% faster cell2mat
function m = cell2mat(c)
[l,w] = size(c);
if l>w
    len = l;
    m = zeros(len,1);
else 
    len = w;
    m = zeros(1,len);
end
for i=1:len
    m(i)=c{i};
end
end