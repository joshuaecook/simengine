% NEIGHBORS - Return all the neighbors of a given index into an array
%
% Usage:
%   [LIST] = NEIGHBORS(MATRIX, INDEX1 [, INDEX2 [, ...]] [, OPTIONS])
%   [LIST, DISTANCE] = NEIGHBORS(MATRIX, INDEX1 [, INDEX2 [, ...]] [, OPTIONS])
%
% Description:
%   Returns a list of all the neighbors to the given matrix within a set
%   radius of the point, by default, step is set to one
%
% Options:
%  '-radius', R - return all the points that are geometrically within a
%  given radius R. The default is R.
%  '-radius', [INNER OUTER] - return all the points that are
%  geometrically outside of the INNER radius and inside the OUTER radius
%  '-offsets', {REL_IDX1 [, RELIND2 [,...]]} - return all the points that
%  are within the specified relative offsets.
%  '-steps', [INNER OUTER] - return all the points that are inclusively
%   within a set number of steps
%  '-indices' - return a list of indices instead of the values
%
% Examples:
%   >> neighbors(10:10:100, 4)
%   [30 50]
%   >> neighbors(10:10:100, 10)
%   [90]
%   >> neighbors(magic(3), 2, 2)
%   [3 1 9 7]
%   >> neighbors(magic(3), 2, 2, '-offsets', {[1 1], [-1 -1]})
%   [2 8]
%   >> [l, d] = neighbors(magic(3), 2, 2, '-offsets', {[1 0], [1 1]})
%   l =
%       9     2
%   d =
%       1.0000    1.4142
%   >> neighbors(magic(3), 2, 2, '-radius', [1.1 1.5], '-indices')
%   [[1 3 1 3];
%    [1 1 3 3]]
%
% Copyright 2010 Simatra Modeling Technologies
function varargout = neighbors(m, varargin)

% set the default options 
opts.radius = 1;
opts.offsets = {};
opts.diagonal = false;
opts.indices = false;

args = varargin;
i = 1;
grab_indices = true;
while ~isempty(args)
    arg = args{1};
    if ischar(arg) 
        if grab_indices 
            grab_indices = false;
            if i == 1
                error('Simatra:neighbors', 'Must specify an index into the data');
            end
            indices = cell2mat(varargin(1:(i-1)));
        end
        switch lower(arg)
            case '-radius'
                if length(args) > 1
                    r = args{2};
                    if isscalar(r)
                        if isreal(r) && r>=0 
                            opts.radius = r;
                        else
                            error('Simatra:neightbors', 'radius requires a real quantity >= 0');
                        end
                    elseif length(r) == 2
                        if r(1) <= r(2) && r(1) >= 0
                            opts.radius = r;
                        else
                            error('Simatra:neighbors', 'radius requires that inner radius <= outer radius');
                        end
                    else
                        error('Simatra:neighbors', 'radius must be a scalar or an array of length two');
                    end
                    i = i + 2;
                else
                    error('Simatra:neighbors','radius requires an option');
                end
            case '-offsets'
                if length(args) > 1
                    s = args{2};
                    if ~iscell(s)
                        s = {s};
                    end
                    offsets = zeros(length(s),length(indices));
                    for ind=1:length(s)
                        if ~isnumeric(s{ind})
                            error('Simatra:neighbors', 'offsets must be numeric arrays');
                        end
                        if length(s{ind}) ~= length(indices)
                            error('Simatra:neighbors', 'offsets must be arrays of length %d', length(indices));
                        end
                        if ~all(floor(s{ind})==s{ind})
                            error('Simatra:neighbors', 'offsets must be expressed as integers');
                        end
                        offsets(ind,:) = s{ind};
                    end
                    opts.offsets = offsets;
                    i = i + 2;
                else
                    error('Simatra:neighbors','offsets requires an option');
                end
            case '-indices'
                opts.indices = true;
                i = i + 1;
            otherwise
                error('Simatra:neighbors', 'Unknown option %s specified', arg);
        end
    else
        i = i + 1;
    end
    
    % return the remaining args
    args = varargin(i:end);
end
if grab_indices
    if i == 1
        error('Simatra:neighbors', 'Must specify an index into the data');
    end
    indices = cell2mat(varargin(1:(i-1)));
end

% error checking
%if ~isnumeric(radius) || ~isscalar(radius)
%    error('Simatra:neighbors', 'All indices must be numeric scalars');
%elseif radius <= 0
%    error('Simatra:neighbors', 'Radius must be positive and greater than zero');
%end
if numdims(m) ~= length(indices) 
    error('Simatra:neighbors', 'Number of indices passed in must match the number of dimensions of the matrix');
else
    num_dimensions = numdims(m);
    if num_dimensions == 1
        if ~isnumeric(indices) || ~isscalar(indices)
            error('Simatra:neighbors', 'All indices must be numeric scalars');
            
        elseif indices < 1 || indices > length(m)
            error('Simatra:neighbors', 'Index %d is not within the range of (1,%d)', indices(i), size(m,i));
            
        end
        
    else
        for i=1:numdims(m)
            if ~isnumeric(indices(i)) || ~isscalar(indices(i))
                error('Simatra:neighbors', 'All indices must be numeric scalars');
                
            elseif indices(i) < 1 || indices(i) > size(m,i)
                error('Simatra:neighbors', 'Index %d is not within the range of (1,%d)', indices(i), size(m,i));
                
            end
        end
    end
end

% use a radius if offsets are not explicit
if isempty(opts.offsets)
    % create a submatrix of all values that can possibly be included
    subset = cell(length(indices), 1);
    for i=1:length(indices)
        fwd = ceil(indices(i) + opts.radius);
        bwd = ceil(indices(i) - opts.radius);
        if length(indices) == 1
            if fwd > length(m)
                fwd = length(m);
            end
        else
            if fwd > size(m,i)
                fwd = size(m,i);
            end
        end
        if bwd < 1
            bwd = 1;
        end
        subset{i} = bwd:1:fwd;
    end
        
    % now, take a smaller section of the matrix
    all_indices = combinations(subset{:});
    sub_tensor = m(subset{:});
    sub_m = reshape(sub_tensor, length(all_indices), 1);
    center = indices;
    center_mat = ones(length(all_indices),1)*center;
    
    % test all the indices - find the Euclidean distance
    distances = sqrt(sum((all_indices-center_mat).^2,2));
    if isscalar(opts.radius)
        valid_indices = find(distances>0 & distances<=opts.radius);
    else
        valid_indices = find(distances>0 & distances>=opts.radius(1) & distances<=opts.radius(2));
    end
    n = (sub_m(valid_indices))';
    
    % return arguments
    if opts.indices
        subset_indices = cell(1,length(center));
        [subset_indices{:}] = ind2sub(size(sub_tensor),valid_indices);
        s = zeros(length(subset_indices),length(subset_indices{1}));
        for i=1:length(subset_indices)
            s(i,:) = subset_indices{i} + subset{i}(1)-1;
        end
        varargout{1} = s;
    else
        varargout{1} = n;
    end
    if nargout == 2
        varargout{2} = distances(valid_indices)';
    end

% else use offsets
else
    % create a list of possible indices, then prune
    center = indices;
    possible_indices = ones(size(opts.offsets,1),1)*center + opts.offsets;
    
    % prune the indices if they are not valid
    valid_entries = logical(zeros(1,size(possible_indices,1)));
    for i=1:size(possible_indices,1)
        row_valid = true;
        for j=1:size(possible_indices,2)
            if numdims(m) == 1
                if possible_indices(i,j) <= 0 || possible_indices(i,j) > length(m)
                    row_valid = false;
                end                
            else
                if possible_indices(i,j) <= 0 || possible_indices(i,j) > size(m,j)
                    row_valid = false;
                end
            end
        end
        valid_entries(i) = row_valid;
    end
    valid_indices = possible_indices(valid_entries,:);
    
    % return the results
    if opts.indices
        varargout{1} = valid_indices';
    else
        split_indices = num2cell(valid_indices,2);
        entries = zeros(1,size(valid_indices,1));
        for i=1:size(valid_indices,1)
            ind = num2cell(split_indices{i});
            entries(i) = m(ind{:});
        end
        varargout{1} = entries;
    end
    
    % return the distances if requested
    if nargout == 2
        center_mat = ones(size(valid_indices,1),1)*center;
        distances = sqrt(sum((valid_indices-center_mat).^2,2));
        varargout{2} = distances';
    end
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

% function numdims (ndims that returns one for an array)
function n = numdims(m)

if ndims(m) == 2
    s = size(m);
    if s(1) == 1
        n = 1;
    elseif s(2) == 1
        n = 1;
    else
        n = 2;
    end
else
    n = ndims(m);
end

end