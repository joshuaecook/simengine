% COMBINATIONS - take all combinations of the input arguments
%
% Usage:
%   MAT = COMBINATIONS(VEC1 [, VEC2 [, ...]])
%
% Example:
% >> m = combinations([1,2], 3, [4,5,6])
% 
% m =
% 
%      1     3     4
%      2     3     4
%      1     3     5
%      2     3     5
%      1     3     6
%      2     3     6
%   
% Copyright 2010 Simatra Modeling Technologies
function l = combinations(varargin)

% Grab the lengths of each of the input arguments
lengths = cell2mat(List.map(@(l)(length(l)), varargin));
dims = length(lengths);

% Initialize the return matrix - each row is an input combination while
% each column is the input value
l = zeros(prod(lengths),dims);

% Loop through each dimension (not each input combination) and produce the 
% vector using only matrix operations
for i=1:dims
    before = prod(lengths(1:(i-1)));
    after = prod(lengths((i+1):dims));
    pre_mat = ones(before,1)*varargin{i};
    pre_vec = reshape(pre_mat, 1, numel(pre_mat));
    post_mat = pre_vec'*ones(1,after);
    post_vec = reshape(post_mat, 1, numel(post_mat));
    l(:, i) = post_vec;
end

end