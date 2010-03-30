function s = FeatureTests(varargin)
%FEATURETESTS All tests that target specific features of the system
%
% Usage:
%  s = FeatureTests - runs all tests
%  s = FeatureTests('-release') - runs only those required for a release
%
%  FEATURETESTS is split up into different tests for each major feature
%  set.  The current sets are:
%   - CoreFeatureTests: Testing inputs, outputs, states, functions,
%     constants, intermediates, and built-in operators
%   - TemporalIteratorTests: Testing temporal and multi iterator constructs
%   - SpatialIteratorTests: TO COME LATER - adding in specific tests for
%     spatial iterators
%
% Option is '-release' which specifies that you want to use a restricted
% release mode of testing.  All tests are expected to pass under '-release'
% mode, while not all tests have to pass in the 'internal' mode.

% Set testing modes - right now there are two modes.  One is used for
% development and has tests that will likely not pass.  The other is used
% for release and should always pass.
INTERNAL = 0; RELEASE = 1;
mode = INTERNAL;

if nargin > 0
    target = varargin{1};
else
    target = '-cpu';
end

if nargin == 2
    if strcmpi(varargin{2},'-release')
        mode = RELEASE;
    else
        error('Simatra:FeatureTests', 'Unexpected argument');
    end
end

s = Suite(['Feature Tests ' target]);
s.add(CoreFeatureTests(target,mode));
s.add(MathFeatureTests(target,mode));
s.add(SubModelTests(target,mode));
s.add(TemporalIteratorTests(target,mode));
s.add(SampledInputTests(varargin{:}));
s.add(ParallelTests(target,mode));
s.add(DSLTests(mode));

end

