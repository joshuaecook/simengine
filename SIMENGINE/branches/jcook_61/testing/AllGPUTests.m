% ALLGPUTEST - returns a test suite of all tests that are available
%
% Usage:
%   s = AllGPUTests - runs the internal set of tests
%   s = AllGPUTests('-internal') - runs the internal set of tests
%   s = AllGPUTests('-release') - runs the release set of tests <DEFAULT>
%
function s = AllGPUTests(varargin)

% Set testing modes - right now there are two modes.  One is used for
% development and has tests that will likely not pass.  The other is used
% for release and should always pass.
INTERNAL = 0; RELEASE = 1;
mode = RELEASE;

if nargin == 1
    if strcmpi(varargin{1},'-internal')
        mode = INTERNAL;
    elseif strcmpi(varargin{1},'-release')
        mode = RELEASE;
    else
        error('Simatra:AllTests', 'Unexpected argument');
    end
end


% Define the all tests suite
s = Suite('All GPU Tests');

% Pull in each of the other test suites
s.add(ReleaseCompileTestsGPU)

% Add full simulation tests
s.add(ReleaseSimulateTestsGPU)

% Add feature tests
if mode == INTERNAL
    s.add(FeatureTests('-gpu'));
else
    s.add(FeatureTests('-gpu','-release'));
end

end