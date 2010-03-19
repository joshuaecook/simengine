% ALLCPUTEST - returns a test suite of all tests that are available
%
% Usage:
%   s = AllCPUTests - runs the internal set of tests
%   s = AllCPUTests('-internal') - runs the internal set of tests
%   s = AllCPUTests('-release') - runs the release set of tests <DEFAULT>
%
function s = AllCPUTests(varargin)

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
s = Suite('All CPU Tests');

% Pull in each of the other test suites
s.add(ReleaseCompileTests)

% Add full simulation tests
s.add(ReleaseSimulateTests)

% Add tests for each solver
s.add(SolverTests)

% Additional compilation tests for internal use
if mode == INTERNAL
    s.add(InternalCompileTests)
end

% Add feature tests
if mode == INTERNAL
    s.add(FeatureTests('-cpu'));
else
    s.add(FeatureTests('-cpu','-release'));
end

end