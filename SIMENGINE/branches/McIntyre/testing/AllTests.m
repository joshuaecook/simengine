% ALLTEST - returns a test suite of all tests that are available
%
% Usage:
%   s = AllTests - runs the internal set of tests
%   s = AllTests('-internal') - runs the internal set of tests
%   s = AllTests('-release') - runs the release set of tests <DEFAULT>
%
function s = AllTests(varargin)

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
s = Suite('All Tests');

% Pull in each of the other test suites
s.add(ReleaseCompileTests)
% The below test won't pass because the model name is different than the
% file name.  This is expected
s.getTest('Release Compile Tests').getTest('CPU - Model neuronWithSynapse').ExpectFail = true;
s.getTest('Release Compile Tests').getTest('GPU - Model neuronWithSynapse').ExpectFail = true;

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
    s.add(FeatureTests);
else
    s.add(FeatureTests('-release'));
end

% Add message tests (for checking compiler output)
s.add(MessageTests)

end