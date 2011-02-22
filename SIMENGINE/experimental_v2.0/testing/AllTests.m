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
% NOTE: this has been superceded by tags
INTERNAL = 0; RELEASE = 1;
mode = RELEASE;

% Timeout after 10 minutes to make sure testing framework never hangs
global SIMEX_TIMEOUT;
SIMEX_TIMEOUT = 600;

% Stop text status bar to avoid polluting Bamboo
global SIMEX_NO_TEXT_STATUS_BAR;
SIMEX_NO_TEXT_STATUS_BAR = true;

% Define the all tests suite
s = Suite('All Tests');

s.add(ML_Tests, {'matlab'});
s.add(AllCPUTests('-release'), {'cpu'});
s.add(AllGPUTests('-release'), {'gpu'});
s.add(DSLTests('-release'), {'dsl'});

% Add message tests (for checking compiler output)
s.add(MessageTests, {'messages'})

end