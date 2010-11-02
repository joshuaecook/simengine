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


% Timeout after 10 minutes to make sure testing framework never hangs
global SIMEX_TIMEOUT;
SIMEX_TIMEOUT = 600;

% Stop text status bar to avoid polluting Bamboo
global SIMEX_NO_TEXT_STATUS_BAR;
SIMEX_NO_TEXT_STATUS_BAR = true;

% Define the all tests suite
s = Suite('All Tests');

if mode == RELEASE
  s.add(ML_CoreFeatureTests('-cpu', '-release'), {'cpu', 'matlab', 'core'});
  s.add(ReleaseMatlabExampleTests, {'cpu', 'examples', 'matlab'})
  s.add(ML_SyntaxTests('-cpu', '-release'), {'cpu', 'matlab'});
  s.add(ML_SubModelTests('-cpu', '-release'), {'cpu', 'matlab', 'submodels'});
  s.add(AllCPUTests('-release'), {'cpu'});
  s.add(AllGPUTests('-release'), {'gpu'});
  s.add(DSLTests('-release'), {'dsl'});
else
  s.add(ML_CoreFeatureTests, {'cpu', 'matlab', 'core'});
  s.add(ReleaseMatlabExampleTests, {'cpu', 'examples', 'matlab'})
  s.add(ML_SyntaxTests('-cpu'), {'cpu', 'matlab'});
  s.add(ML_SubModelTests('-cpu'), {'cpu', 'matlab', 'submodels'});
  s.add(AllCPUTests, {'cpu'});
  s.add(AllGPUTests, {'gpu'});
  s.add(DSLTests(), {'dsl'});
end

% Add message tests (for checking compiler output)
s.add(MessageTests)

end