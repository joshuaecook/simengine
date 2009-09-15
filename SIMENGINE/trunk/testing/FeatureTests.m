% FEATURETESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = FeatureTests - runs all tests
%  s = FeatureTests('-release') - runs only those required for a release
%
function s = FeatureTests(varargin)

% Set testing modes - right now there are two modes.  One is used for
% development and has tests that will likely not pass.  The other is used
% for release and should always pass.
INTERNAL = 0; RELEASE = 1;
mode = INTERNAL;

if nargin == 1
    if strcmpi(varargin{1},'-release')
        mode = RELEASE;
    else
        error('Simatra:FeatureTests', 'Unexpected argument');
    end
end

s = Suite('Feature Tests');

% Add each of the language feature tests
s.add(OutputFeatureTests);
s.add(InputFeatureTests);
s.add(StateFeatureTests);
s.add(InlineFunctionFeatureTests);
s.add(ConstantFeatureTests);
s.add(IntermediateFeatureTests);
s.add(FunctionFeatureTests);

end


function s = OutputFeatureTests

s = Suite('Output Feature Tests');

end

function s = InputFeatureTests

s = Suite('Input Feature Tests');

end

function s = StateFeatureTests

s = Suite('State Feature Tests');

end

function s = InlineFunctionFeatureTests

s = Suite('Inline Function Feature Tests');

end

function s = ConstantFeatureTests

s = Suite('Constant Feature Tests');

end

function s = IntermediateFeatureTests

s = Suite('Intermediate Feature Tests');

end

function s = FunctionFeatureTests

s = Suite('Function Feature Tests');

end
