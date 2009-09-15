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

s.add(Test('OutputStateDirectly',@()(simex('models_FeatureTests/OutputTest1.dsl', 10)), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputIntermediateDirectly',@()(simex('models_FeatureTests/OutputTest2.dsl', 10)), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputGroups',@()(simex('models_FeatureTests/OutputTest3.dsl', 10)), '-equal', struct('y', [0:10; 0:10; -(0:10)]')));
s.add(Test('OutputCondition',@()(simex('models_FeatureTests/OutputTest4.dsl', 10)), '-equal', struct('y', [5:10; 5:10]')));
s.add(Test('OutputTwoValues',@()(simex('models_FeatureTests/OutputTest5.dsl', 10)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:2:20]')));


end

function s = InputFeatureTests

s = Suite('Input Feature Tests');

t1 = Test('NoInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10)), '-withouterror');
t1.ExpectFail = true;
s.add(t1);
input = struct('x',5);
s.add(Test('PassedInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10, input)), '-equal', struct('y', [0:10; 0:5:50]')));

    function y = VerifyDefaultInputs
        m = simex('models_FeatureTests/InputTest2.dsl');
        y = equiv(m.default_inputs, struct('x',3));
    end

s.add(Test('VerifyInputDefaultValue', @VerifyDefaultInputs));
s.add(Test('InputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10)), '-equal', struct('y', [0:10; 0:3:30]')));
s.add(Test('OverrideInputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10, input)), '-equal', struct('y', [0:10; 0:5:50]')));

end

function s = StateFeatureTests

s = Suite('State Feature Tests');

    function y = VerifyDefaultStateInits
        m = simex('models_FeatureTests/StateTest1.dsl');
        y = all(m.default_states == [1 0]);
    end

s.add(Test('VerifyDefaultStateInits', @VerifyDefaultStateInits))
s.add(Test('EvalDefaultStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 0:2:20]')));
new_states = [1 0];
s.add(Test('ModifyStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10, new_states)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 1:2:21]')));

    function y = TestFinalStates
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10);
        y = equiv(finalStates, [11 20]);
    end
s.add(Test('TestFinalStates', @TestFinalStates));
    function y = TestFinalTime
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10);
        y = equiv(tf, 10);
    end
s.add(Test('TestFinalTime', @TestFinalTime));

end

function s = InlineFunctionFeatureTests

s = Suite('Inline Function Feature Tests');

s.add(Test('ExternalFunction',@()(simex('models_FeatureTests/FunctionTest1.dsl', 10)), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InternalFunction',@()(simex('models_FeatureTests/FunctionTest2.dsl', 10)), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InlineEquationFunction',@()(simex('models_FeatureTests/FunctionTest3.dsl', 10)), '-equal', struct('y', [0:10; (0:10).^2]')));

end

function s = ConstantFeatureTests

s = Suite('Constant Feature Tests');

s.add(Test('OneConstant',@()(simex('models_FeatureTests/ConstantTest1.dsl', 10)), '-equal', struct('y', [0:10; 0:10]')));

end

function s = IntermediateFeatureTests

s = Suite('Intermediate Feature Tests');

end

function s = FunctionFeatureTests

s = Suite('Function Feature Tests');

s.add(Test('FunctionModulus', @()(simex('models_FeatureTests/FunctionTestModulus.dsl', 10)), '-equal', struct('y', [0:2:10, 0:2:10]')))

end
