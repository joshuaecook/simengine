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

% This is a critical defect where the below test causes MATLAB to crash.
% I'm adding a dummy placeholder failure in its place
%s.add(Test('OutputNoValues',@()(simex('models_FeatureTests/OutputTest6.dsl', 10)), '-equal', struct()));
s.add(Test('OutputNoValues (DUMMY)', @()(false)));

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
new_states = [0 1];
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
s.add(Test('StateWithoutEquation', @()(simex('models_FeatureTests/StateTest2.dsl', 10)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 5*ones(1,11)]')));


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
t = Test('TwoConstants',@()(simex('models_FeatureTests/ConstantTest2.dsl', 10)), '-withouterror');
t.ExpectFail = true; % there are two constants in this file, so it should produce an error
s.add(t);
t = Test('Constant+Intermediate',@()(simex('models_FeatureTests/ConstantTest3.dsl', 10)), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by an intermediate in this file, so it should produce an error
s.add(t);
t = Test('Constant+State',@()(simex('models_FeatureTests/ConstantTest4.dsl', 10)), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by a state in this file, so it should produce an error
s.add(t);

    function y = InternalConstants
        o = simex('models_FeatureTests/ConstantTest5.dsl', 1);
        y = approx_equiv(o.e_const(end,2), exp(1), 1e-5) && approx_equiv(o.pi_const(end,2), pi, 1e-5);
    end
s.add(Test('InternalConstants',@InternalConstants));

end

function s = IntermediateFeatureTests

s = Suite('Intermediate Feature Tests');
s.add(Test('Intermediate=State', @()(simex('models_FeatureTests/IntermediateTest1.dsl', 10)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:10]')));
s.add(Test('Intermediate=Input', @()(simex('models_FeatureTests/IntermediateTest2.dsl', 10)), '-equal', struct('s', [0:10; 0:10]', 'y', [0:10; ones(1,11)]')));
s.add(Test('InputToOutput', @()(simex('models_FeatureTests/IntermediateTest3.dsl', 10)), '-equal', struct('s', [0:10; 0:10]', 'x', [0:10; ones(1,11)]')));
s.add(Test('Intermediate=Derivative', @()(simex('models_FeatureTests/IntermediateTest4.dsl', 10)), '-equal', struct('s', [0:10; 0:10]', 'y', [0:10; ones(1,11)]')));

end

function s = FunctionFeatureTests

s = Suite('Function Feature Tests');

s.add(Test('FunctionModulus', @()(simex(['models_FeatureTests/' ...
                    'FunctionTestModulus.dsl'], 10)), '-equal', ...
           struct('y', [0:10; 0 1 0 1 0 1 0 1 0 1 0]')))
    function y = FunctionTrig
        o = simex('models_FeatureTests/FunctionTestTrig1.dsl', 10);
        tol = 1e-3;
        y = approx_equiv(sin(o.y(:,1)),o.y(:,2),tol) && ...
            approx_equiv(cos(o.y(:,1)),o.y(:,3),tol) && ...
            approx_equiv(tan(o.y(:,1)),o.y(:,4),tol) && ...
            approx_equiv(csc(o.y(:,1)),o.y(:,5),tol) && ...
            approx_equiv(sec(o.y(:,1)),o.y(:,6),tol) && ...
            approx_equiv(cot(o.y(:,1)),o.y(:,7),tol);
    end
s.add(Test('FunctionTrig', @FunctionTrig));
    function y = FunctionInverseTrig
        o = simex('models_FeatureTests/FunctionTestTrig2.dsl', 10);
        tol = 1e-3;
        y = approx_equiv(asin(o.y(:,1)),o.y(:,2),tol) && ...
            approx_equiv(acos(o.y(:,1)),o.y(:,3),tol) && ...
            approx_equiv(atan(o.y(:,1)),o.y(:,4),tol) && ...
            approx_equiv(acsc(o.y(:,1)),o.y(:,5),tol) && ...
            approx_equiv(asec(o.y(:,1)),o.y(:,6),tol) && ...
            approx_equiv(acot(o.y(:,1)),o.y(:,7),tol);
    end
s.add(Test('FunctionInverseTrig', @FunctionInverseTrig));
    function y = FunctionHyperbolicTrig
        o = simex('models_FeatureTests/FunctionTestTrig3.dsl', 10);
        tol = 1e-3;
        y = approx_equiv(sinh(o.y(:,1)),o.y(:,2),tol) && ...
            approx_equiv(cosh(o.y(:,1)),o.y(:,3),tol) && ...
            approx_equiv(tanh(o.y(:,1)),o.y(:,4),tol) && ...
            approx_equiv(csch(o.y(:,1)),o.y(:,5),tol) && ...
            approx_equiv(sech(o.y(:,1)),o.y(:,6),tol) && ...
            approx_equiv(coth(o.y(:,1)),o.y(:,7),tol);
    end
s.add(Test('FunctionHyperbolicTrig', @FunctionHyperbolicTrig));
    function y = FunctionInverseHyperbolicTrig
        o = simex('models_FeatureTests/FunctionTestTrig4.dsl', 10);
        tol = 1e-3;
        y = approx_equiv(asinh(o.y(:,1)),o.y(:,2),tol) && ...
            approx_equiv(acosh(o.y(:,1)),o.y(:,3),tol) && ...
            approx_equiv(atanh(o.y(:,1)),o.y(:,4),tol) && ...
            approx_equiv(acsch(o.y(:,1)),o.y(:,5),tol) && ...
            approx_equiv(asech(o.y(:,1)),o.y(:,6),tol) && ...
            approx_equiv(acoth(o.y(:,1)),o.y(:,7),tol);
    end
s.add(Test('FunctionInverseHyperbolicTrig', @FunctionInverseHyperbolicTrig));


end
