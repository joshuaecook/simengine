% COREFEATURETESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = CoreFeatureTests - runs all tests
%  s = CoreFeatureTests('-release') - runs only those required for a release
%
function s = CoreFeatureTests(varargin)

if nargin == 0
    mode = 1;
else
    mode = varargin{1};
end

s = Suite('Core Feature Tests');

% Add each of the language feature tests
s.add(OutputFeatureTests);
s.add(InputFeatureTests);
s.add(StateFeatureTests(mode));
s.add(InlineFunctionFeatureTests);
s.add(ConstantFeatureTests);
s.add(IntermediateFeatureTests(mode));
s.add(FunctionFeatureTests);
s.add(DifferenceEquationTests);

end


function s = OutputFeatureTests

s = Suite('Output Feature Tests');

s.add(Test('OutputStateDirectly',@()(simex('models_FeatureTests/OutputTest1.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputIntermediateDirectly',@()(simex('models_FeatureTests/OutputTest2.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputGroups',@()(simex('models_FeatureTests/OutputTest3.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; 0:10; -(0:10)]')));
s.add(Test('OutputCondition',@()(simex('models_FeatureTests/OutputTest4.dsl', 10,'-quiet')), '-equal', struct('y', [5:10; 5:10]')));
s.add(Test('OutputTwoValues',@()(simex('models_FeatureTests/OutputTest5.dsl', 10,'-quiet')), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:2:20]')));
s.add(Test('OutputNoValues',@()(simex('models_FeatureTests/OutputTest6.dsl', 10,'-quiet')), '-equal', struct()));

end

function s = InputFeatureTests

s = Suite('Input Feature Tests');

t1 = Test('NoInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10,'-quiet')), '-withouterror');
t1.ExpectFail = true;
s.add(t1);
input = struct('x',5);
s.add(Test('PassedInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10, input,'-quiet')), '-equal', struct('y', [0:10; 0:5:50]')));

    function y = VerifyDefaultInputs
        m = simex('models_FeatureTests/InputTest2.dsl','-quiet');
        y = equiv(m.default_inputs, struct('x',3));
    end

s.add(Test('VerifyInputDefaultValue', @VerifyDefaultInputs));
s.add(Test('InputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; 0:3:30]')));
s.add(Test('OverrideInputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10, input,'-quiet')), '-equal', struct('y', [0:10; 0:5:50]')));

end

function s = StateFeatureTests(mode)
INTERNAL = 0; RELEASE = 1;

s = Suite('State Feature Tests');

    function y = VerifyDefaultStateInits
        m = simex('models_FeatureTests/StateTest1.dsl','-quiet');
        y = all(m.default_states == [1 0]);
    end

s.add(Test('VerifyDefaultStateInits', @VerifyDefaultStateInits))
s.add(Test('EvalDefaultStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10,'-quiet')), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 0:2:20]')));
new_states = [0 1];
s.add(Test('ModifyStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10, new_states,'-quiet')), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 1:2:21]')));

    function y = TestFinalStates
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10,'-quiet');
        y = equiv(finalStates, [11 20]);
    end
s.add(Test('TestFinalStates', @TestFinalStates));
    function y = TestFinalTime
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10,'-quiet');
        y = equiv(tf, 10);
    end
s.add(Test('TestFinalTime', @TestFinalTime));
s.add(Test('StateWithoutEquation', @()(simex('models_FeatureTests/StateTest2.dsl', 10,'-quiet')), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 5*ones(1,11)]')));
s.add(Test('MultilineEquations (zero states)', @()(simex('models_FeatureTests/StateTest3.dsl', 10,'-quiet')), '-withouterror'))
s.add(Test('InitValueasConstant', @()(simex('models_FeatureTests/StateTest5.dsl', 10,[1],'-quiet')), '-equal', struct('x', [0:10; 1:11]')));

% We should eventually support initial values driven by states
if mode == INTERNAL
    s.add(Test('InitValueasInput', @()(simex('models_FeatureTests/StateTest4.dsl', 10,'-quiet')), '-equal', struct('x', [0:10; 0:10]')));
    s.add(Test('InitValueasInputWithValue', @()(simex('models_FeatureTests/StateTest4.dsl', 10,[1],'-quiet')), '-equal', struct('x', [0:10; 1:11]')));
    input_struct = struct('init', 2);
    s.add(Test('InitValueasInputthenInit', @()(simex('models_FeatureTests/StateTest6.dsl', 10,input_struct,[1],'-quiet')), '-equal', struct('x', [0:10; 1:11]')));
    s.add(Test('InitValueasInitthenInput', @()(simex('models_FeatureTests/StateTest6.dsl', 10,[1],input_struct,'-quiet')), '-equal', struct('x', [0:10; 2:12]')));
end

end

function s = InlineFunctionFeatureTests

s = Suite('Inline Function Feature Tests');

s.add(Test('ExternalFunction',@()(simex('models_FeatureTests/FunctionTest1.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InternalFunction',@()(simex('models_FeatureTests/FunctionTest2.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InlineEquationFunction',@()(simex('models_FeatureTests/FunctionTest3.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; (0:10).^2]')));

end

function s = ConstantFeatureTests

s = Suite('Constant Feature Tests');

s.add(Test('OneConstant',@()(simex('models_FeatureTests/ConstantTest1.dsl', 10,'-quiet')), '-equal', struct('y', [0:10; 0:10]')));
t = Test('TwoConstants',@()(simex('models_FeatureTests/ConstantTest2.dsl', 10,'-quiet')), '-withouterror');
t.ExpectFail = true; % there are two constants in this file, so it should produce an error
s.add(t);
t = Test('Constant+Intermediate',@()(simex('models_FeatureTests/ConstantTest3.dsl', 10,'-quiet')), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by an intermediate in this file, so it should produce an error
s.add(t);
t = Test('Constant+State',@()(simex('models_FeatureTests/ConstantTest4.dsl', 10,'-quiet')), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by a state in this file, so it should produce an error
s.add(t);

    function y = InternalConstants
        o = simex('models_FeatureTests/ConstantTest5.dsl', 1,'-quiet');
        y = approx_equiv(o.e_const(end,2), exp(1), 1e-5) && approx_equiv(o.pi_const(end,2), pi, 1e-5);
    end
s.add(Test('InternalConstants',@InternalConstants));

end

function s = IntermediateFeatureTests(mode)
INTERNAL = 0; RELEASE = 1;

s = Suite('Intermediate Feature Tests');
s.add(Test('Intermediate=State', ...
           @()(simex('models_FeatureTests/IntermediateTest1.dsl', 10,'-quiet')), ...
           '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:10]')));
s.add(Test('Intermediate=Input', ...
           @()(simex('models_FeatureTests/IntermediateTest2.dsl', 10,'-quiet')), ...
           '-equal', struct('s', [0:10; 0:10]', 'y', [[0 10]; ones(1,2)]')));
s.add(Test('InputToOutput', ...
           @()(simex('models_FeatureTests/IntermediateTest3.dsl', 10,'-quiet')), ...
           '-equal', struct('s', [0:10; 0:10]', 'x', [[0 10]; ones(1,2)]')));
s.add(Test('InputFcnOfTime', ...
           @()(simex(['models_FeatureTests/IntermediateTest5.dsl'], 10, '-quiet')), ...
           '-equal', struct('y', [0:10; [zeros(1,6) 1:5]]', 'I', [0:10; [zeros(1,5) ones(1,6)]]')));

% We want to add derivative suport soon
if mode == INTERNAL
  s.add(Test('Intermediate=Derivative', ...
             @()(simex('models_FeatureTests/IntermediateTest4.dsl', 10,'-quiet')), ...
             '-equal', struct('s', [0:10; 0:10]', 'y', [0:10; ones(1,11)]')));
end

end

function s = FunctionFeatureTests

s = Suite('Function Feature Tests');

s.add(Test('FunctionModulus', @()(simex(['models_FeatureTests/' ...
                    'FunctionTestModulus.dsl'], 10,'-quiet')), '-equal', ...
           struct('y', [0:10; 0 1 0 1 0 1 0 1 0 1 0]')))
    function y = FunctionTrig
        i.low = -0.99*pi;
        i.high = 0.99*pi;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        tol = 1e-3;
        y = approx_equiv(sin(o.y(:,2)),o.y(:,3),tol) && ...
            approx_equiv(cos(o.y(:,2)),o.y(:,4),tol) && ...
            approx_equiv(tan(o.y(:,2)),o.y(:,5),tol) && ...
            approx_equiv(csc(o.y(:,2)),o.y(:,6),tol) && ...
            approx_equiv(sec(o.y(:,2)),o.y(:,7),tol) && ...
            approx_equiv(cot(o.y(:,2)),o.y(:,8),tol);
    end
s.add(Test('FunctionTrig', @FunctionTrig));
    function y = FunctionInverseTrig
        i.low = -0.999999;
        i.high = 0.999999;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        tol = 1e-3;
        y = approx_equiv(asin(o.ay(:,2)),o.ay(:,3),tol) && ...
            approx_equiv(acos(o.ay(:,2)),o.ay(:,4),tol) && ...
            approx_equiv(atan(o.ay(:,2)),o.ay(:,5),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        y = y && approx_equiv(acsc(o.ay(:,2)),o.ay(:,6),tol) && ...
            approx_equiv(asec(o.ay(:,2)),o.ay(:,7),tol) && ...
            approx_equiv(acot(o.ay(:,2)),o.ay(:,8),tol);
    end
s.add(Test('FunctionInverseTrig', @FunctionInverseTrig));
    function y = FunctionHyperbolicTrig
        i.low = -pi;
        i.high = pi;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        tol = 1e-3;
        y = approx_equiv(sinh(o.yh(:,2)),o.yh(:,3),tol) && ...
            approx_equiv(cosh(o.yh(:,2)),o.yh(:,4),tol) && ...
            approx_equiv(tanh(o.yh(:,2)),o.yh(:,5),tol) && ...
            approx_equiv(csch(o.yh(:,2)),o.yh(:,6),tol) && ...
            approx_equiv(sech(o.yh(:,2)),o.yh(:,7),tol) && ...
            approx_equiv(coth(o.yh(:,2)),o.yh(:,8),tol);
    end
s.add(Test('FunctionHyperbolicTrig', @FunctionHyperbolicTrig));
    function y = FunctionInverseHyperbolicTrig
        i.low = -pi;
        i.high = pi;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        tol = 1e-3;
        y = approx_equiv(asinh(o.ayh(:,2)),o.ayh(:,3),tol);
        i.low = 1;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        y = y && approx_equiv(acosh(o.ayh(:,2)),o.ayh(:,4),tol);
        i.low = -0.999999;
        i.high = 0.999999;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        y = y && approx_equiv(atanh(o.ayh(:,2)),o.ayh(:,5),tol);
        i.low = 0.000001;
        i.high = 0.999999;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        y = y && approx_equiv(acsch(o.ayh(:,2)),o.ayh(:,6),tol) && ...
            approx_equiv(asech(o.ayh(:,2)),o.ayh(:,7),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i,'-quiet');
        y = y && approx_equiv(acoth(o.ayh(:,2)),o.ayh(:,8),tol);
    end
s.add(Test('FunctionInverseHyperbolicTrig', @FunctionInverseHyperbolicTrig));


end

function s = DifferenceEquationTests

s = Suite('Difference Equation Tests');

s.add(Test('Basic Difference Equation', ...
           @()(simex('models_FeatureTests/DifferenceEquationTest1.dsl', 10, '-quiet')), ...
           '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('Difference Equation larger fs', ...
           @()(simex('models_FeatureTests/DifferenceEquationTest2.dsl', 10, '-quiet')), ...
           '-equal', struct('x', [0:0.5:10; 0:0.5:10]')));
s.add(Test('Difference Equation State Delays', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest3.dsl', ...
                    10, '-quiet')), '-equal', struct('y', [0:10; ...
                    0:10; [0 0:9]; [0 0 0:8]; [0 0 0 0:7]]')));
s.add(Test('Difference Equation Output Delays', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest4.dsl', ...
                    10, '-quiet')), '-equal', struct('y', [0:10; ...
                    0:10; 0:10; [0 0:9]; [0 0 0:8]]')));
s.add(Test('Difference Equation Non-zero init', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest5.dsl', ...
                    10, '-quiet')), '-equal', struct('y', [0:10; ...
                    1:11; 1:11; [1 1:10]; [1 1 1:9]]')));
s.add(Test('Difference Equation Multi-step', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest6.dsl', ...
                    10, '-quiet')), '-equal', struct('y', [0:10; ...
                   [1 1 1:9]]')));

end
