% COREFEATURETESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = CoreFeatureTests - runs all tests
%  s = CoreFeatureTests('-cpu', '-release')
%  s = CoreFeatureTests('-release') - runs only those required for a release
%
function s = CoreFeatureTests(varargin)

if nargin>0
    target = varargin{1};
else
    target = '-cpu';
end

if nargin<2
   mode = 1;
else
   mode = varargin{2};
end

s = Suite(['Core Feature Tests ' target]);

% Add each of the language feature tests
s.add(OutputFeatureTests(target));
s.add(InputFeatureTests(target));
s.add(StateFeatureTests(mode, target));
s.add(InlineFunctionFeatureTests(target));
s.add(ConstantFeatureTests(target));
s.add(IntermediateFeatureTests(mode, target));
s.add(FunctionFeatureTests(target));
s.add(DifferenceEquationTests(target));

end


function s = OutputFeatureTests(target)

s = Suite(['Output Feature Tests ' target], {'outputs'});

s.add(Test('OutputStateDirectly',@()(simex('models_FeatureTests/OutputTest1.dsl', 10, target)), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputIntermediateDirectly',@()(simex('models_FeatureTests/OutputTest2.dsl', 10, target)), '-equal', struct('y', [0:10; 0:10]')));
s.add(Test('OutputGroups',@()(simex('models_FeatureTests/OutputTest3.dsl', 10, target)), '-equal', struct('y', [0:10; 0:10; -(0:10)]')));
s.add(Test('OutputCondition',@()(simex('models_FeatureTests/OutputTest4.dsl', 10, target)), '-equal', struct('y', [5:10; 5:10]')));
s.add(Test('OutputTwoValues',@()(simex('models_FeatureTests/OutputTest5.dsl', 10, target)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:2:20]')));
% This now produces a user error
s.add(Test('OutputNoValues',@()(simex('models_FeatureTests/OutputTest6.dsl', 10, target)), '-equal', struct()));
s.add(Test('DuplicateOutputNames',@()(simex('models_FeatureTests/OutputTest7.dsl')), '-regexpmatch', 'Duplicate output'));
s.add(Test('OutputTime', @()(simex('models_FeatureTests/OutputTest8.dsl', ...
                                   10, target)), '-equal', struct('times', [0 3 6 9; 0 3 6 9]')));
s.add(Test('OutputEvent', @()(simex('models_FeatureTests/OutputTest9.dsl', ...
                                    10, target)), '-equal', struct('times', [0 3 6 9]')));

end

function s = InputFeatureTests(target)

s = Suite(['Input Feature Tests ' target], {'inputs'});

t1 = Test('NoInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10, target)), '-withouterror');
t1.ExpectFail = true;
s.add(t1);
input = struct('x',5);
s.add(Test('PassedInputToState', @()(simex('models_FeatureTests/InputTest1.dsl', 10, input, target)), '-equal', struct('y', [0:10; 0:5:50]')));

    function y = VerifyDefaultInputs
        m = simex('models_FeatureTests/InputTest2.dsl',target);
        y = equiv(m.defaultInputs, struct('x',3));
    end

s.add(Test('VerifyInputDefaultValue', @VerifyDefaultInputs));
s.add(Test('InputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10, target)), '-equal', struct('y', [0:10; 0:3:30]')));
s.add(Test('OverrideInputDefaultValue', @()(simex('models_FeatureTests/InputTest2.dsl', 10, input, target)), '-equal', struct('y', [0:10; 0:5:50]')));

end

function s = StateFeatureTests(mode, target)
INTERNAL = 0; RELEASE = 1;

s = Suite(['State Feature Tests ' target], {'states'});

    function y = VerifyDefaultStateInits
        m = simex('models_FeatureTests/StateTest1.dsl', target);
        y = all(m.defaultStates == [1 0]);
    end

s.add(Test('VerifyDefaultStateInits', @VerifyDefaultStateInits))
s.add(Test('EvalDefaultStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10, target)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 0:2:20]')));
new_states = [0 1];
s.add(Test('ModifyStateInits', @()(simex('models_FeatureTests/StateTest1.dsl', 10, '-resume', new_states, target)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 1:2:21]')));

    function y = TestFinalStates
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10, target);
        y = equiv(finalStates, [11 20]);
    end
s.add(Test('TestFinalStates', @TestFinalStates));
    function y = TestFinalTime
        [o, finalStates, tf] = simex('models_FeatureTests/StateTest1.dsl', 10, target);
        y = equiv(tf, 10);
    end
s.add(Test('TestFinalTime', @TestFinalTime));
s.add(Test('StateWithoutEquation', @()(simex('models_FeatureTests/StateTest2.dsl', 10, target)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 5*ones(1,11)]')));
s.add(Test('MultilineEquations (zero states)', @()(simex('models_FeatureTests/StateTest3.dsl', 10, target)), '-withouterror'))
s.add(Test('InitValueasConstant', @()(simex('models_FeatureTests/StateTest5.dsl', 10, '-resume', [1], target)), '-equal', struct('x', [0:10; 1:11]')));

% We should eventually support initial values driven by states
s.add(Test('InitValueasInput', @()(simex('models_FeatureTests/StateTest4.dsl', 10, target)), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('InitValueasInputWithValue', @()(simex('models_FeatureTests/StateTest4.dsl', 10, '-resume', [1], target)), '-equal', struct('x', [0:10; 1:11]')));
input_struct = struct('init', 2);
s.add(Test('InitValueasInputthenInit', @()(simex('models_FeatureTests/StateTest6.dsl', 10,input_struct, '-resume', [2],target)), '-equal', struct('x', [0:10; 2:12]')));

% finally, we should support no states
s.add(Test('NoStates', @()(simex('models_FeatureTests/StateTest7.dsl', ...
                                 10, target)), '-equal', struct('x', ...
                                                  [0:10; 0:10]')))

si = Suite(['State Initial Values']);
s.add(si);
si.add(Test('TopInitialValueInput', ...
            @()(simex('models_FeatureTests/StateInit1.dsl', 10, struct('init',1))), ...
            '-equal', struct('x', [0:10; 1:11]')));
si.add(Test('SubmodelInitialValueInput', ...
            @()(simex('models_FeatureTests/StateInit2.dsl', 10)), ...
            '-equal', struct('x', [0:10; 2:12]', 'y', [0:10; 3:13]')));
si.add(Test('SubmodelInitialValueTopInput', ...
            @()(simex('models_FeatureTests/StateInit3.dsl', 10, struct('init',1))), ...
            '-equal', struct('x', [0:10; 2:-2:-18]', 'y', [0:10; 1:-1:-9]')));

end

function s = InlineFunctionFeatureTests(target)

s = Suite(['Inline Function Feature Tests ' target]);

s.add(Test('ExternalFunction',@()(simex('models_FeatureTests/FunctionTest1.dsl', 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InternalFunction',@()(simex('models_FeatureTests/FunctionTest2.dsl', 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));
s.add(Test('InlineEquationFunction',@()(simex('models_FeatureTests/FunctionTest3.dsl', 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));

end

function s = ConstantFeatureTests(target)

s = Suite(['Constant Feature Tests ' target], {'constants'});

s.add(Test('OneConstant',@()(simex('models_FeatureTests/ConstantTest1.dsl', 10, target)), '-equal', struct('y', [0:10; 0:10]')));
t = Test('TwoConstants',@()(simex('models_FeatureTests/ConstantTest2.dsl', 10, target)), '-withouterror');
t.ExpectFail = true; % there are two constants in this file, so it should produce an error
s.add(t);
t = Test('Constant+Intermediate',@()(simex('models_FeatureTests/ConstantTest3.dsl', 10, target)), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by an intermediate in this file, so it should produce an error
s.add(t);
t = Test('Constant+State',@()(simex('models_FeatureTests/ConstantTest4.dsl', 10, target)), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by a state in this file, so it should produce an error
s.add(t);

    function y = InternalConstants
        o = simex('models_FeatureTests/ConstantTest5.dsl', 1, target);
        y = approx_equiv(o.e_const(end,2), exp(1), 1e-5) && approx_equiv(o.pi_const(end,2), pi, 1e-5);
    end
% these cause errors in model translate so they don't really need to be
% fixed..
s.add(deprecate(Test('InternalConstants',@InternalConstants)));

s.add(deprecate(Test('ConstantToState',@()(simex('models_FeatureTests/ConstantTest6.dsl', 10, target)), '-equal', struct('two_const', [[0 10];[2 2]]'))));


end

function s = IntermediateFeatureTests(mode, target)
INTERNAL = 0; RELEASE = 1;

s = Suite(['Intermediate Feature Tests ' target]);
s.add(Test('Intermediate=State', ...
           @()(simex('models_FeatureTests/IntermediateTest1.dsl', 10, target)), ...
           '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:10]')));
s.add(Test('Intermediate=Input', ...
           @()(simex('models_FeatureTests/IntermediateTest2.dsl', 10, target)), ...
           '-equal', struct('s', [0:10; 0:10]', 'y', [[0 10]; ones(1,2)]')));
s.add(Test('InputToOutput', ...
           @()(simex('models_FeatureTests/IntermediateTest3.dsl', 10, target)), ...
           '-equal', struct('s', [0:10; 0:10]', 'x', [[0 10]; ones(1,2)]')));
s.add(Test('InputFcnOfTime', ...
           @()(simex(['models_FeatureTests/IntermediateTest5.dsl'], 10, target)), ...
           '-equal', struct('y', [0:10; [zeros(1,6) 1:5]]', 'I', [0:10; [zeros(1,5) ones(1,6)]]')));

% We want to add derivative suport soon
s.add(Test('Intermediate=Derivative', ...
    @()(simex('models_FeatureTests/IntermediateTest4.dsl', 10, target)), ...
    '-equal', struct('s', [0:10; 0:10]', 'y', [0:10; ones(1,11)]'), ...
    {'backlog'}));

end

function s = FunctionFeatureTests(target)

s = Suite(['Function Feature Tests ' target], {'functions'});

function y = MathFunction
  o = simex('models_FeatureTests/FunctionTestMathFunction.dsl', 100);
  tol = 1e-6;
  y = approx_equiv(1+(o.y(:,2)),o.z(:,2),tol) && ...
      approx_equiv(1+(o.x(:,2)),o.y(:,2),tol);
end
s.add(Test('MathFunction', @MathFunction));
    

s.add(Test('RelationalOperations', @()(simex(['models_FeatureTests/' ...
                    'FunctionTestRelational.dsl'],10, target)), ...
           '-equal', struct('y_eq', [5 5], 'y_ne', [[0:4 6:10]; [0:4 ...
                    6:10]]', 'y_gt', [6:10; 6:10]', 'y_lt', [0:4; 0:4]', ...
                            'y_ge', [5:10; 5:10]', 'y_le', [0:5; 0:5]')));

function y = RandomTest
o = simex('models_FeatureTests/RandomTest1.dsl',10);
outliers = [find(o.r1(:,2)<-10)' ...
            find(o.r1(:,2)>0)' ...
            find(o.r2(:,2)<0)' ...
            find(o.r2(:,2)>10)' ...
            find(o.r3(:,2)<-5)' ...
            find(o.r3(:,2)>5)'];
y = not(length(outliers) > 0);
end
s.add(Test('RandomOperations', @RandomTest));
% TODO validate the statistical distributions
s.add(Test('NormalDistribution', ...
           @()(simex(['models_FeatureTests/RandomTest2.dsl'], 2, target)), ...
           '-withouterror'));
s.add(Test('MixedDistribution', ...
           @()(simex(['models_FeatureTests/RandomTest3.dsl'], 10, target)), ...
           '-withouterror'));
s.add(Test('FunctionModulus', @()(simex(['models_FeatureTests/' ...
                    'FunctionTestModulus.dsl'], 10, target)), '-equal', ...
           struct('y', [0:10; 0 1 0 1 0 1 0 1 0 1 0]')));
    function y = FunctionTrig
        i.low = -0.99*pi;
        i.high = 0.99*pi;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
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
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        tol = 1e-3;
        y = approx_equiv(asin(o.ay(:,2)),o.ay(:,3),tol) && ...
            approx_equiv(acos(o.ay(:,2)),o.ay(:,4),tol) && ...
            approx_equiv(atan(o.ay(:,2)),o.ay(:,5),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        y = y && approx_equiv(acsc(o.ay(:,2)),o.ay(:,6),tol) && ...
            approx_equiv(asec(o.ay(:,2)),o.ay(:,7),tol) && ...
            approx_equiv(acot(o.ay(:,2)),o.ay(:,8),tol);
    end
s.add(Test('FunctionInverseTrig', @FunctionInverseTrig));
    function y = FunctionHyperbolicTrig
        i.low = -pi;
        i.high = pi;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
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
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        tol = 1e-3;
        y = approx_equiv(asinh(o.ayh(:,2)),o.ayh(:,3),tol);
        i.low = 1;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        y = y && approx_equiv(acosh(o.ayh(:,2)),o.ayh(:,4),tol);
        i.low = -0.999999;
        i.high = 0.999999;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        y = y && approx_equiv(atanh(o.ayh(:,2)),o.ayh(:,5),tol);
        i.low = 0.000001;
        i.high = 0.999999;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        y = y && approx_equiv(acsch(o.ayh(:,2)),o.ayh(:,6),tol) && ...
            approx_equiv(asech(o.ayh(:,2)),o.ayh(:,7),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex('models_FeatureTests/FunctionTestTrig.dsl', 100, i, target);
        y = y && approx_equiv(acoth(o.ayh(:,2)),o.ayh(:,8),tol);
    end
s.add(Test('FunctionInverseHyperbolicTrig', @FunctionInverseHyperbolicTrig));



end

function s = DifferenceEquationTests(target)

s = Suite(['Difference Equation Tests ' target], 'differenceequs');

s.add(Test('Basic Difference Equation', ...
           @()(simex('models_FeatureTests/DifferenceEquationTest1.dsl', 10, target)), ...
           '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('Difference Equation larger fs', ...
           @()(simex('models_FeatureTests/DifferenceEquationTest2.dsl', 10, target)), ...
           '-equal', struct('x', [0:0.5:10; 0:0.5:10]')));
s.add(Test('Difference Equation State Delays', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest3.dsl', ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    0:10; [0 0:9]; [0 0 0:8]; [0 0 0 0:7]]')));
s.add(Test('Difference Equation Output Delays', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest4.dsl', ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    0:10; 0:10; [0 0:9]; [0 0 0:8]]')));
s.add(Test('Difference Equation Non-zero init', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest5.dsl', ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    1:11; 1:11; [1 1:10]; [1 1 1:9]]')));
s.add(Test('Difference Equation Multi-step', @ ...
           ()(simex('models_FeatureTests/DifferenceEquationTest6.dsl', ...
                    10, target)), '-equal', struct('y', [0:10; ...
                   [1 1 1:9]]')));
s.add(Test('Difference Equation of n', ...
           @()(simex('models_FeatureTests/DifferenceEquationTest7.dsl', 10, target)), ...
           '-equal', struct('iter', [0:10; 0:10; [0 0:9]]')));

end
