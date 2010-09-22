% COREFEATURETESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = CoreFeatureTests - runs all tests
%  s = CoreFeatureTests('-cpu', '-release')
%  s = CoreFeatureTests('-release') - runs only those required for a release
%
function s = ML_CoreFeatureTests(varargin)

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

s = Suite(['MATLAB DIESEL Core Feature Tests ' target]);

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

s = Suite(['Output Feature Tests ' target]);

% Define a common iterator
t = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);


% Output a state
    function m = OutputTest1
        m = Model('OutputTest1', t);
        y = m.state(0);
        m.diffequ(y, 1);
        m.output('y',y);
    end
s.add(Test('OutputStateDirectly',@()(simex(OutputTest1, 10, target)), '-equal', struct('y', [0:10; 0:10]')));

% Output an intermediate
    function m = OutputTest2
        m = Model('OutputTest2', t);
        x = m.state(0);
        m.diffequ(x, 1);
        m.equ('y', x);
        m.output('y');
    end
s.add(Test('OutputIntermediateDirectly',@()(simex(OutputTest2, 10, target)), '-equal', struct('y', [0:10; 0:10]')));

% Output a group of values
    function m = OutputTest3
        m = Model('OutputTest3', t);
        x = m.state(0);
        m.diffequ(x, 1);
        neg_x = m.equ('neg_x', -x);
        m.output('y', x, neg_x);
    end
s.add(Test('OutputGroups',@()(simex(OutputTest3, 10, target)), '-equal', struct('y', [0:10; 0:10; -(0:10)]')));

% Output with a condition
    function m = OutputTest4
        m = Model('OutputTest4', t);
        y = m.state(0);
        m.diffequ(y, 1);
        m.output('y',y,'when',y>=5);
    end
s.add(Test('OutputCondition',@()(simex(OutputTest4, 10, target)), '-equal', struct('y', [5:10; 5:10]')));

% Output two values
    function m = OutputTest5
        m = Model('OutputTest5', t);
        x = m.state('x',0);
        y = m.state('y',0);
        m.diffequ(x, 1);
        m.diffequ(y, 2);
        m.output('x');
        m.output('y');
    end
s.add(Test('OutputTwoValues',@()(simex(OutputTest5, 10, target)), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0:2:20]')));

% Output no values
    function m = OutputTest6
        m = Model('OutputTest6', t);
        y = m.state(0);
        m.diffequ(y, 1);
    end
s.add(Test('OutputNoValues',@()(simex(OutputTest6, 10, target)), '-equal', struct()));

% Output duplicate values
    function m = OutputTest7
        m = Model('OutputTest7', t);
        x = m.state('x',0);
        y = m.state('y',0);
        m.diffequ(x, cos(Exp(t)));
        m.diffequ(y, -x);
        m.output('x');
        try
            m.output('x');
        catch me
            disp(me.message);
        end
    end
s.add(Test('DuplicateOutputNames',@()(OutputTest7), '-regexpmatch', 'already exists'));

% Output time
    function m = OutputTest8
        m = Model('OutputTest8', t);
        x = m.state(0);
        m.diffequ(x, 1);
        m.output('times', t, 'when', mod(x,3)==0);
    end
s.add(Test('OutputTime', @()(simex(OutputTest8, 10, target)), '-equal', struct('times', [0 3 6 9; 0 3 6 9]')));

% Output event
    function m = OutputTest9
        m = Model('OutputTest9', t);
        x = m.state(0);
        m.diffequ(x, 1);
        m.output(t, 'when', mod(x,3)==0);
    end
s.add(Test('OutputEvent', @()(simex(OutputTest9, 10, target)), '-equal', struct('t', [0 3 6 9]')));

end

function s = InputFeatureTests(target)

s = Suite(['Input Feature Tests ' target]);

% Define a common iterator
t = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);

    function m = InputTest1
        m = Model('InputTest1', t);
        x = m.input('x');
        y = m.state(0);
        m.diffequ(y, x);
        m.output('y', y);
    end

t1 = Test('NoInputToState', @()(simex(InputTest1, 10, target)), '-withouterror');
t1.ExpectFail = true;
s.add(t1);

input = struct('x',5);
s.add(Test('PassedInputToState', @()(simex(InputTest1, 10, input, target)), '-equal', struct('y', [0:10; 0:5:50]')));

    function m = InputTest2
        m = Model('InputTest2', t);
        x = m.input('x', 3);
        y = m.state(0);
        m.diffequ(y, x);
        m.output('y', y);
    end

    function y = VerifyDefaultInputs
        m = simex(InputTest2,target);
        y = equiv(m.defaultInputs, struct('x',3));
    end

s.add(Test('VerifyInputDefaultValue', @VerifyDefaultInputs));


s.add(Test('InputDefaultValue', @()(simex(InputTest2, 10, target)), '-equal', struct('y', [0:10; 0:3:30]')));
s.add(Test('OverrideInputDefaultValue', @()(simex(InputTest2, 10, input, target)), '-equal', struct('y', [0:10; 0:5:50]')));

end

function s = StateFeatureTests(mode, target)
INTERNAL = 0; RELEASE = 1;

s = Suite(['State Feature Tests ' target]);

% Define a common iterator
t = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);


    function m = StateTest1
        m = Model('StateTest1',t);
        x = m.state('x',1);
        y = m.state('y',0);
        m.diffequ(x,1);
        m.diffequ(y,2);
        m.output('x');
        m.output('y');
    end

x_index = 0;
y_index = 0;
    function y = VerifyDefaultStateInits
        m = simex(StateTest1, target);
        x_index = find(strcmp(m.states,'x'));
        y_index = find(strcmp(m.states,'y'));
        state_inits = [m.defaultStates(x_index) m.defaultStates(y_index)];
        y = all(state_inits == [1 0]);
    end

s.add(Test('VerifyDefaultStateInits', @VerifyDefaultStateInits))
s.add(Test('EvalDefaultStateInits', @()(simex(StateTest1, 10, target)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 0:2:20]')));
    function o = ModifyStateInits
        new_states = [-1 -1];
        new_states(x_index) = 0;
        new_states(y_index) = 1;
        o = simex(StateTest1, 10, '-resume', new_states, target);
    end
s.add(Test('ModifyStateInits', @()(ModifyStateInits), '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 1:2:21]')));

    function y = TestFinalStates
        [o, finalStates, tf] = simex(StateTest1, 10, target);
        final_state_values = [finalStates(x_index) finalStates(y_index)];
        y = length(finalStates) == 2 && equiv(final_state_values, [11 20]);
    end
s.add(Test('TestFinalStates', @TestFinalStates));
    function y = TestFinalTime
        [o, finalStates, tf] = simex(StateTest1, 10, target);
        y = equiv(tf, 10);
    end
s.add(Test('TestFinalTime', @TestFinalTime));

    function m = StateTest2
        m = Model('StateTest2',t);
        x = m.state(1);
        y = m.state(5);
        m.diffequ(x, 1);
        m.output('x',x);
        m.output('y',y);
    end
s.add(Test('StateWithoutEquation', @()(simex(StateTest2, 10, target)), '-equal', struct('x', [0:10; 1:11]', 'y', [0:10; 5*ones(1,11)]')));

    function m = StateTest3
        m = Model('StateTest3',t);
        A = Exp(1);
        B = Exp(2);
        C = Exp(3);
        sum = m.equ(A+B+C);
        diff = m.equ(A-B-C);
        m.output(sum);
        m.output(diff);
    end
s.add(Test('MultilineEquations (zero states)', @()(simex(StateTest3, 10, target)), '-withouterror'))

    function m = StateTest4
        m = Model('StateTest4',t);
        init = m.input('init', 0);
        x = m.state(init);
        m.diffequ(x, 1);
        m.output(x);
    end

    function m = StateTest5
        m = Model('StateTest5',t);
        init = m.equ(1);
        x = m.state(init);
        m.diffequ(x, 1);
        m.output(x);
    end

s.add(Test('InitValueasConstant', @()(simex(StateTest5, 10, '-resume', [1], target)), '-equal', struct('x', [0:10; 1:11]')));

% We should eventually support initial values driven by states
s.add(Test('InitValueasInput', @()(simex(StateTest4, 10, target)), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('InitValueasInputWithValue', @()(simex(StateTest4, 10, '-resume', [1], target)), '-equal', struct('x', [0:10; 1:11]')));

    function m = StateTest6
        m = Model('StateTest6',t);
        init = m.input('init');
        x = m.state(init);
        m.diffequ(x,1);
        m.output(x);
    end
input_struct = struct('init', 2);
s.add(Test('InitValueasInputthenInit', @()(simex(StateTest6, 10,input_struct, '-resume', [2],target)), '-equal', struct('x', [0:10; 2:12]')));

% finally, we should support no states
    function m = StateTest7
        m = Model('StateTest7',t);
        x = m.equ(t);
        m.output(x);
    end
s.add(Test('NoStates', @()(simex(StateTest7, ...
                                 10, target)), '-equal', struct('x', ...
                                                  [0:10; 0:10]')))

si = Suite(['State Initial Values']);
s.add(si);
    function m = StateInit1
        m = Model('StateInit1',t);
        init = m.input('init');
        x = m.state(init);
        m.diffequ(x, 1);
        m.output(x);
    end
si.add(Test('TopInitialValueInput', ...
            @()(simex(StateInit1, 10, struct('init',1))), ...
            '-equal', struct('x', [0:10; 1:11]')));
    function m = StateInit2
        m_subby = Model('subby',t);
        init = m_subby.input('init');
        a = m_subby.state(init);
        m_subby.diffequ(a, 1);
        m_subby.output(a);
        
        m = Model('StateInit2',t);
        s1 = m.submodel(m_subby);
        s1.init = 2;
        s2 = m.submodel(m_subby);
        s2.init = 3;
        
        m.output('x', s1.a);
        m.output('y', s2.a);
    end
si.add(Test('SubmodelInitialValueInput', ...
            @()(simex(StateInit2, 10)), ...
            '-equal', struct('x', [0:10; 2:12]', 'y', [0:10; 3:13]')));
        
    function m = StateInit3
        m_subby = Model('subby',t);
        init = m_subby.input('init');
        a = m_subby.state(init);
        m_subby.diffequ(a, -init);
        m_subby.output(a);
        
        m = Model('StateInit2',t);
        init = m.input('init');
        s1 = m.submodel(m_subby);
        s1.init = 2;
        s2 = m.submodel(m_subby);
        s2.init = init;
        
        m.output('x', s1.a);
        m.output('y', s2.a);
    end

si.add(Test('SubmodelInitialValueTopInput', ...
            @()(simex(StateInit3, 10, struct('init',1))), ...
            '-equal', struct('x', [0:10; 2:-2:-18]', 'y', [0:10; 1:-1:-9]')));

end

function s = InlineFunctionFeatureTests(target)

s = Suite(['Inline Function Feature Tests ' target]);

% Define a common iterator
t = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);

    function m = FunctionTest1
        function y = sqr(x)
            y = x^2;
        end
        m = Model('FunctionTest1',t);
        x = m.state(0);
        m.diffequ(x, 1);
        y = m.equ(sqr(x));
        m.output(y);
    end
s.add(Test('ExternalFunction',@()(simex(FunctionTest1, 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));

% Duplicate test to above
%s.add(Test('InternalFunction',@()(simex('models_FeatureTests/FunctionTest2.dsl', 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));

% No syntax available now
%s.add(Test('InlineEquationFunction',@()(simex('models_FeatureTests/FunctionTest3.dsl', 10,target)), '-equal', struct('y', [0:10; (0:10).^2]')));

end

function s = ConstantFeatureTests(target)

s = Suite(['Constant Feature Tests ' target]);

% Define a common iterator
time = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);

    function m = ConstantTest1
        m = Model('ConstantTest1',time);
        c = m.equ('c',1);
        pi_exp = m.equ('pi', pi);
        y = m.state(0);
        m.diffequ(y,c);
        m.output(y);
    end

s.add(Test('OneConstant',@()(simex(ConstantTest1, 10, target)), '-equal', struct('y', [0:10; 0:10]')));

    function m = ConstantTest2
        m = Model('ConstantTest2',time);
        c = m.equ('c',1);
        c = m.equ('c',2);
        y = m.state(0);
        m.diffequ(y,c);
        m.output(y);
    end

t = Test('TwoConstants',@()(simex(ConstantTest2, 10, target)), '-withouterror');
t.ExpectFail = true; % there are two constants in this file, so it should produce an error
s.add(t);

% duplicate to above since we can't distinguish between equations and
% constants
%t = Test('Constant+Intermediate',@()(simex('models_FeatureTests/ConstantTest3.dsl', 10, target)), '-withouterror');
%t.ExpectFail = true; % there is one constant overwritten by an intermediate in this file, so it should produce an error
%s.add(t);

    function m = ConstantTest4
        m = Model('ConstantTest4',time);
        c = m.equ('c',1);
        c = m.state('c',2);
        y = m.state(0);
        m.diffequ(y,c);
        m.output(y);
    end

t = Test('Constant+State',@()(simex(ConstantTest4, 10, target)), '-withouterror');
t.ExpectFail = true; % there is one constant overwritten by a state in this file, so it should produce an error
s.add(t);


    function m = ConstantTest5
        m = Model('ConstantTest5',time);
        e_const = m.state(Exp('e'));
        pi_const = m.state(Exp('pi'));
        m.output(e_const);
        m.output(pi_const);
    end

    function y = InternalConstants
        o = simex(ConstantTest5, 1, target);
        y = approx_equiv(o.e_const(end,2), exp(1), 1e-5) && approx_equiv(o.pi_const(end,2), pi, 1e-5);
    end
% This just doesn't work since we don't have constants defined in DSL
% anymore...
%s.add(Test('InternalConstants',@InternalConstants));

end

function s = IntermediateFeatureTests(mode, target)
INTERNAL = 0; RELEASE = 1;

s = Suite(['Intermediate Feature Tests ' target]);

function m = IntermediateTest1
m = Model('IntermediateTest1');
m.solver='forwardeuler'; m.dt = 1;
x = m.state(0);
m.diffequ(x, 1);
m.output(x);
m.output('y', x);
end

s.add(Test('Intermediate=State', ...
           @()(simex(IntermediateTest1, 10, target)), ...
           '-equal', struct('x', [0:10; 0:10]', 'y', [0:10; 0: ...
                    10]')));

function m = IntermediateTest2
m = Model('IntermediateTest2');
m.solver='forwardeuler'; m.dt = 1;
x = m.input('x', 1);
s = m.state(0);
m.diffequ(s, 1);
m.output(s);
m.output('y', x);
end

s.add(Test('Intermediate=Input', ...
           @()(simex(IntermediateTest2, 10, target)), ...
           '-equal', struct('s', [0:10; 0:10]', 'y', [[0 10]; ...
                    ones(1,2)]')));

function m = IntermediateTest3
m = Model('IntermediateTest3');
m.solver='forwardeuler'; m.dt = 1;
x = m.input('x', 1);
s = m.state(0);
m.diffequ(s, 1);
m.output(s);
m.output(x);
end

s.add(Test('InputToOutput', ...
           @()(simex(IntermediateTest3, 10, target)), ...
           '-equal', struct('s', [0:10; 0:10]', 'x', [[0 10]; ...
                    ones(1,2)]')));


function m = IntermediateTest5
m = Model('IntermediateTest5');
m.solver='forwardeuler'; m.dt = 1;
y = m.state(0);
I = m.equ(piecewise(0, m.time < 5, 1));
m.diffequ(y, I);
m.output(y);
m.output(I);
end


s.add(Test('InputFcnOfTime', ...
           @()(simex(IntermediateTest5, 10, target)), ...
           '-equal', struct('y', [0:10; [zeros(1,6) 1:5]]', 'I', [0:10; [zeros(1,5) ones(1,6)]]')));

% We want to add derivative suport soon
% if mode == INTERNAL
%   s.add(Test('Intermediate=Derivative', ...
%              @()(simex('models_FeatureTests/IntermediateTest4.dsl', 10, target)), ...
%              '-equal', struct('s', [0:10; 0:10]', 'y', [0:10; ones(1,11)]')));
% end

end

function s = FunctionFeatureTests(target)

s = Suite(['Function Feature Tests ' target]);

function m = FunctionTestMathFunction
m = Model('FunctionTestMathFunction');
m.solver = 'forwardeuler'; m.dt = 1;
x = m.state(0);
m.diffequ(x, 0.01);
f = @(x)(x+1);
y = f(x);
z = f(y);
m.output(x);
m.output(y);
m.output(z);
end

function y = MathFunction
  o = simex(FunctionTestMathFunction, 100);
  tol = 1e-6;
  y = approx_equiv(1+(o.y(:,2)),o.z(:,2),tol) && ...
      approx_equiv(1+(o.x(:,2)),o.y(:,2),tol)
end
s.add(Test('MathFunction', @MathFunction));
    
function m = FunctionTestRelational
m = Model('FunctionTestRelational');
m.solver = 'forwardeuler'; m.dt = 1;
x = m.state(0);
m.diffequ(x, 1);
threshold = 5;
m.output('y_eq', x, 'when', x==5);
m.output('y_ne', x, 'when', x~=5);
m.output('y_gt', x, 'when', x>5);
m.output('y_lt', x, 'when', x<5);
m.output('y_ge', x, 'when', x>=5);
m.output('y_le', x, 'when', x<=5);
end

s.add(Test('RelationalOperations', @()(simex(FunctionTestRelational,10, target)), ...
           '-equal', struct('y_eq', [5 5], 'y_ne', [[0:4 6:10]; [0:4 ...
                    6:10]]', 'y_gt', [6:10; 6:10]', 'y_lt', [0:4; 0:4]', ...
                            'y_ge', [5:10; 5:10]', 'y_le', [0:5; 0:5]')));

function m = RandomTest1
m = Model('RandomTest1');
m.solver='forwardeuler'; m.dt=1;
x = m.state(0);
m.diffequ(x, 1);
r1 = m.random('uniform', 'low', -10, 'high', 0);
r2 = m.random('uniform', 'low', 0, 'high', 10);
r3 = m.random('uniform', 'low', -5, 'high', 5);
m.output(x);
m.output(r1);
m.output(r2);
m.output(r3);
end


function y = RandomTest
o = simex(RandomTest1,10);
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

function m = RandomTest2
m = Model('RandomTest2');
m.solver='forwardeuler'; m.dt=1;
x = m.state(0);
m.diffequ(x, 1);
r1 = m.random('normal', 'mean', 0, 'stddev', 1);
r2 = m.random('normal', 'mean', 5, 'stddev', 10);
r3 = m.random('normal', 'mean', -5, 'stddev', 5);
m.output(x);
m.output(r1);
m.output(r2);
m.output(r3);
end

s.add(Test('NormalDistribution', ...
           @()(simex(RandomTest2, 2, target)), ...
           '-withouterror'));

function m = RandomTest3
m = Model('RandomTest3');
m.solver='forwardeuler'; m.dt=1;
x = m.state(0);
m.diffequ(x, 1);
r1 = m.random('normal', 'mean', 0, 'stddev', 1);
r2 = m.random('uniform', 'low', -10, 'high', 0);
m.output(x);
m.output(r1);
m.output(r2);
end

s.add(Test('MixedDistribution', ...
           @()(simex(RandomTest3, 10, target)), ...
           '-withouterror'));

function m = FunctionTestModulus
m = Model('FunctionTestModulus');
m.solver = 'forwardeuler'; m.dt = 1;
x = m.state(0);
m.diffequ(x, 1);
y = mod(x,2);
m.output(y);
end

s.add(Test('FunctionModulus', @()(simex(FunctionTestModulus, 10, target)), '-equal', ...
           struct('y', [0:10; 0 1 0 1 0 1 0 1 0 1 0]')));

function m = FunctionTestTrig
m = Model('FunctionTestTrig');
m.solver = 'forwardeuler'; m.dt = 1;
x = m.state(0);
m.diffequ(x, 0.01);
low = m.input('low', 0);
high = m.input('high', pi);
sx = x*(high-low)+low;
    y1 = sin(sx);
    y2 = cos(sx);
    y3 = tan(sx);
    y4 = csc(sx);
    y5 = sec(sx);
    y6 = cot(sx);

    ay1 = asin(sx);
    ay2 = acos(sx);
    ay3 = atan(sx);
    ay3b = atan2(sx, sx^2);
    ay4 = acsc(sx);
    ay5 = asec(sx);
    ay6 = acot(sx);

    yh1 = sinh(sx);
    yh2 = cosh(sx);
    yh3 = tanh(sx);
    yh4 = csch(sx);
    yh5 = sech(sx);
    yh6 = coth(sx);

    ayh1 = asinh(sx);
    ayh2 = acosh(sx);
    ayh3 = atanh(sx);
    ayh4 = acsch(sx);
    ayh5 = asech(sx);
    ayh6 = acoth(sx);
    

    m.output('y', sx, y1, y2, y3, y4, y5, y6);
    m.output('ay', sx, ay1, ay2, ay3, ay4, ay5, ay6, ay3b);
    m.output('yh', sx, yh1, yh2, yh3, yh4, yh5, yh6);
    m.output('ayh', sx, ayh1, ayh2, ayh3, ayh4, ayh5, ayh6);
end

function y = FunctionTrig
        i.low = -0.99*pi;
        i.high = 0.99*pi;
        o = simex(FunctionTestTrig, 100, i, target);
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
        o = simex(FunctionTestTrig, 100, i, target);
        tol = 1e-3;
        y = approx_equiv(asin(o.ay(:,2)),o.ay(:,3),tol) && ...
            approx_equiv(acos(o.ay(:,2)),o.ay(:,4),tol) && ...
            approx_equiv(atan(o.ay(:,2)),o.ay(:,5),tol) && ...
            approx_equiv(atan2(o.ay(:,2),o.ay(:,2).^2),o.ay(:,9),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex(FunctionTestTrig, 100, i, target);
        y = y && approx_equiv(acsc(o.ay(:,2)),o.ay(:,6),tol) && ...
            approx_equiv(asec(o.ay(:,2)),o.ay(:,7),tol) && ...
            approx_equiv(acot(o.ay(:,2)),o.ay(:,8),tol);
    end
s.add(Test('FunctionInverseTrig', @FunctionInverseTrig));
    function y = FunctionHyperbolicTrig
        i.low = -pi;
        i.high = pi;
        o = simex(FunctionTestTrig, 100, i, target);
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
        o = simex(FunctionTestTrig, 100, i, target);
        tol = 1e-3;
        y = approx_equiv(asinh(o.ayh(:,2)),o.ayh(:,3),tol);
        i.low = 1;
        i.high = 2;
        o = simex(FunctionTestTrig, 100, i, target);
        y = y && approx_equiv(acosh(o.ayh(:,2)),o.ayh(:,4),tol);
        i.low = -0.999999;
        i.high = 0.999999;
        o = simex(FunctionTestTrig, 100, i, target);
        y = y && approx_equiv(atanh(o.ayh(:,2)),o.ayh(:,5),tol);
        i.low = 0.000001;
        i.high = 0.999999;
        o = simex(FunctionTestTrig, 100, i, target);
        y = y && approx_equiv(acsch(o.ayh(:,2)),o.ayh(:,6),tol) && ...
            approx_equiv(asech(o.ayh(:,2)),o.ayh(:,7),tol);
        i.low = 1.0001;
        i.high = 2;
        o = simex(FunctionTestTrig, 100, i, target);
        y = y && approx_equiv(acoth(o.ayh(:,2)),o.ayh(:,8),tol);
    end
s.add(Test('FunctionInverseHyperbolicTrig', @FunctionInverseHyperbolicTrig));



end

function s = DifferenceEquationTests(target)

s = Suite(['Difference Equation Tests ' target]);

function m = DifferenceEquationTest1
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest1', n);
x = m.state(0);
m.recurrenceequ(x, x+1);
m.output(x);
end

s.add(Test('Basic Difference Equation', ...
           @()(simex(DifferenceEquationTest1, 10, target)), ...
           '-equal', struct('x', [0:10; 0:10]')));

function m = DifferenceEquationTest2
n = Iterator('discrete', 'sample_frequency', 2);
m = Model('DifferenceEquationTest2', n);
x = m.state(0);
m.recurrenceequ(x, x+0.5);
m.output(x);
end

s.add(Test('Difference Equation larger fs', ...
           @()(simex(DifferenceEquationTest2, 10, target)), ...
           '-equal', struct('x', [0:0.5:10; 0:0.5:10]')));

function m = DifferenceEquationTest3
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest3', n);
x = m.state(0);
x_delay1 = m.state(0);
x_delay2 = m.state(0);
x_delay3 = m.state(0);
m.recurrenceequ(x, x+1);
m.recurrenceequ(x_delay1, x);
m.recurrenceequ(x_delay2, x(n-1));
m.recurrenceequ(x_delay3, x(n-2));
m.output('y', x, x_delay1, x_delay2, x_delay3);
end

s.add(Test('Difference Equation State Delays', @ ...
           ()(simex(DifferenceEquationTest3, ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    0:10; [0 0:9]; [0 0 0:8]; [0 0 0 0:7]]')));

function m = DifferenceEquationTest4
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest4', n);
x = m.state(0);
m.recurrenceequ(x, x+1);
m.output('y', x, x(n), x(n-1), x(n-2));
end

s.add(Test('Difference Equation Output Delays', @ ...
           ()(simex(DifferenceEquationTest4, ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    0:10; 0:10; [0 0:9]; [0 0 0:8]]')));

function m = DifferenceEquationTest5
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest5', n);
x = m.state(1);
m.recurrenceequ(x, x+1);
m.output('y', x, x(n), x(n-1), x(n-2));
end

s.add(Test('Difference Equation Non-zero init', @ ...
           ()(simex(DifferenceEquationTest5, ...
                    10, target)), '-equal', struct('y', [0:10; ...
                    1:11; 1:11; [1 1:10]; [1 1 1:9]]')));

function m = DifferenceEquationTest6
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest6', n);
x = m.state(1);
m.recurrenceequ(x, x+1);
m.output('y', x(n-2));
end

s.add(Test('Difference Equation Multi-step', @ ...
           ()(simex(DifferenceEquationTest6, ...
                    10, target)), '-equal', struct('y', [0:10; ...
                   [1 1 1:9]]')));

function m = DifferenceEquationTest7
n = Iterator('discrete', 'sample_period', 1);
m = Model('DifferenceEquationTest7', n);
x = m.state(0);
m.recurrenceequ(x, x+1);
m.output('iter', n, x(n-1));
end

s.add(Test('Difference Equation of n', ...
           @()(simex(DifferenceEquationTest7, 10, target)), ...
           '-equal', struct('iter', [0:10; 0:10; [0 0:9]]')));

end
