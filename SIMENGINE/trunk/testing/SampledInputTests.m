function s = SampledInputTests(varargin)
%   mode can be either 0 for internal or 1 for release
INTERNAL = 0; RELEASE = 1;

if nargin > 0
  target = varargin{1};
else
  target = '-cpu';
end

if nargin == 2
    if strcmpi(varargin{2},'-release')
        mode = RELEASE;
    else
        error('Simatra:FeatureTests', 'Unexpected argument');
    end
end

s = Suite(['Sampled Input Tests ' target]);
s.add(SampledInputHoldTests(target));
s.add(SampledInputHaltTests(target));
s.add(SampledInputCycleTests(target));
end

function s = SampledInputHoldTests(target)
s = Suite(['Sampled Input Hold Tests ' target]);

input.i = {[0:5]};
s.add(Test('OneInputHoldValueLess', @()(simex('models_FeatureTests/SampledInputHoldTest1.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0:4]')));
s.add(Test('OneInputHoldValueEqual', @()(simex('models_FeatureTests/SampledInputHoldTest1.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0:5]')));
s.add(Test('OneInputHoldValueMore', @()(simex('models_FeatureTests/SampledInputHoldTest1.dsl', 10, input, target)), '-equal', struct('o', [0:10; 0:5 5 5 5 5 5]')));

s1 = Suite('Downsampling');
s1.add(Test('OneInputHoldValueLess', @()(simex('models_FeatureTests/SampledInputHoldTest2.dsl', 4, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputHoldValueEqual', @()(simex('models_FeatureTests/SampledInputHoldTest2.dsl', 5, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputHoldValueMore', @()(simex('models_FeatureTests/SampledInputHoldTest2.dsl', 10, input, target)), '-equal', struct('o', [0:2:10; 0:2:4 5 5 5]')));

s.add(s1);

s2 = Suite('Upsampling');
s2.add(Test('OneInputHoldValueLess', @()(simex('models_FeatureTests/SampledInputHoldTest3.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0 0 1 1 2]')));
s2.add(Test('OneInputHoldValueEqual', @()(simex('models_FeatureTests/SampledInputHoldTest3.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0 0 1 1 2 2]')));
s2.add(Test('OneInputHoldValueMore', @()(simex('models_FeatureTests/SampledInputHoldTest3.dsl', 10, input, target)), '-equal', struct('o', [0:10; 0 0 1 1 2 2 3 3 4 4 5]')));

s.add(s2);

end

function s = SampledInputHaltTests(target)
s = Suite(['Sampled Input Halt Tests ' target]);

input.i = {[0:5]};
s.add(Test('OneInputHaltValueLess', @()(simex('models_FeatureTests/SampledInputHaltTest1.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0:4]')));
s.add(Test('OneInputHaltValueEqual', @()(simex('models_FeatureTests/SampledInputHaltTest1.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0:5]')));
s.add(Test('OneInputHaltValueMore', @()(simex('models_FeatureTests/SampledInputHaltTest1.dsl', 10, input, target)), '-equal', struct('o', [0:5; 0:5]')));

t1 = Test('OneInputHaltValueNoInput',@()(simex('models_FeatureTests/SampledInputHaltTest1.dsl', 10, target)), '-withouterror');
t1.ExpectFail = true;
s.add(t1);

input.i = {[0:5 0/0]};
t2 = Test('OneInputHaltValueWithNAN',@()(simex('models_FeatureTests/SampledInputHaltTest1.dsl', 10, input, target)), '-withouterror');
t2.ExpectFail = true;

s.add(t2);

t3 = Test('OneInputHaltValueNoDefault',@()(simex('models_FeatureTests/SampledInputHaltTest4.dsl', 10, target)), '-withouterror');
t3.ExpectFail = true;
s.add(t3);

s1 = Suite('Downsampling');
s1.add(Test('OneInputHaltValueLess', @()(simex('models_FeatureTests/SampledInputHaltTest2.dsl', 4, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputHaltValueEqual', @()(simex('models_FeatureTests/SampledInputHaltTest2.dsl', 5, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputHaltValueMore', @()(simex('models_FeatureTests/SampledInputHaltTest2.dsl', 10, input, target)), '-equal', struct('o', [0:2:3; 0:2:4]')));

s.add(s1);

s2 = Suite('Upsampling');
s2.add(Test('OneInputHaltValueLess', @()(simex('models_FeatureTests/SampledInputHaltTest3.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0 0 1 1 2]')));
s2.add(Test('OneInputHaltValueEqual', @()(simex('models_FeatureTests/SampledInputHaltTest3.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0 0 1 1 2 2]')));
s2.add(Test('OneInputHaltValueMore', @()(simex('models_FeatureTests/SampledInputHaltTest3.dsl', 10, input, target)), '-equal', struct('o', [0:10; 0 0 1 1 2 2 3 3 4 4 5]')));

s.add(s2);

end

function s = SampledInputCycleTests(target)
s = Suite(['Sampled Input Cycle Tests ' target]);

input.i = {[0:5]};
s.add(Test('OneInputCycleValueLess', @()(simex('models_FeatureTests/SampledInputCycleTest1.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0:4]')));
s.add(Test('OneInputCycleValueEqual', @()(simex('models_FeatureTests/SampledInputCycleTest1.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0:5]')));
s.add(Test('OneInputCycleValueMore', @()(simex('models_FeatureTests/SampledInputCycleTest1.dsl', 15, input, target)), '-equal', struct('o', [0:15; 0:5 0:5 0:3]')));

s1 = Suite('Downsampling');
s1.add(Test('OneInputCycleValueLess', @()(simex('models_FeatureTests/SampledInputCycleTest2.dsl', 4, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputCycleValueEqual', @()(simex('models_FeatureTests/SampledInputCycleTest2.dsl', 5, input, target)), '-equal', struct('o', [0:2:4; 0:2:4]')));
s1.add(Test('OneInputCycleValueMore', @()(simex('models_FeatureTests/SampledInputCycleTest2.dsl', 10, input, target)), '-equal', struct('o', [0:2:10; 0:2:4 0:2:4]')));

s.add(s1);

s2 = Suite('Upsampling');
s2.add(Test('OneInputCycleValueLess', @()(simex('models_FeatureTests/SampledInputCycleTest3.dsl', 4, input, target)), '-equal', struct('o', [0:4; 0 0 1 1 2]')));
s2.add(Test('OneInputCycleValueEqual', @()(simex('models_FeatureTests/SampledInputCycleTest3.dsl', 5, input, target)), '-equal', struct('o', [0:5; 0 0 1 1 2 2]')));
s2.add(Test('OneInputCycleValueMore', @()(simex('models_FeatureTests/SampledInputCycleTest3.dsl', 10, input, target)), '-equal', struct('o', [0:10; 0 0 1 1 2 2 3 3 4 4 5]')));

s.add(s2);


end
