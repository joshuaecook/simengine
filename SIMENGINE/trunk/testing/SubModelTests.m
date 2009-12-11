function s = SubModelTests(varargin)
%SUBMODELTESTS Tests the basic features of submodels
%   Detailed explanation goes here
%
% Usage:
%  s = SubModelTests - runs all tests
%  s = SubModelTests('-release') - runs only those required for a release
%
INTERNAL = 0; RELEASE = 1;

target = varargin{1};

if nargin == 1
    mode = 1;
else
    mode = varargin{2};
end

s = Suite(['Sub-model Feature Tests ' target]);
s.add(BasicSubModelTests(mode, target));
s.add(AlgebraicSubModelTests(target));
s.add(OrderingTests(target));

end

function s = BasicSubModelTests(mode, target)

s = Suite(['Basic Sub-Model Tests ' target]);

s.add(Test('BasicSubModelTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest1.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
s.add(Test('SubModelInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest2.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
s.add(Test('DuplicateSubModelInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest3.dsl'], 10,target)), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:10; 0:3:30]')));
s.add(Test('DuplicateSubModelToOutputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest4.dsl'], 10,target)), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:10; 0:3:30]')));
s.add(Test('SubModelDefaultInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest5.dsl'], 10,target)), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:2:20; 0:10]')));
s.add(Test('DeepHierarchy', ...
            @()(simex(['models_FeatureTests/SubModelTest7.dsl'], 10,target)), ...
            '-equal', struct('y', [0:10; 0:3:30; 0:8:80; 0:10]')));

INTERNAL = 0; RELEASE = 1;

% this is related to init values driven by inputs
if mode == INTERNAL
    s.add(Test('SubModelInputToInitTest', @()(simex(['models_FeatureTests/' ...
        'SubModelTest6.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
end

end

function s = AlgebraicSubModelTests(target)
s = Suite(['Algebraic Sub-Model Tests ' target]);

s.add(Test('BasicAlgebraicSubModelTest', ...
            @()(simex(['models_FeatureTests/AlgebraicSubModelTest1.dsl'], 10, target)), ...
            '-equal', struct('y', [0:10; 0:10; (0:10).^2]')));
s.add(Test('MultipleIterators', ...
            @()(simex(['models_FeatureTests/AlgebraicSubModelTest2.dsl'], 10, target)), ...
            '-equal', struct('y', [0:10; 0:10; (0:10).^2.*3; (0:10).^2.*3]')));
s.add(Test('IteratorOfInput', ...
            @()(simex(['models_FeatureTests/AlgebraicSubModelTest4.dsl'], 10, target)), ...
            '-equal', struct('y', [0:10; (0:10).^2]', ...
                             'z', [0:10; 0:10]')));
end

function s = OrderingTests(target)

s = Suite(['Ordering Tests ' target]);

s.add(Test('CascadedModelTest', ...
            @()(simex(['models_FeatureTests/OrderingTest1.dsl'], 10,target)), ...
            '-equal', struct('y', [0:10; 0:10;cumsum([0 0:9])]')));
s.add(Test('FlippedCascadedModelTest', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest2.dsl'], 10,target)), '-equal', ...
            struct('y', [0:10; cumsum([0 0:9]); 0:10]')));
s.add(Test('InterconnectedModelTest1', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest3.dsl'], 10,target)), '-equal', ...
            struct('y', [0:10; zeros(1,11); zeros(1,11)]')));
s.add(Test('InterconnectedModelTest2', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest4.dsl'], 10,target)), '-equal', ...
            struct('y', [0:10; 2.^(0:10); 2.^(0:10)]')));

end

