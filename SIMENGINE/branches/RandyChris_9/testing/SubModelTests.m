function s = SubModelTests(varargin)
%SUBMODELTESTS Tests the basic features of submodels
%   Detailed explanation goes here
%
% Usage:
%  s = SubModelTests - runs all tests
%  s = SubModelTests('-release') - runs only those required for a release
%
if nargin == 0
    mode = 1;
else
    mode = varargin{1};
end

s = Suite('Sub-model Feature Tests');

s1 = Suite('Basic Sub-Model Tests');
s.add(s1);

s1.add(Test('BasicSubModelTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest1.dsl'], 10,'-quiet')), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
s1.add(Test('SubModelInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest2.dsl'], 10,'-quiet')), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
s1.add(Test('DuplicateSubModelInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest3.dsl'], 10,'-quiet')), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:10; 0:3:30]')));
s1.add(Test('DuplicateSubModelToOutputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest4.dsl'], 10,'-quiet')), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:10; 0:3:30]')));
s1.add(Test('SubModelDefaultInputTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest5.dsl'], 10,'-quiet')), '-equal', ...
           struct('y', [0:10; 0:2:20; 0:2:20; 0:10]')));
s1.add(Test('SubModelInputToInitTest', @()(simex(['models_FeatureTests/' ...
                    'SubModelTest6.dsl'], 10,'-quiet')), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));

s2 = Suite('Algebraic Sub-Model Tests');
s.add(s2);

s2.add(Test('BasicAlgebraicSubModelTest', @()(simex(['models_FeatureTests/' ...
                    'AlgebraicSubModelTest1.dsl'], 10,'-quiet')), '-equal', struct('y', [0:10; 0:10; (0:10).^2]')));

s3 = Suite('Ordering Tests');
s.add(s3);
s3.add(Test('CascadedModelTest', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest1.dsl'], 10,'-quiet')), '-equal', ...
            struct('y', [0:10; 0:10;cumsum([0 0:9])]')));
s3.add(Test('FlippedCascadedModelTest', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest2.dsl'], 10,'-quiet')), '-equal', ...
            struct('y', [0:10; cumsum([0 0:9]); 0:10]')));
s3.add(Test('InterconnectedModelTest1', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest3.dsl'], 10,'-quiet')), '-equal', ...
            struct('y', [0:10; zeros(1,11); zeros(1,11)]')));
s3.add(Test('InterconnectedModelTest2', @()(simex(['models_FeatureTests/' ...
                    'OrderingTest4.dsl'], 10,'-quiet')), '-equal', ...
            struct('y', [0:10; 2.^(0:10); 2.^(0:10)]')));




end

