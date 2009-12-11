function s = TemporalIteratorTests(varargin)
% TEMPORALITERATORTESTS runs a set of tests across temporal iterators
%   mode can be either 0 for internal or 1 for release
INTERNAL = 0; RELEASE = 1;

target = varargin{1};

s = Suite(['Temporal Iterator Feature Tests ' target]);
s.add(SimpleIteratorTests(target));
s.add(UpdateIteratorTests(target));
s.add(PostProcessIteratorTests(target));
s.add(MultipleTemporalIteratorTests(target));
s.add(MultipleIteratorsSubModelTests(target));
s.add(ImmediateIteratorTests(target));

end


function s = SimpleIteratorTests(target)

s = Suite(['Simple Iterator Tests ' target]);

s.add(Test('OneTimeIteratorImplicit',@()(simex('models_FeatureTests/OneTimeIteratorTest1.dsl',10,target)), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('OneTimeIteratorExplicit',@()(simex('models_FeatureTests/OneTimeIteratorTest2.dsl',10,target)), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('OneDiscreteIterator',@()(simex('models_FeatureTests/OneDiscreteIteratorTest1.dsl',10,target)), '-equal', struct('x', [0:10; 0:10]')));

% Parallel tests
s.add(Test('OneTimeIteratorImplicit parallel',@()(simex('models_FeatureTests/OneTimeIteratorTest1.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));
s.add(Test('OneTimeIteratorExplicit parallel',@()(simex('models_FeatureTests/OneTimeIteratorTest2.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));
s.add(Test('OneDiscreteIterator parallel',@()(simex('models_FeatureTests/OneDiscreteIteratorTest1.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));

end

function s = UpdateIteratorTests(target)

s = Suite(['Update Iterator Tests ' target]);

sawtooth = [0 1 2 3 0 1 2 3 0 1 2];
s.add(Test('UpdateTimeIterator', ...
           @()(simex(['models_FeatureTests/UpdateContinuousIteratorTest1.dsl'],10)), ...
           '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateContinuousIterator',@()(simex('models_FeatureTests/UpdateContinuousIteratorTest2.dsl',10,target)), '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateDiscreteNIterator',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest1.dsl',10,target)), '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateOtherDiscreteIterator',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest2.dsl',10,target)), '-equal', struct('x', [0:10; sawtooth]')));

% Parallel tests
s.add(Test('UpdateTimeIterator parallel',@()(simex(['models_FeatureTests/UpdateContinuousIteratorTest1.dsl'],10,zeros(10,1),'-quiet',target)),'-allequal'));
s.add(Test('UpdateContinuousIterator parallel',@()(simex('models_FeatureTests/UpdateContinuousIteratorTest2.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));
s.add(Test('UpdateDiscreteNIterator parallel',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest1.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));
s.add(Test('UpdateOtherDiscreteIterator parallel',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest2.dsl',10,zeros(10,1),'-quiet',target)), '-allequal'));

end

function s = PostProcessIteratorTests(target)

s = Suite(['Post Process Iterator Tests ' target]);

s.add(Test('TwoDelayUsingPPTimeIterator', ...
           @()(simex('models_FeatureTests/PostProcessContinuousIteratorTest1.dsl',10,target)), ...
           '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPContinuousIterator',@()(simex('models_FeatureTests/PostProcessContinuousIteratorTest2.dsl',10,target)), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingIndex', ...
           @()(simex('models_FeatureTests/PostProcessContinuousIteratorTest3.dsl',10,target)), ...
           '-equal', struct('y', [0:10; 0:10; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPDiscreteNIterator',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest1.dsl',10,target)), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPOtherDiscreteIterator',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest2.dsl',10,target)), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));

% Parallel tests
s.add(Test('TwoDelayUsingPPTimeIterator parallel',@()(simex('models_FeatureTests/PostProcessContinuousIteratorTest1.dsl',10,zeros(10,3),'-quiet',target)),'-allequal'));
s.add(Test('TwoDelayUsingPPContinuousIterator parallel',@()(simex('models_FeatureTests/PostProcessContinuousIteratorTest2.dsl',10,zeros(10,3),'-quiet',target)), '-allequal'));
s.add(Test('TwoDelayUsingIndex parallel', @()(simex('models_FeatureTests/PostProcessContinuousIteratorTest3.dsl',10,zeros(10,3),'-quiet',target)), '-allequal'));
s.add(Test('TwoDelayUsingPPDiscreteNIterator parallel',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest1.dsl',10,zeros(10,3),'-quiet',target)), '-allequal'));
s.add(Test('TwoDelayUsingPPOtherDiscreteIterator parallel',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest2.dsl',10,zeros(10,3),'-quiet',target)), '-allequal'));

end

function s = ImmediateIteratorTests(target)

s = Suite(['Immediate Iterator Tests ' target]);

s.add(Test('constant -> output', @()(simex('models_FeatureTests/ImmediateIteratorTest1.dsl', 1, '-quiet',target)), '-equal', struct('G', [0:1; 9.80665 * ones(1,2)]')));
s.add(Test('immediate intermediate -> output', @()(simex('models_FeatureTests/ImmediateIteratorTest2.dsl', 1, '-quiet',target)), '-approxequal', struct('F', [0:1; 2942 * ones(1,2)]')));
s.add(Test('immediate intermediate -> submodel -> output', @()(simex('models_FeatureTests/ImmediateIteratorTest3.dsl', 1, '-quiet',target)), '-approxequal', struct('F', [0:1; 2942 * ones(1,2)]')));
s.add(Test('top input -> submodel -> immediate intermediate -> output', @()(simex('models_FeatureTests/ImmediateIteratorTest4.dsl', 1, '-quiet',target)), '-approxequal', struct('F', [0:1; 2942 * ones(1,2)]')));

% Parallel tests
inputs.r = ones(1,10);
s.add(Test('immediate intermediate -> output parallel', @()(simex('models_FeatureTests/ImmediateIteratorTest2.dsl', 1, inputs, '-quiet',target)), '-allequal'));
s.add(Test('immediate intermediate -> submodel -> output parallel', @()(simex('models_FeatureTests/ImmediateIteratorTest3.dsl', 1, inputs, '-quiet',target)), '-allequal'));
s.add(Test('top input -> submodel -> immediate intermediate -> output parallel', @()(simex('models_FeatureTests/ImmediateIteratorTest4.dsl', 1, inputs, '-quiet',target)), '-allequal'));           

end

function s = MultipleTemporalIteratorTests(target)

s = Suite(['Multiple Temporal Iterator Tests ' target]);

s.add(Test('TwoSameTemporalIterators', @()(simex('models_FeatureTests/TwoTemporalIteratorTest1.dsl',10, '-quiet',target)), '-equal', struct('x1', [0:10; 0:10]', 'x2', [0:10; 0:2:20]')));
s.add(Test('TwoSolversSameDT', @()(simex('models_FeatureTests/TwoTemporalIteratorTest2.dsl',10, '-quiet',target)), '-equal', struct('x1', [0:10; 0:10]', 'x2', [0:10; 0:2:20]')));
s.add(Test('OneSolverTwoSynchDTs', @()(simex('models_FeatureTests/TwoTemporalIteratorTest3.dsl',10, '-quiet',target)), '-equal', struct('x1', [0:10; 0:10]', 'x2', [0:0.5:10; 0:1:20]')));
s.add(Test('TwoSolversTwoSynchDTs', @()(simex('models_FeatureTests/TwoTemporalIteratorTest4.dsl',10, '-quiet',target)), '-equal', struct('x1', [0:10; 0:10]', 'x2', [0:0.5:10; 0:1:20]')));
s.add(Test('TwoSolversTwoSynchDTsWithOutputs', @()(simex('models_FeatureTests/TwoTemporalIteratorTest4b.dsl',10, '-quiet',target)), '-equal', struct('y1', [0:10; 0:10; 0:2:20]', 'y2', [0:0.5:10; floor(0:0.5:10); 0:1:20]')));
s.add(Test('OneSolverTwoASynchDTs',@()(simex('models_FeatureTests/TwoTemporalIteratorTest5.dsl',100,'-quiet',target)),'-equal',struct('y1',[0:10:100;0:10:100; floor((0:10:100)/3)*6]','y2',[0:3:100; floor((0:3:100)/10)*10; 0:(2*3):200]')));
s.add(Test('TwoSolversTwoASynchDTs',@()(simex('models_FeatureTests/TwoTemporalIteratorTest6.dsl',100,'-quiet',target)),'-equal',struct('y1',[0:10:100;0:10:100; floor((0:10:100)/3)*6]','y2',[0:3:100;floor((0:3:100)/10)*10; 0:(2*3):200]')));
s.add(Test('TwoSolversWithIntermediate',@()(simex('models_FeatureTests/TwoTemporalIteratorTest7.dsl',10,'-quiet',target)),'-equal', struct('y', [0:10; 0:10; 0:2:20; 0:3:30]')));
s.add(Test('TwoCoupledIterators',@()(simex('models_FeatureTests/TwoTemporalIteratorTest8.dsl',10, '-quiet',target)),'-equal', struct('x1', [0:10; [0 2.^(0:9)]]','x2', [0:10; [1 2.^(0:9)]]')));
s.add(Test('TwoCoupledIteratorsInSubModel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest9.dsl',10, '-quiet',target)),'-equal', struct('x1', [0:10; [0 2.^(0:9)]]','x2', [0:10; [1 2.^(0:9)]]')));

% Parallel tests
s.add(Test('TwoSameTemporalIterators parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest1.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('TwoSolversSameDT parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest2.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('OneSolverTwoSynchDTs parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest3.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('TwoSolversTwoSynchDTs parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest4.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('TwoSolversTwoSynchDTsWithOutputs parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest4b.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('OneSolverTwoASynchDTs parallel',@()(simex('models_FeatureTests/TwoTemporalIteratorTest5.dsl',100,zeros(10,2),'-quiet',target)),'-allequal'));
s.add(Test('TwoSolversTwoASynchDTs parallel',@()(simex('models_FeatureTests/TwoTemporalIteratorTest6.dsl',100,zeros(10,2),'-quiet',target)),'-allequal'));
s.add(Test('TwoSolversWithIntermediate parallel',@()(simex('models_FeatureTests/TwoTemporalIteratorTest7.dsl',10,zeros(10,2),'-quiet',target)),'-allequal'));
states = [0 1; 0 1; 0 1; 0 1; 0 1; 0 1; 0 1; 0 1; 0 1; 0 1];
s.add(Test('TwoCoupledIterators parallel',@()(simex('models_FeatureTests/TwoTemporalIteratorTest8.dsl',10,states, '-quiet',target)),'-allequal'));
s.add(Test('TwoCoupledIteratorsInSubModel parallel', @()(simex('models_FeatureTests/TwoTemporalIteratorTest9.dsl',10,states, '-quiet',target)),'-allequal'));

end

function s = MultipleIteratorsSubModelTests(target)

s = Suite(['Multiple Temporal Iterators through Sub-Model Tests ' target]);


s.add(Test('UpdateExpInSubModel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest1.dsl',10, '-quiet',target)), '-equal', struct('y', [0:10; mod(0:10,4)]')));
s.add(Test('TwoIteratorsAcrossSubModels', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest2.dsl',10, '-quiet',target)), '-equal', struct('y1', [0:10; 0:10]','y2', [0:10; 0:2:20]')));
s.add(Test('TwoIteratorsMixedAcrossSubModels',  @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest3.dsl',10, '-quiet',target)), '-equal', struct('y1', [0:10; 0:10; 0:2:20]','y2', [0:10; 0:2:20; 0:4:40]')));
s.add(Test('MoreComplexTwoIteratorsMixedAcrossSubModels', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest4.dsl',10, '-quiet',target)), '-equal', ...
                     struct('ya', [0:10; 0:10; 0:2:20]', ...
                            'yb', [0:10; 0:2:20; 0:4:40]', ...
                            'ya1', [0:10; 0:10; 0:2:20]', ...
                            'yb1', [0:10; 0:2:20; 0:4:40]', ...
                            'ya2', [0:10; 0:10; 0:4:40]', ...
                            'yb2', [0:10; 0:2:20; 0:2:20]')));
s.add(Test('PostProcessInSubmodel',@()(simex('models_FeatureTests/TemporalIteratorSubModelsTest5.dsl',10, '-quiet',target)),'-equal', struct('y', [0:10; 0 0 0:8]')));
s.add(Test('PostProcessAndUpdateInSubmodel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest6.dsl',10, '-quiet',target)),'-equal', struct('y', [0:10; 0 0 0:3 0:3 0]')));

% Parallel tests
s.add(Test('UpdateExpInSubModel parallel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest1.dsl',10,zeros(10,1), '-quiet',target)), '-allequal'));
s.add(Test('TwoIteratorsAcrossSubModels parallel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest2.dsl',10,zeros(10,2), '-quiet',target)), '-allequal'));
s.add(Test('TwoIteratorsMixedAcrossSubModels parallel',  @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest3.dsl',10,zeros(10,4), '-quiet',target)), '-allequal'));
s.add(Test('MoreComplexTwoIteratorsMixedAcrossSubModels parallel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest4.dsl',10,zeros(10,4), '-quiet',target)), '-allequal'));
s.add(Test('PostProcessInSubmodel parallel',@()(simex('models_FeatureTests/TemporalIteratorSubModelsTest5.dsl',10,zeros(10,3), '-quiet',target)),'-allequal'));
s.add(Test('PostProcessAndUpdateInSubmodel parallel', @()(simex('models_FeatureTests/TemporalIteratorSubModelsTest6.dsl',10, zeros(10,3),'-quiet',target)),'-allequal'));

end
