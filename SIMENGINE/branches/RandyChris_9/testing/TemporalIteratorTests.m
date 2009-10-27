function s = TemporalIteratorTests(mode)
% TEMPORALITERATORTESTS runs a set of tests across temporal iterators
%   mode can be either 0 for internal or 1 for release
INTERNAL = 0; RELEASE = 1;

s = Suite('Temporal Iterator Feature Tests');
s.add(SimpleIteratorTests);
s.add(UpdateIteratorTests);
s.add(PostProcessIteratorTests);

end


function s = SimpleIteratorTests

s = Suite('Simple Iterator Tests');

s.add(Test('OneTimeIteratorImplicit',@()(simex('models_FeatureTests/OneTimeIteratorTest1.dsl',10,'-quiet')), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('OneTimeIteratorExplicit',@()(simex('models_FeatureTests/OneTimeIteratorTest2.dsl',10,'-quiet')), '-equal', struct('x', [0:10; 0:10]')));
s.add(Test('OneDiscreteIterator',@()(simex('models_FeatureTests/OneDiscreteIteratorTest1.dsl',10,'-quiet')), '-equal', struct('x', [0:10; 0:10]')));

end

function s = UpdateIteratorTests

s = Suite('Update Iterator Tests');

sawtooth = [0 1 2 3 0 1 2 3 0 1 2];
s.add(Test('UpdateTimeIterator',@()(simex('models_FeatureTests/UpdateContinuousIteratorTest1.dsl',10,'-quiet')), '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateContinuousIterator',@()(simex('models_FeatureTests/UpdateContinuousIteratorTest2.dsl',10,'-quiet')), '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateDiscreteNIterator',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest1.dsl',10,'-quiet')), '-equal', struct('x', [0:10; sawtooth]')));
s.add(Test('UpdateOtherDiscreteIterator',@()(simex('models_FeatureTests/UpdateDiscreteIteratorTest2.dsl',10,'-quiet')), '-equal', struct('x', [0:10; sawtooth]')));

end

function s = PostProcessIteratorTests

s = Suite('Post Process Iterator Tests');
s.add(Test('TwoDelayUsingPPTimeIterator',@()(simex('models_FeatureTests/PostProcessContinuousIteratorTest1',10,'-quiet')), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPContinuousIterator',@()(simex('models_FeatureTests/PostProcessContinuousIteratorTest2',10,'-quiet')), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPDiscreteNIterator',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest1',10,'-quiet')), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));
s.add(Test('TwoDelayUsingPPOtherDiscreteIterator',@()(simex('models_FeatureTests/PostProcessDiscreteIteratorTest2',10,'-quiet')), '-equal', struct('y', [0:10; 0:10; 0 0:9; 0 0 0:8]')));


end