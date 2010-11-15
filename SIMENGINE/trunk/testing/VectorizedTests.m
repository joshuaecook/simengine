function s = VectorizedTests(target)

if nargin == 0
    target = '-cpu';
else
    switch target
        case {'-gpu', 'gpu'}
            target = '-gpu';
        case {'-cpu', 'cpu'}
            target = 'cpu';
        otherwise
            error('Simatra:VectorizedTests', 'Unrecognized target %s', target);
    end
end

s = Suite(['Vectorized Tests (target=' target ')']);
s.add(VectorizedStateTests(target));
s.add(VectorizedSubModelTests(target), {'submodels'});


end


function s = VectorizedStateTests(target)

s = Suite('VectorizedStateTests');

exp_output = struct('x', struct('time', 0:10, 'values', zeros(2,2,11)));
exp_output.x.values(:,:,2:11) = cumsum(ones(2,2,10),3);
s.add(Test('Matrix State', @()(simex('models_VectorizedTests/StateMatrixTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));
exp_output = struct('x', struct('time', 0:10, 'values', ones(101,11)));
exp_output.x.values(:,1) = 0:100;
exp_output.x.values = cumsum(exp_output.x.values, 2);
s.add(Test('Range State Compile', @()(simex('models_VectorizedTests/StateRangeTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));
exp_output = struct('x', struct('time', 0:10, 'values', ones(5,11)));
exp_output.x.values(:,1) = [0 1 2 3 4];
exp_output.x.values = cumsum(exp_output.x.values, 2);
s.add(Test('Vector State Output', @()(simex('models_VectorizedTests/StateVectorTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));

end

function s = VectorizedSubModelTests(target)

s = Suite('VectorizedSubModelTests');

exp_output = struct('x', struct('time', 0:10, 'values', ones(5,11)));
exp_output.x.values(:,1) = [0 1 2 3 4];
exp_output.x.values = cumsum(exp_output.x.values, 2);
s.add(Test('Vector State in Submodel', @()(simex('models_VectorizedTests/SubModelTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));
exp_output = struct('x', struct('time', 0:10, 'values', ones(5,11)));
exp_output.x.values(:,1) = zeros(5,1);
exp_output.x.values = cumsum(exp_output.x.values, 2);
s.add(Test('Vectorized Submodel', @()(simex('models_VectorizedTests/SubModelTest2.dsl', 10, target, '-fastcompile')), '-equal', exp_output));
exp_output = struct('x', struct('time', 0:10, 'values', ones(5,11)));
exp_output.x.values(:,1) = [0 1 2 3 4];
exp_output.x.values = cumsum(exp_output.x.values, 2);
s.add(Test('Vectorized Input to Submodel', @()(simex('models_VectorizedTests/SubModelTest3.dsl', 10, target, '-fastcompile')), '-equal', exp_output));



end