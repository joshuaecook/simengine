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
s.add(VectorizedOperationTests(target));
s.add(SubReferenceTests(target), {'subref'});


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

function s = VectorizedOperationTests(target)

s = Suite('VectorizedOperationTests');

exp_output = struct('x', struct('time', 0:10, 'values', zeros(3,2,11)), ...
                    'transpose_x', struct('time', 0:10, 'values', zeros(2,3,11)));
exp_output.x.values(:,:,2:11) = cumsum(ones(3,2,10),3);
exp_output.transpose_x.values(:,:,2:11) = cumsum(ones(2,3,10),3);
s.add(Test('Transpose Matrix', @()(simex('models_VectorizedTests/OpTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));

exp_output = struct('x', struct('time', 0:10, 'values', ones(4,11)), ...
                    't_x', struct('time', 0:10, 'values', ones(1,4,11)), ...
                    't_t_x', struct('time', 0:10, 'values', ones(4,11)));
exp_output.x.values(:,1) = 0:3;
exp_output.x.values = cumsum(exp_output.x.values,2);
exp_output.t_x.values(:,:,1) = (0:3)';
exp_output.t_x.values = cumsum(exp_output.t_x.values,2);
exp_output.t_t_x.values = exp_output.x.values;  % same as original (t_t_x = (x')')
s.add(Test('Transpose Vector', @()(simex('models_VectorizedTests/OpTest2.dsl', 10, target, '-fastcompile')), '-equal', exp_output));

% do a matrix multiplication test, copy the logic from the dsl file
x = [1,2;3,4;5,6];
y = -[1,2,3;4,5,6];
mul1 = zeros(3,3,11);
mul2 = zeros(2,2,11);
for i=1:11
    mul1(:,:,i) = x*y;
    mul2(:,:,i) = y*x;
    x = x + 1;
    y = y - 1;
end
time = 0:10;
exp_output = struct('mul1', struct('time', time, 'value', mul1),...
                    'mul2', struct('time', time, 'value', mul2));
s.add(Test('Matrix Multiplication', @()(simex('models_VectorizedTests/OpTest3.dsl', 10, target, '-fastcompile')), '-equal', exp_output));

% solve a linear set of equations
M = [1 2; 3 4];
b = [1 2]';
x = zeros(2,11);
for i=1:11
    x(:,i) = linsolve(M, b);
    b = b + 1;
end
exp_output = struct('x', struct('time', time, 'value', x));
s.add(Test('Linear Solver', @()(simex('models_VectorizedTests/OpTest4.dsl', 10, target, '-fastcompile')), '-equal', exp_output));
    
end

function s = SubReferenceTests(target)

s = Suite('Sub Referencing Tests');

s.add(SubReferenceReadTests(target));
s.add(SubReferenceAssignTests(target));

end

function s = SubReferenceReadTests(target)

s = Suite('Sub Referencing Read Tests');

a = ones(11,5);
a(1,:) = 0:4;
exp_output = struct('x', cumsum(a, 1));
s.add(Test('SubReferenceReadScalar', @()(simex('models_VectorizedTests/SubRefTest1.dsl', 10, target, '-fastcompile')), '-equal', exp_output));


end

function s = SubReferenceAssignTests(target)

s = Suite('Sub Referencing Assign Tests');

end