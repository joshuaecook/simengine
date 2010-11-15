% ML_SYNTAXTESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = ML_SyntaxTests - runs all tests
%  s = ML_SyntaxTests('-cpu', '-release')
%  s = ML_SyntaxTests('-release') - runs only those required for a release
%
function s = ML_SyntaxTests(varargin)

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

s = Suite(['MATLAB DIESEL Syntax Tests ' target], {'matlab'});

% Add each of the syntax tests
if strcmp(target, '-cpu')
    s.add(ExpTests);
    s.add(SubModelTests);
    s.add(InvalidCodeTests);
end

end


function s = InvalidCodeTests

s = Suite('Invalid Code', {'simex', 'messages'});

% empty model
    function m = EmptyModel
        m = Model('EmptyModel');
    end

s.add(CreateUserErrorTest('EmptyModel', EmptyModel, ...
    'Model has no states or outputs'));

% made up variable
    function m = MadeUpVariable
        m = Model('MadeUpVariable');
        m.output('out', Exp('x'));
    end

s.add(CreateUserErrorTest('MadeUpVariable', MadeUpVariable, ...
    'Quantity x is undefined'));

% algebraic loop
    function m = AlgebraicLoop
        m = Model('AlgebraicLoop');
        x = Exp('x');
        y = m.equ('y', x);
        x = m.equ('x', y);
        m.output('out', x, y);
    end
s.add(CreateUserErrorTest('AlgebraicLoop', AlgebraicLoop, ...
    'Cycle found'));

end

% A series of tests to verify that expressions work as expected
function s = ExpTests

s = Suite('Expression Tests');

noErrorTest = @(id, fcn)(Test(id, fcn, '-withouterror'));

% First test basic instantiation of terminals
s_usage = Suite('Instantiation Tests');
s_usage.add(noErrorTest('literal', @()(Exp(1))));
s_usage.add(noErrorTest('var', @()(Exp('a'))));

    function sub = getSubModel()
        mdl = Model('MyModel');
        sub_mdl = Model('MySubModel');
        sub_mdl.output('x', 1);
        sub = mdl.submodel(sub_mdl);
    end

s_usage.add(noErrorTest('reference', @()(Exp(getSubModel(),'x')))); % This is failing because I switched the first parameter to an instance in Exp.m
s_usage.add(noErrorTest('iterator', @()(Exp(Iterator('t')))));
t = Test('cell', @()(Exp({'a'})), '-withouterror');
t.ExpectFail = true;
s_usage.add(t);
s.add(s_usage);

% Now, we can move into symbolic operations
s_ops = Suite('Operation Tests');
a = Exp('a'); b = Exp('b'); c = Exp('c');
% Algebraic operators
s_ops.add(noErrorTest('uplus', @()(+a)));
s_ops.add(noErrorTest('uminus', @()(-a)));
s_ops.add(noErrorTest('plus', @()(a+b)));
s_ops.add(noErrorTest('minus', @()(a-b)));
s_ops.add(noErrorTest('times', @()(a.*b)));
s_ops.add(noErrorTest('mtimes', @()(a*b)));
s_ops.add(noErrorTest('mpower', @()(a^b)));
s_ops.add(noErrorTest('mrdivide', @()(a/b)));
s_ops.add(noErrorTest('mldivide', @()(a/b)));
s_ops.add(noErrorTest('mod', @()(mod(a,b))));

% Relational operators
s_ops.add(noErrorTest('eq', @()(a==b)));
s_ops.add(noErrorTest('neq', @()(a~=b)));
s_ops.add(noErrorTest('lt', @()(a<b)));
s_ops.add(noErrorTest('gt', @()(a>b)));
s_ops.add(noErrorTest('le', @()(a<=b)));
s_ops.add(noErrorTest('ge', @()(a>=b)));

% Logical operators
s_ops.add(noErrorTest('and', @()(a&b)));
s_ops.add(noErrorTest('or', @()(a|b)));
s_ops.add(noErrorTest('not', @()(~a)));
s_ops.add(noErrorTest('any', @()(any(a,b,c))));
s_ops.add(noErrorTest('all', @()(all(a,b,c))));

% Trancendental and other unary functions
fcns = {'sin', 'cos', 'tan', 'csc', 'sec', 'cot', ...
       'asin', 'acos', 'atan', 'acsc', 'asec', 'acot', ...
       'sinh', 'cosh', 'tanh', 'csch', 'sech', 'coth', ...
       'asinh', 'acosh', 'atanh', 'acsch', 'asech', 'acoth', ...
       'log', 'log10', 'log10', 'sqrt', ...
        'round', 'floor', 'ceil', 'abs'};
for i=1:length(fcns)
  s_ops.add(noErrorTest(fcns{i}, @()(feval(fcns{i},a))));
end

% additional speciality functions
s_ops.add(noErrorTest('atan2', @()(atan2(a,b))));
s_ops.add(noErrorTest('transpose', @()(a')));
s_ops.add(noErrorTest('piecewise', @()(piecewise(a,b,c))));
s_ops.add(noErrorTest('sum', @()(sum(a,b,c))));
s_ops.add(noErrorTest('prod', @()(prod(a,b,c))));
s_conv = Suite('Convolution Tests');
t = Test('conv_two_vars', @()(conv(a,b)), '-withouterror');
t.ExpectFail = true; % wrong
s_conv.add(t);
t = Test('conv_with_vector', @()(conv(a,[1 1 1])), '-withouterror');
t.ExpectFail = true; % wrong
s_conv.add(t);
t = Test('conv_with_vector', @()(conv(a(Iterator('n','discrete')),[1 ...
                    1 1])), '-withouterror');
t.ExpectFail = false; % correct
s_conv.add(t);
s_ops.add(s_conv);
s_ops.add(noErrorTest('max', @()(max(a,b))));
s_ops.add(noErrorTest('min', @()(min(a,b))));

s.add(s_ops);

% Vector and Matrix literals
s_vec = Suite('Non-scalar Literal Tests', {'vectorization', 'v2.0'});

verifySizeTest = @(id, fcn, expected)(Test(id, @()(ndims(fcn()) == ...
                                                  length(expected) ...
                                                  && isequal(size(fcn()),expected))));
s_vec.add(verifySizeTest('VerifyTestFunction', @()(rand(2,3,5)), [2 3 5]));
a = Exp(1);
v = Exp(rand(1,10));
M = Exp(rand(3,5));
Volume = Exp(rand(2,3,4));
s_vec.add(verifySizeTest('vector', @()(v), [1 10]));
s_vec.add(verifySizeTest('matrix', @()(M), [3 5]));
s_vec.add(verifySizeTest('vector transpose', @()(v'), [10 1]));
s_vec.add(verifySizeTest('matrix transpose', @()(M'), [5 3]));
s_vec.add(verifySizeTest('volume', @()(Volume), [2 3 4]));

s.add(s_vec);

% Vector operations
s_vec_ops = Suite('Vector operations', {'vectorization', 'v2.0'});

function t = expectErrorTest(id, fcn)
t = Test(id, fcn, '-withouterror');
t.ExpectFail = true;
end

s_vec_ops.add(verifySizeTest('vector-scalar mplus', @()(v+a), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar times', @()(v*a), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar power', @()(v^a), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar divide', @()(v/a), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar inverted mplus', @()(a+v), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar inverted times', @()(a*v), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar inverted power', @()(a^v), [1 10]));
s_vec_ops.add(verifySizeTest('vector-scalar inverted divide', @()(a/v), [1 10]));

s_vec_ops.add(verifySizeTest('vector mplus', @()(v+v), [1 10]));
s_vec_ops.add(verifySizeTest('vector times', @()(v.*v), [1 10]));
s_vec_ops.add(verifySizeTest('vector power', @()(v.^v), [1 10]));
s_vec_ops.add(verifySizeTest('vector divide', @()(v./v), [1 10]));

s_vec_ops.add(verifySizeTest('vector sin', @()(sin(v)), [1 10]));
s_vec_ops.add(verifySizeTest('vector acoth', @()(acoth(v)), [1 10]));
s_vec_ops.add(verifySizeTest('vector exp', @()(exp(v)), [1 10]));
s_vec_ops.add(verifySizeTest('vector log', @()(log(v)), [1 10]));

s_vec_ops.add(expectErrorTest('matrix times wrong dims', @()(v*v)));
s_vec_ops.add(expectErrorTest('matrix times wrong dims transposed', @()(v'*v')));
s_vec_ops.add(verifySizeTest('matrix times A', @()(v*v'), [1 1]));
s_vec_ops.add(verifySizeTest('matrix times B', @()(v'*v), [10 10]));

s.add(s_vec_ops);

s_mat_ops = Suite('Matrix operations', {'vectorization', 'v2.0'});

s_mat_ops.add(expectErrorTest('matrix times wrong dims', @()(M*M)));
s_mat_ops.add(expectErrorTest('matrix times wrong dims transposed', @()(M'*M')));
s_mat_ops.add(verifySizeTest('matrix times A', @()(M*M'), [3 3]));
s_mat_ops.add(verifySizeTest('matrix times B', @()(M'*M), [5 5]));

s_mat_ops.add(verifySizeTest('linear solve', @()(linsolve(Exp(rand(10,10)), ...
                                                  Exp(rand(10,1)))), ...
                                                 [10 1]));

s.add(s_mat_ops);

end

function s = SubModelTests

s = Suite('SubModel Tests', {'submodels'});

    function m = SquareSubModel
        m = Model('sub');
        x = m.input('x');
        m.output('y', x^2);
    end

s_len = Suite('Length Tests', {'vectorization', 'v2.0'});
s.add(s_len);


    function r = OneSubModelA
        m = Model('top');
        sm = m.submodel(SquareSubModel);
        r = (length(sm) == 1);
    end

s_len.add(Test('OneSubModel A', @OneSubModelA));

    function r = OneSubModelB
        m = Model('top');
        sm = m.submodel(SquareSubModel, 1);
        r = (length(sm) == 1) && (numel(sm) == 1);
    end

s_len.add(Test('OneSubModel B', @OneSubModelB));

    function r = MultiSubModels1DA
        m = Model('top');
        dims = [1 10];
        sm = m.submodel(SquareSubModel, dims);
        r = isequal(size(sm), dims) && (length(sm) == max(dims));
    end

s_len.add(Test('1D Vector Submodels A', @MultiSubModels1DA));

    function r = MultiSubModels1DB
        m = Model('top');
        dims = [10 1];
        sm = m.submodel(SquareSubModel, dims);
        r = isequal(size(sm),dims) && (length(sm) == max(dims));
    end

s_len.add(Test('1D Vector Submodels B', @MultiSubModels1DB));

    function r = MultiSubModels2DA
        m = Model('top');
        dims = [5 10];
        sm = m.submodel(SquareSubModel, dims);
        r = isequal(size(sm), dims) && (length(sm) == max(dims));
    end

s_len.add(Test('2D Matrix Submodels A', @MultiSubModels2DA));

    function r = MultiSubModels2DB
        m = Model('top');
        dims = 10;
        sm = m.submodel(SquareSubModel, dims);
        r = isequal(size(sm), [dims dims]) && (length(sm) == dims);
    end

s_len.add(Test('2D Matrix Submodels B', @MultiSubModels2DB));

function r = MultiSubModels3D
        m = Model('top');
        dims = [5 10 20];
        sm = m.submodel(SquareSubModel, dims);
        r = isequal(size(sm), dims) && (length(sm) == max(dims));
    end

s_len.add(Test('3D Volume Submodels', @MultiSubModels3D));

end
