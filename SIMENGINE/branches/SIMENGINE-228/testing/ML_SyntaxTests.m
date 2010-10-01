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

s = Suite(['MATLAB DIESEL Syntax Tests ' target]);

% Add each of the syntax tests
if strcmp(target, '-cpu')
    s.add(InvalidCodeTests);
    s.add(SubModelTests);
end

end


function s = InvalidCodeTests

s = Suite('Invalid Code');

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


function s = SubModelTests

s = Suite('SubModel Tests');

    function m = SquareSubModel
        m = Model('sub');
        x = m.input('x');
        m.output('y', x^2);
    end

s_len = Suite('Length Tests');
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
        r = (length(sm) == 1);
    end

s_len.add(Test('OneSubModel B', @OneSubModelB));

    function r = MultiSubModels1D
        m = Model('top');
        sm = m.submodel(SquareSubModel, 10);
        r = (length(sm) == 10);
    end

s_len.add(Test('1D Vector Submodels', @MultiSubModels1D));

    function r = MultiSubModels2D
        m = Model('top');
        dims = [5 10];
        sm = m.submodel(SquareSubModel, dims);
        r = all(size(sm) == dims) && (length(sm) == dims(1));
    end

s_len.add(Test('2D Matrix Submodels', @MultiSubModels2D));

    function r = MultiSubModels3D
        m = Model('top');
        dims = [5 10 20];
        sm = m.submodel(SquareSubModel, dims);
        r = all(size(sm) == dims) && (length(sm) == dims(1));
    end

s_len.add(Test('3D Volume Submodels', @MultiSubModels3D));

end
