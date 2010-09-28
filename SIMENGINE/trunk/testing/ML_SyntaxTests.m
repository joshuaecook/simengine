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
    'cycle exists'));

end


