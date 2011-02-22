

function s = ML_ExpTests(varargin)
s = Suite('MATLAB Exp Tests');
s.add(LiteralExpTests);
s.add(VariableExpTests);
s.add(OperationExpTests);
end

% *************************************** Literal Exp Tests ************************************
function s = LiteralExpTests()
s = Suite('MATLAB Literal Exp Tests');
s.add(ExpLiteralEquivalenceTests);
s.add(ExpLiteralSubsRefTests);
s.add(ExpLiteralSubsRefEndTests);
end

function s = ExpLiteralEquivalenceTests()
s = Suite('Exp Literal Equivalency Tests');
s.add(LiteralEquivalentTest('Single literal', rand(1)));
s.add(LiteralEquivalentTest('1D literal', rand(1,100)));
s.add(LiteralEquivalentTest('2D literal', rand(10,10)));
s.add(LiteralEquivalentTest('3D literal', rand(10,10,10)));
s.add(LiteralEquivalentTest('4D literal', rand(10,10,10,2)));
end

function t = LiteralEquivalentTest(testname, literal)
t = Test(testname, @()(eval(Exp(literal).toMatStr)), '-equal', literal);
end

function s = ExpLiteralSubsRefTests()
s = Suite('Exp Literal SubsRef Tests');
s.add(LiteralSubsRefTest('single literal - single subref', rand(1,1), ':'));
s.add(LiteralSubsRefTest('1D literal - single subref', rand(1,10), 5));
s.add(LiteralSubsRefTest('1D literal - 1D subref', rand(1,10), 2:10));
s.add(LiteralSubsRefTest('2D literal - single subref', rand(10), 2, 5));
s.add(LiteralSubsRefTest('2D literal - 1D subref', rand(10), ':', 5));
s.add(LiteralSubsRefTest('2D literal - 2D subref', rand(10), 1:5, 5:10));
s.add(LiteralSubsRefTest('3D literal - single subref', rand(10,10,10), 503));
s.add(LiteralSubsRefTest('3D literal - 1D subref', rand(10,10,10), 6,7,':'));
s.add(LiteralSubsRefTest('3D literal - 2D subref', rand(10,10,10), 6,7:-1:4,':'));
s.add(LiteralSubsRefTest('3D literal - 3D subref', rand(10,10,10), 5:6,7,':'));
s.add(LiteralSubsRefTest('4D literal - single subref', rand(5,5,5,5), 1,2,3,4));
s.add(LiteralSubsRefTest('4D literal - 1D subref', rand(5,5,5,5), 1,2,3,4:5));
s.add(LiteralSubsRefTest('4D literal - 2D subref', rand(5,5,5,5), 1,2,':',4:5));
s.add(LiteralSubsRefTest('4D literal - 3D subref', rand(5,5,5,5), [1 3 5],2,':',4:5));
s.add(LiteralSubsRefTest('4D literal - 4D subref', rand(5,5,5,5), [1 3 5],3:4,':',4:5));
end

function t = LiteralSubsRefTest(testname, literal, varargin)
e = Exp(literal);
l = literal(varargin{:});
t = Test(testname, @()(eval(e(varargin{:}).toMatStr)), '-equal', l);
end

function s = ExpLiteralSubsRefEndTests()
s = Suite('Exp Literal SubsRef End Tests');
l = rand(5,5,5,5);
e = Exp(l);
s.add(Test('4D literal - end', @()(eval(e(end).toMatStr)), '-equal', l(end)));
s.add(Test('4D literal - 2:end,5', @()(eval(e(2:end,5).toMatStr)), '-equal', l(2:end,5)));
s.add(Test('4D literal - 5,end,3', @()(eval(e(5,end,3).toMatStr)), '-equal', l(5,end,3)));
s.add(Test('4D literal - end,end,end,end-1', @()(eval(e(end,end,end,end-1).toMatStr)), '-equal', l(end,end,end,end-1)));
s.add(Test('4D literal - 1:2:end', @()(eval(e(1:2:end).toMatStr)), '-equal', l(1:2:end)));
s.add(Test('4D literal - 1,2,2:end', @()(eval(e(1,2,2:end).toMatStr)), '-equal', l(1,2,2:end)));
end

% *************************************** Variable Exp Tests ************************************

function s = VariableExpTests()
s = Suite('Variable Exp Tests');
s.add(VariableExpEquivalenceTests);
s.add(VariableExpSubsRefTests);
end

function s = VariableExpEquivalenceTests()
s = Suite('Variable Exp Equivalence Tests');
s.add(Test('Single variable', @()(VariableExpEquivalence(5))));
s.add(Test('1D variable', @()(VariableExpEquivalence(rand(1,10)))));
s.add(Test('2D variable', @()(VariableExpEquivalence(rand(5)))));
s.add(Test('3D variable', @()(VariableExpEquivalence(rand(5,5,5)))));
s.add(Test('4D variable', @()(VariableExpEquivalence(rand(5,5,5,5)))));
end

function p = VariableExpEquivalence(value)
v = value;
e = Exp('v', size(v));  % We are using a variable 'v' defined in Matlab to validate a variable Exp
                        % Size is not really needed here, but is included for consistency with the subsref tests
p = isequal(size(v), size(eval(e.toMatStr))) && isequal(v, eval(e.toMatStr));
end

function s = VariableExpSubsRefTests()
s = Suite('Variable Exp SubsRef Tests');
s.add(Test('single variable - single subref', @()(VariableSubsRefTest(rand(1,1), ':'))));
s.add(Test('1D variable - single subref', @()(VariableSubsRefTest(rand(1,10), 5))));
s.add(Test('1D variable - 1D subref', @()(VariableSubsRefTest(rand(1,10), 2:10))));
s.add(Test('2D variable - single subref', @()(VariableSubsRefTest(rand(10), 2, 5))));
s.add(Test('2D variable - 1D subref', @()(VariableSubsRefTest(rand(10), ':', 5))));
s.add(Test('2D variable - 2D subref', @()(VariableSubsRefTest(rand(10), 1:5, 5:10))));
s.add(Test('3D variable - single subref', @()(VariableSubsRefTest(rand(10,10,10), 503))));
s.add(Test('3D variable - 1D subref', @()(VariableSubsRefTest(rand(10,10,10), 6,7,':'))));
s.add(Test('3D variable - 2D subref', @()(VariableSubsRefTest(rand(10,10,10), 6,7:-1:4,':'))));
s.add(Test('3D variable - 3D subref', @()(VariableSubsRefTest(rand(10,10,10), 5:6,7,':'))));
s.add(Test('4D variable - single subref', @()(VariableSubsRefTest(rand(5,5,5,5), 1,2,3,4))));
s.add(Test('4D variable - 1D subref', @()(VariableSubsRefTest(rand(5,5,5,5), 1,2,3,4:5))));
s.add(Test('4D variable - 2D subref', @()(VariableSubsRefTest(rand(5,5,5,5), 1,2,':',4:5))));
s.add(Test('4D variable - 3D subref', @()(VariableSubsRefTest(rand(5,5,5,5), [1 3 5],2,':',4:5))));
s.add(Test('4D variable - 4D subref', @()(VariableSubsRefTest(rand(5,5,5,5), [1 3 5],3:4,':',4:5))));
end

function p = VariableSubsRefTest(value, varargin)
v = value;
e = Exp('v', size(v));
v2 = v(varargin{:});
e2 = e(varargin{:});
p = isequal(size(v2), size(eval(e2.toMatStr))) && isequal(v2, eval(e2.toMatStr));
end

% *************************************** Operation Exp Tests ************************************
function s = OperationExpTests()
s = Suite('Operation Exp Tests');
s.add(OperationExpEquivalenceTests);
s.add(OperationExpSubsRefTests);
end

function s = OperationExpEquivalenceTests()
s = Suite('Operation Exp Equivalence Tests (L = literal, V = variable)');
literal = true;
variable = false;
ops = {'+', '-', '.*', './'};
dims = {[1 1], [1 5], [5 5], [5 5 5], [5 5 5 5]};
for o = 1:length(ops)
  for da = 1:length(dims)
    for db = 1:length(dims)
      s.add(Test(['L' mat2str(dims{da}) ops{o} 'L' mat2str(dims{db})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{da}, dims{db}, literal, literal))));
      s.add(Test(['L' mat2str(dims{da}) ops{o} 'V' mat2str(dims{db})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{da}, dims{db}, literal, variable))));
      s.add(Test(['V' mat2str(dims{da}) ops{o} 'L' mat2str(dims{db})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{da}, dims{db}, variable, literal))));
      s.add(Test(['V' mat2str(dims{da}) ops{o} 'V' mat2str(dims{db})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{da}, dims{db}, literal, variable))));
      if ~isequal(dims{da}, dims{db})
        s.add(Test(['L' mat2str(dims{db}) ops{o} 'L' mat2str(dims{da})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{db}, dims{da}, literal, literal))));
        s.add(Test(['L' mat2str(dims{db}) ops{o} 'V' mat2str(dims{da})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{db}, dims{da}, literal, variable))));
        s.add(Test(['V' mat2str(dims{db}) ops{o} 'L' mat2str(dims{da})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{db}, dims{da}, variable, literal))));
        s.add(Test(['V' mat2str(dims{db}) ops{o} 'V' mat2str(dims{da})], @()(BinaryOperatorExpEquivalence(ops{o}, dims{db}, dims{da}, literal, variable))));
      end
    end
  end
end
end

function [p, opA, opB, r, eR] = BinaryOperatorExpEquivalence(oper, dimOpA, dimOpB, litA, litB)
expectError = false;
% Matlab operation on literals
opA = rand(dimOpA);
opB = rand(dimOpB);
try
  r = eval(['opA' oper 'opB']);
catch
  expectError = true;
end

% Create expression operands
if litA
  eOpA = Exp(opA);
else
  eOpA = Exp('opA', dimOpA);
end
if litB
  eOpB = Exp(opB);
else
  eOpB = Exp('opB', dimOpB);
end

try
  % Create resultant expression
  eR = eval(['eOpA' oper 'eOpB']);
  p = isequal(size(eR), size(r)) && isequal(eval(eR.toMatStr), r) && ~expectError;
catch
  p = expectError;
end

end


function s = OperationExpSubsRefTests()
s = Suite('Operation Exp SubsRef Tests');
literal = true;
variable = false;
ops = {'+', '.*'};
dims = {[1 1], [1 5], [5 5], [5 5 5], [5 5 5 5]};
subrefs{1} = {{':'}};
subsrefs{2} = {{4}, {2:4}};
subsrefs{3} = {{2 5}, {':' 5}, {1:4 2:5}};
subsrefs{4} = {{3 4 3}, {2 3 1:3}, {3 [2 4] 1:3}, {3:5 [1 5] 2:3}};
subsrefs{5} = {{4 3 2 1}, {3 4 2 ':'}, {[2 5] 4 1 ':'}, {':' ':' 5 ':'}, {[2 4] [1 5] 2:3 ':'}};
subsrefsNames = {'single subref', '1D subref', '2D subref', '3D subref', '4D subref'};
for o = 1:length(ops)
  for da = 1:length(dims)
    for db = [1 da]
      for sr = 1:da
        subs = subsrefs{sr};
        for srr = 1:length(subs)
          s.add(Test(['L' mat2str(dims{da}) ops{o} 'V' mat2str(dims{db}) ' : ' subsrefsNames{srr}], @()(BinaryOperatorExpSubsRef(ops{o}, dims{da}, dims{db}, literal, variable, subs{srr}))));
          s.add(Test(['V' mat2str(dims{da}) ops{o} 'L' mat2str([1 1]) ' : ' subsrefsNames{srr}], @()(BinaryOperatorExpSubsRef(ops{o}, dims{da}, [1 1], variable, literal, subs{srr}))));
        end
      end
    end
  end
end
end

function p = BinaryOperatorExpSubsRef(oper, dimOpA, dimOpB, litA, litB, subs)
[p, opA, opB, r, eR] = BinaryOperatorExpEquivalence(oper, dimOpA, dimOpB, litA, litB);
if p == true
  p = isequal(eval(eR(subs{:}).toMatStr), r(subs{:}));
end
end