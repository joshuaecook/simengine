

function s = ML_ExpTests(varargin)

s = Suite('MATLAB Exp Tests');

s.add(ExpLiteralTests);
s.add(ExpLiteralSubsRefTests);

end

function s = ExpLiteralTests()

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
s.add(LiteralSubsRefTest('2D literal', rand(10), 2, 5));
end

function t = LiteralSubsRefTest(testname, literal, varargin)
e = Exp(literal);
m = literal;
t = Test(testname, @()(eval(e(varargin{:}).toMatStr)), '-equal', literal(varargin{:}));
end