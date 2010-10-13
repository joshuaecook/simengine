

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
s.add(LiteralSubsRefTest('single literal - single subref', rand(1,1), ':'));
s.add(LiteralSubsRefTest('1D literal - single subref', rand(1,10), 5));
s.add(LiteralSubsRefTest('1D literal - 1D subref', rand(1,10), 2:8));
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
% Not sure how to pass 'end' as part of a subsref in this fashion?  Need a separate test?
end

function t = LiteralSubsRefTest(testname, literal, varargin)
e = Exp(literal);
m = literal(varargin{:});
t = Test(testname, @()(eval(e(varargin{:}).toMatStr)), '-equal', m);
end