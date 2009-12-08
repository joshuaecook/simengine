% MESSAGETESTS - runs the tests related to checking output messages
% from the compiler
%
% Usage:
% s = MessageTests - runs all tests
%
%  s = CoreFeatureTests('-release') - runs only those required for a release
%
function s = MessageTests(varargin)
INTERNAL = 0; RELEASE = 1;

if nargin == 0
    mode = RELEASE;
else
    mode = varargin{1};
end

s = Suite('Message Tests');

% Verify the version
s.add(Test('CheckVersion', @()(simex('models_SolverTests/fn_forwardeuler.dsl')),'-regexpmatch', 'v0\.92'));

% Add basic variable tests
s.add(VariableTests);
% s.add(IteratorTests);
% s.add(SolverTests);
s.add(EquationTests);


end

function s = VariableTests

% Define a suite to check for undefined or over defined variables
s = Suite('Variable Tests');

% Start adding tests
s.add(CreateUserErrorTest('UndefinedIntermediate', 'VariableTest1.dsl', ...
                          'Unknown identifier encountered: k'));
s.add(CreateUserErrorTest('UndefinedDiffEquVariable', 'VariableTest2.dsl', ...
                          'Unknown identifier encountered: y'));
s.add(CreateUserErrorTest('UndefinedOutputInHeader', 'VariableTest3.dsl', ...
                          'No quantity found matching output in header: y'));
s.add(CreateUserErrorTest('NoOutputsInHeader', 'VariableTest4.dsl', ...
                          'No outputs specified in model'));
s.add(CreateUserErrorTest('DefinedOutputNotInHeader', 'VariableTest5.dsl', ...
                          'Output x does not appear as a returned quantity in the model header'));
s.add(CreateUserErrorTest('InputAndEquation', 'VariableTest6.dsl', ...
                          'Variable x was defined both as an input and in an equation'));
s.add(CreateUserErrorTest('DuplicateDefinition', 'VariableTest7.dsl', ...
                          'Variable x has already been defined'));
s.add(CreateUserErrorTest('NoEquations', 'VariableTest8.dsl', ...
                          'Model has no equations defined'));

end

% Equation Tests - find errors in equations
function s = EquationTests

s = Suite('Equation Tests')

s.add(CreateUserErrorTest('UndefinedFunction', 'EquationTest1.dsl', ...
                          'Unknown identifier encountered: f'))
s.add(CreateUserErrorTest('DoubleAssignment', 'EquationTest2.dsl', ...
                          'Use an EQUATIONS block to define more than one equation'))

end

% CreateUserErrorTest - Creates a test case (suite) to search for an error message
function s = CreateUserErrorTest(id, dslmodel, expectedstring)
dsl = fullfile('models_MessageTests', dslmodel);

s = Suite(id);
t1 = Test('UserError', @()(simex(dsl)), '-regexpmatch', 'USER ERROR');
t2 = Test('AppropriateMessage', @()(dispAndReturn(t1.Output)), '-regexpmatch', ...
          expectedstring);
t3 = Test('NoFailure', @()(dispAndReturn(t1.Output)), '-regexpmatch', 'FAILURE:');
t3.ExpectFail = true;

s.add(t1);
s.add(t2);
s.add(t3);

end

function y = dispAndReturn(str)
disp(str);
y = true;
end