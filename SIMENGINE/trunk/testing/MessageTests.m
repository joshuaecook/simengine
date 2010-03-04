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
s.add(Test('CheckVersion', @()(simex('models_SolverTests/fn_forwardeuler.dsl')),'-regexpmatch', 'v0\.93'));

% Add basic variable tests
s.add(SyntaxTests);
s.add(VariableTests);
s.add(IteratorTests);
% s.add(SolverTests);
s.add(EquationTests);
s.add(OutputTests);


end

function s = SyntaxTests

% Define a suite to check for errors found in the lexer/grammar
s = Suite('Syntax Tests');


% Create tests just around the model syntax
s_model = Suite('Model Syntax Tests');
s.add(s_model);

% Add the tests one by one
s_model.add(CreateUserErrorTest('MispelledModel', 'ModelSyntaxTest1.dsl', ...
                                'Unknown identifier encountered: mdel'));
s_model.add(CreateUserErrorTest('NoModelEnd', 'ModelSyntaxTest2.dsl', ...
                                'Adding END keyword'));
s_model.add(CreateUserErrorTest('WrongModelName', 'ModelSyntaxTest3.dsl', ...
                                'No model found with name: ModelSyntaxTest3'));


% Now create tests for states
s_states = Suite('State Syntax Tests');
s.add(s_states);

s_states.add(CreateUserErrorTest('MispelledState', 'StateSyntaxTest1.dsl', ...
                                 'Unknown identifier encountered: stat'))
s_states.add(CreateUserErrorTest('StateNoVariable', 'StateSyntaxTest2.dsl', ...
                                 'STATE keyword must be followed by'));
s_states.add(CreateUserErrorTest('NoInitialValue', 'StateSyntaxTest3.dsl', ...
                                 'Initial value for state x is not specified'));
s_states.add(CreateUserErrorTest('UndefinedProperty', 'StateSyntaxTest4.dsl', ...
                                 'Member SomeProperty not found in object .*x'));
s_states.add(CreateUserErrorTest('NoWithForProperties', 'StateSyntaxTest5.dsl', ...
                                 'Adding WITH keyword'));
s_states.add(CreateUserErrorTest('InputInitialValue', 'StateSyntaxTest6.dsl', ...
                                 'Input values for states not defined'));

% Now create tests for equations
s_equs = Suite('Equation Syntax Tests');
s.add(s_equs);

s_equs.add(CreateUserErrorTest('MispelledEquation', ...
                               'EquationSyntaxTest1.dsl', 'Unknown identifier encountered: euation'));
s_equs.add(CreateUserErrorTest('NoEquationKeyword', ...
                               'EquationSyntaxTest2.dsl', 'Adding EQUATION keyword'));
s_equs.add(CreateUserErrorTest('EQUATIONSnotEQUATION', ...
                               'EquationSyntaxTest3.dsl', 'Removing EQUATIONS keyword'))
s_equs.add(CreateUserErrorTest('NoRHS', ...
                               'EquationSyntaxTest4.dsl', ['EQUATION '...
                               'keyword must be followed by an equation']));


% Tests for submodels
s_sub = Suite('Sub Model Syntax Tests');
s.add(s_sub);

s_sub.add(CreateUserErrorTest('SubModelAsFunction', ...
                              'SubModelSyntaxTest1.dsl',['Identifier '...
                              'S is not a function']))
s_sub.add(CreateUserErrorTest('AccessOutputAsClassMember', ...
                              'SubModelSyntaxTest2.dsl', ...
                              'Member y not found in object model definition S'))
s_sub.add(CreateUserErrorTest('SubModelInputNotDefined', ...
                              'SubModelSyntaxTest3.dsl', ...
                              'input value was not specified'))
s_sub.add(CreateUserErrorTest('SubModelInputDefinedAsEquation', ...
                              'SubModelSyntaxTest4.dsl', ...
                              'left hand side of an equation can only contain'))
s_sub.add(CreateUserErrorTest('DefiningSubModelOutputAsProperty', ...
                              'SubModelSyntaxTest5.dsl', ...
                              'Invalid definition of non-input to submodel'))

% Tests for iterators
s_iters = Suite('Iterator Syntax Tests');
s.add(s_iters);

s_iters.add(CreateUserErrorTest('DiscreteIteratorWithSolver', ...
                                'IteratorSyntaxTest1.dsl', ...
                                'Discrete time iterators can not have an integration method \(solver\) defined'))
s_iters.add(CreateUserErrorTest('DiscreteIteratorWithDiffEq', ...
                                'IteratorSyntaxTest2.dsl', ...
                                'Unexpected derivative found with discrete iterator'))

end

% Iterator specific error tests
function s = IteratorTests

s = Suite('Iterator Tests');

s.add(CreateUserErrorTest('UndefinedIterator', 'IteratorTest1.dsl', ...
                          'Unknown identifier encountered: t_undefined'));
s.add(CreateUserErrorTest('UndefinedIteratorIndex', 'IteratorTest2.dsl', ...
                          'Unknown identifier encountered: t_undefined'));
s.add(CreateUserErrorTest('WrongIteratorIndex', 'IteratorTest3.dsl', ...
                          ['Quantity .* is already assigned to iterator .* therefore '...
                          'can not use iterator']));
s.add(CreateUserErrorTest('ForwardIndexing', 'IteratorTest4.dsl', ...
                          ['Invalid positive temporal index found on quantity']));
s.add(CreateUserErrorTest('CurrentIndexingOnDiscreteState', 'IteratorTest5.dsl', ...
                          ['Invalid temporal index on discrete state x. Discrete states must be defined as x.n.1. on the left hand side of equation']));
s.add(CreateUserErrorTest('ForwardIndexingOnDiscreteState', 'IteratorTest6.dsl', ...
                          ['Invalid temporal index on discrete state x. Discrete states must be defined as x.n.1. on the left hand side of equation']));
s.add(CreateUserErrorTest('InvalidIndexExpression', 'IteratorTest7.dsl', ...
                          ['Invalid index detected on index of x']));
s.add(CreateUserErrorTest('InvalidIteratorOnState', 'IteratorTest8.dsl', ...
                          ['Temporal iterators can not be used as part of a state declaration']));

end

function s = VariableTests

% Define a suite to check for undefined or over defined variables
s = Suite('Variable Tests');

% Start adding tests
s.add(CreateUserErrorTest('UndefinedIntermediate', 'VariableTest1.dsl', ...
                          'Unknown identifier encountered: k'));
s.add(CreateUserErrorTest('UndefinedDiffEquVariable', 'VariableTest2.dsl', ...
                          'Derivative of quantity y not declared as a state is not supported'));
s.add(CreateUserErrorTest('UndefinedOutputInHeader', 'VariableTest3.dsl', ...
                          'No quantity found matching output in header: y'));
s.add(CreateUserErrorTest('NoOutputsInHeader', 'VariableTest4.dsl', ...
                          'Model VariableTest4 does not have any outputs defined'));
s.add(CreateUserErrorTest('DefinedOutputNotInHeader', 'VariableTest5.dsl', ...
                          'Output x does not appear as a returned quantity in the model header'));
s.add(CreateUserErrorTest('InputAndEquation', 'VariableTest6.dsl', ...
                          ['Model quantity x has been defined multiple '...
                          'times']));
s.add(CreateUserErrorTest('DuplicateDefinition', 'VariableTest7.dsl', ...
                          ['Model quantity x has been defined multiple '...
                          'times']));
% we can ignore this one since it's a duplicate to having no
% outputs defined
%s.add(CreateUserErrorTest('NoEquations', 'VariableTest8.dsl', ...
%                          'Model has no equations defined'));
s.add(CreateUserErrorTest('DuplicatedOutput', 'VariableTest9.dsl', ...
                          'Duplicate output x'));

end

% Output Tests - find errors in outputs in single models and sub
% models
function s = OutputTests

s = Suite('Output Tests');

s.add(CreateUserErrorTest('OutputWithoutEquation', 'OutputTest1.dsl', ...
                          ['Model quantity y has no equation associated '...
                           'with it']));
s.add(CreateUserErrorTest('OutputContentWithoutEquation', 'OutputTest2.dsl', ...
                          'Unknown identifier encountered: x'));
s.add(CreateUserErrorTest('OutputConditionWithoutEquation', 'OutputTest3.dsl', ...
                          'Unknown identifier encountered: x'));
s.add(CreateUserErrorTest('OutputContentsThroughSubmodels', 'OutputTest4.dsl', ...
                          'Output .* in model .* can not be a grouping'));
s.add(CreateUserWarningTest('OutputContentsThroughSubmodels', 'OutputTest5.dsl', ...
                            'The condition .* for output .* in model .* is being ignored'));


end

% Equation Tests - find errors in equations
function s = EquationTests

s = Suite('Equation Tests');

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
t3 = Test('NoFailure', @()(dispAndReturn(t1.Output)), '-regexpmatch', 'FAILURE:|Exception');
t3.ExpectFail = true;

s.add(t1);
s.add(t2);
s.add(t3);

end

% CreateUserWarningTest - Creates a test case (suite) to search for
% a warning message
function s = CreateUserWarningTest(id, dslmodel, expectedstring)
dsl = fullfile('models_MessageTests', dslmodel);

s = Suite(id);
t1 = Test('Completes', @()(simex(dsl)), '-withouterror');
t2 = Test('UserWarning', @()(dispAndReturn(t1.Output)), '-regexpmatch', 'WARNING');
t3 = Test('AppropriateMessage', @()(dispAndReturn(t1.Output)), '-regexpmatch', ...
          expectedstring);

s.add(t1);
s.add(t2);
s.add(t3);

end

function y = dispAndReturn(str)
disp(str);
y = true;
end