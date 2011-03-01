%% simEngine Supported Operations
% When using the <simEngine_Model_help.html DIESEL for MATLAB
% Model generation toolkit>, equations generated are automatically
% converted in simEngine expressions.  This is done because each variable
% that is created by a method in <simEngine_Model_help.html
% Model> that generates a variable actually returns an
% *Exp* variable.  The methods in <simEngine_Model_help.html
% Model> that return an Exp type are: <matlab:doc('Model.input') input>,
% <matlab:doc('Model.random') random>, and <matlab:doc('Model.input')
% state>.
%
% Expressions can also be created using the constructor:
%
% * <matlab:doc('Exp.Exp') Exp> - create a new *Exp* object
%
% Once variables are created, the following operations, as defined in
% <matlab:doc('Exp') Exp>, are listed here:
%
%% Algebraic
%
% * plus      - Plus (+)
% * uplus     - Unary plus (+)
% * minus     - Minus (-)
% * uminus    - Unary minus (-)
% * mtimes    - Multiply (*)
% * mpower    - Power (^)
% * mrdivide  - Divide (/)
% * mldivide  - Left divide (\)
% * mod       - Modulus
%
%% Relational
%
% * eq        - Equal (==)
% * neq       - Not equal (~=)
% * lt        - Less than (<)
% * gt        - Greater than (>)
% * le        - Less than or equal (<=)
% * ge        - Greater than or equal (>=)
%
%% Logical
%
% * and       - Logical AND (&)
% * or        - Logical OR (|)
% * not       - Logical NOT (~)
% * any       - True if any elements are true
% * all       - True if all elements are true
%
%% Trigonometric
%
% * sin, cos, tan          - Trigonometric functions
% * csc, sec, cot          - Reciprocal trigonometric functions
% * asin, acos, atan       - Inverse trigonometric functions
% * atan2                  - Four quadrant inverse tangent
% * acsc, asec, acot       - Inverse reciprocal trigonometric functions
% * sinh, cosh, tanh       - Hyperbolic functions
% * csch, sech, coth       - Reciprocal hyperbolic functions
% * asinh, acosh, atanh    - Inverse hyperbolic functions
% * acsch, asech, acoth    - Inverse reciprocal hyperbolic functions
%
%% Exponential
%
% * exp       - Exponential
% * log       - Natural logarithm
% * log10     - Base 10 logarithm
% * sqrt      - Square root
%
%% Rounding:
%
% * round     - Round towards nearest integer
% * floor     - Round towards negative infinity
% * ceil      - Round towards positive infinity
%
%% Speciality:
%
% * abs       - Absolute value
% * <matlab:doc('Exp.piecewise') piecewise> - Functional conditional (see also <matlab:doc('piecewise') piecewise> for numeric types)
% * sum       - Sum of elements
% * prod      - Product of elements
% * conv      - Convolution of Exp and numeric vector
% * max       - Max of two arguments
% * min       - Min of two arguments
%
%% Examples

a = Exp('a');
b = Exp('b');
% for a right triangle, the hypotenuse has size
c = sqrt(a^2 + b^2)

