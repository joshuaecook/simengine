classdef Exp
% Exp - Build simEngine symbolic expressions in MATLAB
%
% Exp Methods:
%   Constructor:
%   Exp - create a new Exp object
%
%   Algebraic operators:
%   plus      - Plus (+)
%   uplus     - Unary plus (+)
%   minus     - Minus (-)
%   uminus    - Unary minus (-)
%   mtimes    - Multiply (*)
%   mpower    - Power (^)
%   mrdivide  - Divide (/)
%   mldivide  - Left divide (\)
%   mod       - Modulus
%
%   Relational operators:
%   eq        - Equal (==)
%   neq       - Not equal (~=)
%   lt        - Less than (<)
%   gt        - Greater than (>)
%   le        - Less than or equal (<=)
%   ge        - Greater than or equal (>=)
%
%   Logical operators:
%   and       - Logical AND (&)
%   or        - Logical OR (|)
%   not       - Logical NOT (~)
%   any       - True if any elements are true
%   all       - True if all elements are true
%
%   Trigonometric operators:
%   sin, cos, tan          - Trigonometric functions
%   csc, sec, cot          - Reciprocal trigonometric functions
%   asin, acos, atan       - Inverse trigonometric functions
%   atan2                  - Four quadrant inverse tangent
%   acsc, asec, acot       - Inverse reciprocal trigonometric functions
%   sinh, cosh, tanh       - Hyperbolic functions
%   csch, sech, coth       - Reciprocal hyperbolic functions
%   asinh, acosh, atanh    - Inverse hyperbolic functions
%   acsch, asech, acoth    - Inverse reciprocal hyperbolic functions
%   
%   Exponential operators:
%   exp       - Exponential
%   log       - Natural logarithm
%   log10     - Base 10 logarithm
%   sqrt      - Square root
%
%   Rounding:
%   round     - Round towards nearest integer
%   floor     - Round towards negative infinity
%   ceil      - Round towards positive infinity
%
%   Speciality:
%   abs       - Absolute value
%   piecewise - Functional conditional
%   sum       - Sum of elements
%   prod      - Product of elements
%   conv      - Convolution of Exp and numeric vector
%   max       - Max of two arguments
%   min       - Min of two arguments
%
% Copyright 2010 Simatra Modeling Technologies
% Website: www.simatratechnologies.com
% Support: support@simatratechnologies.com
%    
    properties (Constant, Access = private)
        NULLEXP = 0
        VARIABLE = 1
        LITERAL = 2
        OPERATION = 3
        REFERENCE = 4
        ITERATOR = 5
    end
    
    properties (Constant, Access = private)
        NONOTATION = 0
        PREFIX = 1
        INFIX = 2
    end
    
    properties (Access = private)
        type
        inst
        val
        op
        args
        dims = [1 1]
        indices = 1
        derived = false
        iterReference = false
        notation = Exp.NONOTATION
    end
    
    methods
        function e = Exp(v, varargin)
            % Exp - create an Exp simEngine expression
            %
            % Usage:
            %   E = Exp(ID) - create an expression from a string id
            %   E = Exp(LITERAL) - create an expression from a number
            %   E = Exp(ITER) - create an expression from an iterator
            %   E = Exp(SUBMODEL, ID) - create a submodel variable
            %   reference
            %
            % Description:
            %   The Exp constructor creates simEngine expressions from
            %   variables and literals.  In general, this constructor almost
            %   never has to be called directly, since expression data
            %   types are returned by calls to MODEL/STATE, MODEL/INPUT,
            %   etc.  The Exp constructor can be used to convert iterators
            %   into expressions so that iterators can be used as literal
            %   quantities in computations.
            %
            % Examples:
            %   x = Exp('x'); % define the 'x' variable
            %   (x+1)^2 % will return 'Exp: ((x+1)^2)'
            %
            %   % Use an iterator in computation
            %   t = Iterator('continuous', 'solver', 'ode45');
            %   v = x*Exp(t); % Compute velocity from a position and time
            %  
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            if nargin == 0
                e.val = '';
                e.type = e.NULLEXP;
            elseif nargin == 1
                e.val = v;
                if ischar(v)
                    e.type = e.VARIABLE;
                elseif isnumeric(v)
                    e.type = e.LITERAL;
                    e.dims = size(v);
                elseif islogical(v)
                    e.type = e.LITERAL;
                    e.dims = size(v);
                    e.val = v * 1; % promote logical to numeric
                elseif isa(v, 'Exp')
                    e = v;
                elseif isa(v, 'Iterator')
                    e.type = e.ITERATOR;
                    e.iterReference = v;
                    e.val = v.id;
                else
                    classname = class(v);
                    error('Simatra:Exp', 'Invalid type ''%s'' passed to Exp.  Must be either a string variable name, a numeric literal, an Exp type, or an Iterator.', classname);
                end
            elseif nargin == 2 && isa(v, 'Instance') && ischar(varargin{1})
              e.type = e.REFERENCE;
              e.inst = v.InstName;
              e.val = varargin{1};
              e.dims = size(v);
              e.indices = reshape(1:prod(e.dims), e.dims);
            else
                dims = [varargin{:}];
                if isscalar(dims)
                  dims = [dims dims];
                end
                if ischar(v) && isnumeric(dims)
                  if all(dims > 0)
                    if(strcmp(v, 'NULLEXP'))
                      e.type = e.NULLEXP
                      e.val = '';
                    else
                      e.type = e.VARIABLE;
                      e.val = v;
                    end
                    e.dims = dims;
                    e.indices = reshape(1:prod(dims),dims);
                  else
                    error('Simatra:Exp','Dimension extents must be > 0.');
                  end
                else
                  error('Simatra:Exp', 'When calling Exp with multiple arguments.  Last arguments must all be numeric dimensions.')
                end
            end
        end
        
        % Algebraic operations
        function er = plus(varargin)
            fcn = @(elem, init)(oper('+', {init, elem}));
            switch nargin
                case 0
                    error('Simatra:Exp:plus','Not enough input arguments');
                case 1
                    er = Exp(varargin{1}); % NON STANDARD BEHAVIOR
                case 2
                    er = oper('+', varargin);
                otherwise
                    er = List.foldl(fcn, varargin{1}, varargin(2:end));
            end
            %er = oper('+', {e1, e2});
        end
        
        function er = sum(varargin)
            er = plus(varargin{:});
        end
        
        function er = minus(e1, e2)
            er = oper('-', {e1, e2});
        end
        
        function er = uminus(e1)
            er = oper('-', {e1});
        end
        
        function er = uplus(e1)
           er = e1;
        end
        
        function er = times(varargin)
            fcn = @(elem, init)(oper('*', {init, elem}));
            switch nargin
                case 0
                    error('Simatra:Exp:times','Not enough input arguments');
                case 1
                    er = Exp(varargin{1}); % NON STANDARD BEHAVIOR
                case 2
                    er = oper('*', varargin);
                otherwise
                    er = List.foldl(fcn, varargin{1}, varargin(2:end));
            end
            %er = oper('*', {e1, e2});
        end
        
        function er = prod(varargin)
            er = times(varargin{:});
        end
        
        function er = mtimes(e1, e2)
        if length(size(e1)) > 2 || length(size(e1)) > 2
          error('Simatra:Exp:mtimes', 'Input arguments must be 2-D');
        elseif size(e1,2) ~= size(e2,1)
          if ~isequal(size(e1), [1 1]) && ~isequal(size(e2), [1 1])
            error('Simatra:Exp:mtimes', 'Inner matrix dimensions must agree.');
          end
        end
        if isequal(size(e1), [1 1]) || isequal(size(e2), [1 1])
          er = oper('*', {e1, e2});
        else
          er = oper('matrix_mul', {e1, e2}, Exp.PREFIX, [size(e1,1) size(e2,2)]);
        end
        end
        
        function er = rdivide(e1, e2)
            er = oper('/', {e1, e2});
        end

        function er = mrdivide(e1, e2)
        if ~isequal(size(e1), [1 1]) || ~isequal(size(e2), [1 1])
          error('Simatra:Exp:mrdivide', 'Not yet supported.');
        else
          er = oper('/', {e1, e2});
        end
        end

        function er = ldivide(e1, e2)
            er = oper('/', {e2, e1});
        end

        function er = mldivide(e1, e2)
        if ~isequal(size(e1), [1 1]) || ~isequal(size(e2), [1 1])
          error('Simatra:Exp:mldivide', 'Not yet supported.');
        else
          er = oper('/', {e2, e1});
        end
        end
        
        function er = power(e1, e2)
            er = oper('^', {e1, e2});
        end
        
        function er = mpower(e1, e2)
        if ~isequal(size(e1), [1 1]) || ~isequal(size(e2), [1 1])
          error('Simatra:Exp:mpower', 'Not yet supported.');
        else
          er = oper('^', {e1, e2});
        end
        end
        
        function er = mod(e1, e2)
            er = oper('%%', {e1, e2});
        end
        
        function er = floor(e1)
            er = oper('floor', {e1});
        end
        
        function er = ceil(e1)
            er = oper('ceil', {e1});
        end
        
        function er = round(e1)
            er = oper('round', {e1});
        end
        
        % Relational
        function er = gt(e1, e2)
            er = oper('>', {e1, e2});
        end

        function er = lt(e1, e2)
            er = oper('<', {e1, e2});
        end
        
        function er = ge(e1, e2)
            er = oper('>=', {e1, e2});
        end

        function er = le(e1, e2)
            er = oper('<=', {e1, e2});
        end
        
        function er = eq(e1, e2)
            er = oper('==', {e1, e2});
        end
         
        function er = ne(e1, e2)
            er = oper('<>', {e1, e2});
        end
        
        % Logical
        function er = and(varargin)
            fcn = @(elem, init)(oper(' and ', {init, elem}));
            switch nargin
                case 0
                    error('Simatra:Exp:and','Not enough input arguments');
                case 1
                    er = Exp(varargin{1}); % NON STANDARD BEHAVIOR
                case 2
                    er = oper(' and ', varargin);
                otherwise
                    er = List.foldl(fcn, varargin{1}, varargin(2:end));
            end
            %er = oper(' and ', {e1, e2});
        end
        
        function er = all(varargin)
            er = and(varargin{:});
        end
        
        function er = or(varargin)
            fcn = @(elem, init)(oper(' or ', {init, elem}));
            switch nargin
                case 0
                    error('Simatra:Exp:or','Not enough input arguments');
                case 1
                    er = Exp(varargin{1}); % NON STANDARD BEHAVIOR
                case 2
                    er = oper(' or ', varargin);
                otherwise
                    er = List.foldl(fcn, varargin{1}, varargin(2:end));
            end
            %er = oper(' or ', {e1, e2});
        end

        function er = any(varargin)
            er = or(varargin{:});
        end
        
        function er = not(e1)
            er = oper('not', {e1});
        end

        % Functions
        function er = exp(e1)
            er = oper('exp', {e1});
        end
        function er = log(e1)
            er = oper('ln', {e1});
        end
        function er = log10(e1)
            er = oper('log10', {e1});
        end
        function er = sqrt(e1)
            er = oper('sqrt', {e1});
        end
        function er = abs(e1)
            er = oper('abs', {e1});
        end
        
        % Trig functions
        function er = sin(e1)
            er = oper('sin', {e1});
        end
        function er = cos(e1)
            er = oper('cos', {e1});
        end
        function er = tan(e1)
            er = oper('tan', {e1});
        end
        function er = csc(e1)
            er = oper('csc', {e1});
        end
        function er = sec(e1)
            er = oper('sec', {e1});
        end
        function er = cot(e1)
            er = oper('cot', {e1});
        end
        function er = asin(e1)
            er = oper('asin', {e1});
        end
        function er = acos(e1)
            er = oper('acos', {e1});
        end
        function er = atan(e1)
            er = oper('atan', {e1});
        end
        function er = atan2(e1, e2)
            er = oper('atan2', {e1, e2}, Exp.PREFIX);
        end
        function er = acsc(e1)
            er = oper('acsc', {e1});
        end
        function er = asec(e1)
            er = oper('asec', {e1});
        end
        function er = acot(e1)
            er = oper('acot', {e1});
        end
        function er = sinh(e1)
            er = oper('sinh', {e1});
        end
        function er = cosh(e1)
            er = oper('cosh', {e1});
        end
        function er = tanh(e1)
            er = oper('tanh', {e1});
        end
        function er = csch(e1)
            er = oper('csch', {e1});
        end
        function er = sech(e1)
            er = oper('sech', {e1});
        end
        function er = coth(e1)
            er = oper('coth', {e1});
        end
        function er = asinh(e1)
            er = oper('asinh', {e1});
        end
        function er = acosh(e1)
            er = oper('acosh', {e1});
        end
        function er = atanh(e1)
            er = oper('atanh', {e1});
        end
        function er = acsch(e1)
            er = oper('acsch', {e1});
        end
        function er = asech(e1)
            er = oper('asech', {e1});
        end
        function er = acoth(e1)
            er = oper('acoth', {e1});
        end
        
        % Piecewise functions
        function er = piecewise(varargin)            
            % Piecewise - functional conditional operator
            %
            % Usage:
            %   RESULT = Piecewise(VALUE1, CONDITION1, [VALUE2, CONDITION2, [...]], OTHERWISE)  
            %
            % Description:
            %   The piecewise operator allows conditional expressions to be 
            %   embedded inline within a series of operations.  This is the
            %   only way to provide IF type functionality to simEngine
            %   since the MATLAB model framework can not see IF statements
            %   in the code.  All IF statements in MATLAB are evaluated at
            %   compile time.  The Piecewise operator, in contrast, is
            %   evaluated at simulation time.
            %  
            % Examples:
            %   % max of two value
            %   max_value = piecewise(x, x>y, y);
            %   % heavywise step function
            %   step = piecewise(1, t>0, 0);
            %
            % See also PIECEWISE
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            if nargin == 1 && iscell(varargin{1})
                args = varargin{1};
            else
                args = varargin;
            end
            isOdd = mod(length(args), 2) == 1;
            if ~isOdd
                error('Simatra:Exp:piecewise', 'Pieceswise expects an odd number of Expression arguments')
            end
            maxsize = [1 1];
            for i = 1:numel(varargin)
              argsize = size(varargin{i});
              if ~isequal(argsize, [1 1])
                if isequal(maxsize, [1 1])
                  maxsize = argsize;
                elseif ~isequal(argsize, maxsize)
                  error('Simatra:Exp:piecewise', 'All arguments to piecewise must be equal dimensions or have dimensions [1 1]. (%s ~= %s)', mat2str(maxsize), mat2str(argsize));
                end
              end
            end
            er = oper('piecewise', varargin);
        end
        
        % Compound functions (TODO - make these have arbitrary numbers of
        % arguments)
        function er = max(e1, e2)
            er = piecewise(e1, e1 > e2, e2);
            % Can't uncomment this since we can't efficiently model this in
            % MATLAB - this exponentially explodes in size
%             function r = maxfcn(a,b)
%                 if ~isa(a, 'Exp')
%                     a = Exp(a);
%                 end
%                 if ~isa(b, 'Exp')
%                     b = Exp(b);
%                 end
%                 r = piecewise(a, a>b, b);
%             end
%             switch nargin
%                 case 0
%                     error('Simatra:Exp:max','Not enough input arguments');
%                 case 1
%                     er = Exp(varargin{1}); % NON STANDARD BEHAVIOR
%                 case 2
%                     er = maxfcn(varargin{1},varargin{2});
%                 otherwise
%                     er = List.foldl(@maxfcn, varargin{1}, varargin(2:end));
%             end

        end
        function er = min(e1, e2)
            er = piecewise(e1, e1 < e2, e2);
        end        
        
        % speciality functions
        function er = conv(e1, vec)
            % Conv - Convolution operator
            %
            % Usage:
            %   RESULT = Conv(EXPVAR, VECTOR)
            %
            % Description:
            %   The convolution operator takes an Exp variable with a
            %   defined iterator and convolutes it over time with a vector.
            %   The first argument must be a variable with an iterator
            %   defined and a relative index of 0, for example x[n].  The
            %   second argument must be numeric.
            %  
            % Examples:
            %   % three point moving averager
            %   n = Iterator('n', 'discrete', 'sample_frequency', 44100);
            %   y = conv(x(n), (1/3)*[1 1 1]);
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            if ~isa(e1, 'Exp')
                error('Simatra:Exp:conv', 'First argument must be an expression type');
            end
            if ~isnumeric(vec) || size(vec,1) > 1 || isempty(vec)
                error('Simatra:Exp:conv', 'Second argument must be a non-empty numeric vector');
            end
            if ~isa(e1.iterReference, 'IteratorReference') || e1.iterReference.delay ~= 0
                error('Simatra:Exp:conv', 'First argument must have an iterator with delay 0 specified');
            end
            iter = e1.iterReference.iterator;
            if length(vec) == 1
                er = e1*vec;
            else
                products = cell(1,length(vec));
                for i=1:length(products)
                    prev_value = e1;
                    prev_value.iterReference = iter-(i-1);
                    products{i} = prev_value * vec(i);
                end
                er = plus(products{:});
            end
        end
        
        % for arrays
        function l = length(e1)
            l = max(e1.dims);
        end
        
        function s = size(e1, ind)
            if 1 == nargin
                s = e1.dims;
            else
                s = e1.dims(ind);
            end
        end
        
        function i = end(e1, k, n)
          if n == k
            i = prod(e1.dims(k:end));
          else
            i = e1.dims(k);
          end
        end
        
        function er = reshape(e, varargin)
        dims = [varargin{:}];
        switch e.type
         case e.LITERAL
          er = Exp(reshape(e.val, dims));
         case {e.VARIABLE, e.REFERENCE}
          er = e;
          er.indices = reshape(er.indices, dims);
          er.dims = dims;
          er.derived = true;
         case e.OPERATION
          if isequal(e.dims, [1 1])
            error('Simatra:Exp:reshape', 'Cannot use reshape on an Exp that has only one element.');
          else
            er = e;
            for i = 1:numel(er.args)
              if ~isequal(er.args{i}.dims, [1 1])
                er.args{i} = reshape(er.args{i}, dims);
              end
            end
          end
         case e.ITERATOR
          error('Reshape of an Iterator Exp is not allowed.');
        end
        end
        
        function str = toId(e)
            str = regexprep(toStr(e),'\.','__');
        end
        
        function b = isRef(e)
            b = (e.type == e.REFERENCE);
        end
        
        function [er, constStruct] = subsref(e, s)
        constStruct = struct();
          if strcmp(s(1).type, '.')
            if strcmp(s(1).subs, 'toStr')
              er = toStr(e);
            elseif strcmp(s(1).subs, 'toDslStr')
              [er, constStruct] = toDslStr(e,s(2).subs);
              s = [];
            elseif strcmp(s(1).subs, 'toMatStr')
              er = toMatStr(e);
            else
              error('Simatra:Exp:subsref', 'Invalid reference ''%s'' to Exp.', s(1).subs);
            end
          elseif strcmp(s(1).type, '()')
            ss = s(1);
            switch e.type
             case {e.VARIABLE, e.REFERENCE}
              if isa(ss.subs, 'Iterator') || isa(ss.subs, 'IteratorReference')
                er = e;
                er.iterReference = ss.subs.toReference;
              elseif isa(ss.subs{1}, 'Iterator') || isa(ss.subs{1}, 'IteratorReference')
                iref = ss.subs{1}.toReference;
                if length(ss.subs) > 1
                  ss.subs = ss.subs(2:end);
                  er = Exp(subsref(e.val, ss));
                else
                  er = e;
                end
                er.iterReference = iref;
              else
                temp = subsref(e.indices, ss);
                er = Exp(e.val, size(temp));
                er.type = e.type;
                er.inst = e.inst;
                er.indices = temp;
                er.derived = true;
              end
             case e.LITERAL,
              %if isa(ss.subs, 'Iterator') || isa(ss.subs, 'IteratorReference')
              %  er = e;
              %  er.iterReference = ss.subs.toReference;
              %elseif isa(ss.subs{1}, 'Iterator') || isa(ss.subs{1}, 'IteratorReference')
              %  ss.subs = ss.subs(2:end);
              %  er = Exp(subsref(e.val, ss));
              %  er.iterReference = ss.subs{1}.toReference;
              %else
                er = Exp(subsref(e.val, ss));
              %end
             case e.ITERATOR,
              error('Simatra:Exp:subsref', 'Can not index Exp.ITERATOR.');
             case e.OPERATION,
              if isa(ss.subs, 'Iterator') || isa(ss.subs, 'IteratorReference')
                er = e;
                er.iterReference = ss.subs.toReference;
              else
                args = cell(size(e.args));
                if isa(ss.subs{1}, 'Iterator') || isa(ss.subs{1}, 'IteratorReference')
                  hasiref = true;
                  iref = ss.subs{1}.toReference;
                  ss.subs = ss.subs(2:end);
                else
                  hasiref = false;
                end
                % Test for piecewise op for pruning NULLEXP
                checkpieces = strcmp(e.op, 'piecewise');
                for i = 1:numel(e.args)
                  if ~isequal(e.dims, [1 1]) && isequal(e.args{i}.dims, [1 1])
                    % If the operation Exp is multidimensional but
                    % an argument Exp is singular, pass on the
                    % singular argument.  This is valid.
                    args{i} = e.args{i};
                  else
                    % Otherwise, pass on the subsref to the argument
                    args{i} = Exp(subsref(e.args{i}, ss));
                  end
                  % If a subsref produces an argument that is NULLEXP, the entire result is NULLEXP
                  if ~checkpieces && args{i}.type == e.NULLEXP
                    er = Exp();
                    return;
                  end
                end
                if checkpieces
                  for i = 2:2:numel(args)
                    if args{i}.type == e.LITERAL
                      % Prune any unused arguments
                      if isequal(args{i}.val, ones(args{i}.dims))
                        args = args{1:i-1};
                        i = 2; % Restart check
                      elseif isequal(args{i}.val, zeros(args{i}.dims))
                        args = args{i+1:end};
                        i = 2; % Restart check
                      end
                    end
                  end
                  if numel(args) == 1
                    er = args;
                    return;
                  end
                end
                
                er = oper(e.op, args, e.notation);
                if hasiref
                  er.iterReference = iref;
                end
              end
            end
            else
            error('Simatra:Exp:subsref', 'Subsref type %s not supported', s(i).type);
          end
          % Recursively call subsref for additional references
          if length(s) > 1
            er = subsref(er, s(2:end));
          end
        end

        
        % Expression Processing
        % =======================================================
        
        function syms = exp_to_symbols(e)
            if ~isa(e, 'Exp')
                class(e)
                e
                error('Simatra:Exp:exp_to_symbols', 'Argument is not an expression');
            elseif isempty(e.type)
                e.val
                error('Simatra:Exp:exp_to_symbols', 'Expression type is empty');                
            end
            
            
            switch e.type
                case {e.VARIABLE, e.ITERATOR}
                    syms = {e.val};
                case e.LITERAL
                    syms = {};
                case e.REFERENCE
                    syms = {}; % ignore this since this is a different scope
                case e.OPERATION
                    syms = {};
                    arguments = e.args;
                    for i=1:length(arguments)
                        syms = [syms exp_to_symbols(arguments{i})];
                    end
            end
            syms = unique(syms);
        end
        
        
        function i = toIterReference(e)
        i = e.iterReference;
        end
        
        function iters = findIterators(e, map)
            if 1 == nargin
                map = containers.Map;
            end
            
            if isempty(e.type)
                e
                error('Simatra:Exp:findIterators', 'Unexpected empty expression type')
            end
            switch e.type
                case {e.VARIABLE, e.REFERENCE}
                    if isa(e.iterReference, 'IteratorReference')
                        iter = e.iterReference.iterator;
                        map(iter.id) = iter;
                    end
                case e.ITERATOR
                    map(e.val) = e.iterReference;
                case e.OPERATION
                    arguments = e.args;
                    for i=1:length(arguments)
                        a = arguments{i};
                        if isa(a, 'Exp')
                            map = findIterators(a, map);
                        end
                    end
            end
            iters = map;            
        end
        
        
        % Display functions
        % =======================================================
        
        function s = char(e)
            s = toStr(e);
        end
        
        function s = toStr(e)
        error('Simatra:Exp:toStr', 'Please call either toMatStr or toDslStr instead of toStr.');
        %s = toDslStr(e);
        end
        
        function s = toMatStr(e)
        s = '';
            if isempty(e.type)
                e.val
                error('Simatra:Exp:toMatStr', 'Unexpected empty expression type')
            end

            switch e.type
                case e.VARIABLE
                    s = e.val;
                    if e.derived
                      if numel(e.indices) > 1
                        s = ['reshape(' e.val '(' mat2str(reshape(e.indices, 1, numel(e.indices))) '),' mat2str(e.dims) ')'];
                      else
                        s = [e.val '(' num2str(e.indices) ')'];
                      end
                    else
                      s = e.val;
                    end
                    if isa(e.iterReference, 'IteratorReference')
                        s = [s '[' toStr(e.iterReference) ']'];
                        %error('Iterator references to Exp not supported in Matlab.')
                    end
               case e.REFERENCE
                    if e.derived
                      error('Simatra:Exp:toMatStr','Unhandled derived Exp for Exp.REFERENCE.');
                    else
                      s = [e.inst '.' e.val];
                    end
                    if isa(e.iterReference, 'IteratorReference')
                        s = [s '[' e.iterReference.toStr ']'];
                    end
                case e.ITERATOR
                    s = e.val;
                case e.LITERAL
                    if length(size(e.val)) > 2
                        s = ['reshape(' mat2str(reshape(e.val,1, numel(e.val)),17) ',' mat2str(size(e.val)) ')'];
                    else
                        s = mat2str(e.val,17);
                    end
                case e.OPERATION
                    arguments = e.args;
                    if strcmp(e.op, 'piecewise')
                        if length(arguments) == 1
                            s = toMatStr(arguments{1});
                        else
                            s = 'piecewise(';
                            for i=1:2:(length(arguments)-1);
                                s = [s toMatStr(arguments{i}) ', ' toMatStr(arguments{i+1}) ', '];
                            end
                            s = [s  toMatStr(arguments{end}) ')'];
                        end
                    else
                        if length(arguments) == 1
                            s = ['(' e.op '(' toMatStr(arguments{1}) '))'];
                        elseif length(arguments) == 2
                            if e.notation == Exp.INFIX
                                s = ['(' toMatStr(arguments{1}) e.op toMatStr(arguments{2}) ')'];
                            else
                                % treat by default as Exp.PREFIX
                                s = ['(' e.op '(' toMatStr(arguments{1}) ', ' toMatStr(arguments{2}) '))'];
                            end
                        end
                    end
            end
        end

        function [s, constStruct] = toDslStr(e, constStruct)
            if isempty(e.type)
                e.val
                error('Simatra:Exp:toDslStr', 'Unexpected empty expression type')
            end
            
            %if ~isequal(e.dims, [1 1])
            %  error('Simatra:Exp:toDslStr', 'Multidimensional elements are not supported in Diesel. \n%s', toMatStr(e))
            %end

            switch e.type
             case e.VARIABLE
              if e.derived == true
                error('Simatra:Exp:toDslStr', 'Not yet implemented subsref on variable');
                %s = [e.val '_' num2str(e.indices)]; % Flatten the variable name with its index
              else
                s = e.val;
              end
              if isa(e.iterReference, 'IteratorReference')
                s = [s '[' e.iterReference.toStr ']'];
              end
             case e.REFERENCE
              if e.derived == true
                error('Simatra:Exp:toDslStr', 'Not yet implemented subsref on reference');
                %s = [e.inst '_' num2str(e.indices) '.' e.val];
              else
                s = [e.inst '.' e.val];
              end
              if isa(e.iterReference, 'IteratorReference')
                s = [s '[' e.iterReference.toStr ']'];
              end
             case e.ITERATOR
              s = e.val;
             case e.LITERAL
              if isequal(e.dims, [1 1])
                if isreal(e.val)
                  s = mat2str(e.val,17);
                else
                  s = ['complex(' mat2str(real(e.val),17) ', ' mat2str(imag(e.val),17) ')'];
                end 
              else
                cname = ['const_' num2str(length(fieldnames(constStruct)))];
                constStruct.(cname) = e.val;
                s = ['&"constants.pb":' cname ' @(tensor ' mat2str(size(e.val)) ')'];
              end
             case e.OPERATION
                    arguments = e.args;
                    if strcmp(e.op, 'piecewise')
                        if length(arguments) == 1
                            [s, constStruct] = toDslStr(arguments{1}, constStruct);
                        else
                            s = '{';
                            for i=1:2:(length(arguments)-1);
                              [s1, constStruct] = toDslStr(arguments{i}, constStruct);
                              [s2, constStruct] = toDslStr(arguments{i+1}, constStruct);
                                s = [s  s1 ' when ' s2 ', '];
                            end
                            [s1, constStruct] = toDslStr(arguments{end}, constStruct)
                            s = [s  s1 ' otherwise}'];
                        end
                    else
                        if length(arguments) == 1
                          [s1, constStruct] = toDslStr(arguments{1}, constStruct);
                          s = ['(' e.op '(' s1 '))'];
                        elseif length(arguments) == 2
                          [s1, constStruct] = toDslStr(arguments{1}, constStruct);
                          [s2, constStruct] = toDslStr(arguments{2}, constStruct);
                          if e.notation == Exp.INFIX
                            s = ['(' s1 e.op s2 ')'];
                          else
                            % treat by default as Exp.PREFIX
                            s = ['(' e.op '(' s1 ', ' s2 '))'];
                          end
                        end
                    end
            end
        end

        function s = toVariableName(e)
            if isempty(e.type)
                e.val
                error('Simatra:Exp:toVariableName', 'Unexpected empty expression type')
            end

            switch e.type
                case e.VARIABLE
                    s = e.val;
                otherwise
                    error('Simatra:Exp:toVariableName', ['Expression ' toStr(e) ' is not a variable'])
            end
        end
            
        
        function disp(e)
            disp(['Expression: ' toMatStr(e)]);
        end
    end
    
    
    
    
end

function er = oper(operation, args, infix, optdims)
len = length(args);
exps = cell(1,len);
for i=1:len
    exps{i} = Exp(args{i});
end
er = Exp;
if nargin == 2
    er.notation = Exp.INFIX;
else
    if infix == Exp.INFIX;
        er.notation = Exp.INFIX;
    else
        er.notation = Exp.PREFIX;
    end
end

% check binary operations
if len == 2 && nargin < 4
    size1 = size(args{1});
    size2 = size(args{2});
    if ~isequal(size1, [1 1]) && ~isequal(size2, [1 1]) && ~isequal(size1, size2)
        error('Simatra:Exp', 'Invalid expression dimensions %s and %s', mat2str(size1), mat2str(size2));
    end
    if isequal(size1, [1 1])
        er.dims = size2;
    else
        er.dims = size1;
    end
end
er.type = er.OPERATION;
er.op = operation;
er.args = exps;
% Override result dimensions for operation (e.g. Matrix * Vector multiplication)
if nargin == 4
  er.dims = optdims;
end
end

