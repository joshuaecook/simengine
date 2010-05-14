classdef Exp
    
    properties (Constant, Access = private)
        NULLEXP = 0
        VARIABLE = 1
        LITERAL = 2
        OPERATION = 3
        REFERENCE = 4
    end
    
    properties (Access = private)
        type
        val
        op
        args
    end
    
    methods
        function e = Exp(v, sub)
            if nargin == 0
                e.val = '';
                e.type = e.NULLEXP;
            elseif nargin == 1
                e.val = v;
                if isstr(v)
                    e.type = e.VARIABLE;
                elseif isnumeric(v)
                    e.type = e.LITERAL;
                elseif isa(v, 'Exp')
                    e = v;
                end
            elseif nargin == 2
                e.val = [v '.' sub];
                e.type = e.REFERENCE;
            end
        end
        
        % Algebraic operations
        function er = plus(e1, e2)
            er = oper('+', {e1, e2});
        end
        
        function er = minus(e1, e2)
            er = oper('-', {e1, e2});
        end
        
        function er = uminus(e1)
            er = oper('-', {e1});
        end
        
        function er = times(e1, e2)
            er = oper('*', {e1, e2});
        end
        
        function er = mtimes(e1, e2)
            er = oper('*', {e1, e2});
        end
        
        function er = rdivide(e1, e2)
            er = oper('/', {e1, e2});
        end

        function er = mrdivide(e1, e2)
            er = oper('/', {e1, e2});
        end

        function er = power(e1, e2)
            er = oper('^', {e1, e2});
        end
        
        function er = mpower(e1, e2)
            er = oper('^', {e1, e2});
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
        
        function str = toId(e)
            str = regexprep(toStr(e),'\.','__');
        end
        
        function b = isRef(e)
            b = (e.type == e.REFERENCE);
        end
        
        function s = toStr(e)
            switch e.type
                case e.VARIABLE
                    s = e.val;
                case e.REFERENCE
                    s = e.val;
                case e.LITERAL
                    s = num2str(e.val);
                case e.OPERATION
                    if length(e.args) == 1
                        s = ['(' e.op '(' toStr(e.args(1)) '))'];
                    elseif length(e.args) == 2
                        s = ['(' toStr(e.args(1)) e.op toStr(e.args(2)) ')'];
                    end
            end
        end
        function disp(e)
            disp(['Exp: ' toStr(e)]);
        end
    end
    
    
    
    
end

function er = oper(operation, args)
for i=1:length(args)
    exps(i) = Exp(args{i});
end
er = Exp;
er.type = er.OPERATION;
er.op = operation;
er.args = exps;
end
