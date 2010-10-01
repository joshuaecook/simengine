classdef Tag
   
    properties (Constant)
        TAG = 0
        EXPRESSION = 1
    end
    
    properties (Access = protected)
        name
        type
        op
        args
    end
    
    methods (Access = public)
        
        function t = Tag(value, args)
            if nargin == 1
                if ~ischar(value)
                    error('Simatra:Tag', 'Tag inputs can only be strings');
                end
                t.name = lower(value);
                t.type = Tag.TAG;
            else
                switch (value)
                    case {'~', '&', '|'}
                        t.op = value;
                        if ~iscell(args)
                            error('Simatra:Tag', 'Tag arguments must be a cell array');
                        end
                        t.args = args;
                        t.type = Tag.EXPRESSION;
                    otherwise
                        error('Simatra:Tag', 'Suitable operations are ~, &, and |');
                end
            end
        end
       
        function disp(t)
            disp(['Tag: ' toStr(t)])
        end
        
        function r = test(t, tag_list)
            tag_lower = List.map(@lower, tag_list);
            exists = @(tag)(any(strcmp(tag_lower, tag)));
            
            r = evaluate(t, exists);
        end
        
        function r = not(t)
            r = Tag('~', {t});
        end
        
        function r = and(t1, t2)
            r = Tag('&', {t1, t2});
        end
        
        function r = or(t1, t2)
            r = Tag('|', {t1, t2});
        end    
    end
        
    methods (Access = protected)
        
        function r = evaluate(t, exists)
            if t.type == Tag.TAG
                r = exists(t.name);
            else
                switch t.op
                    case '~'
                        r = not(evaluate(t.args{1}, exists));
                    case '&'
                        r = and(evaluate(t.args{1}, exists), evaluate(t.args{2}, exists));
                    case '|'
                        r = or(evaluate(t.args{1}, exists), evaluate(t.args{2}, exists));
                    otherwise
                        error('Simatra:Tag:evaluate', 'Suitable operations are ~, &, and |');
                end
            end
        end
        
        function s = toStr(t)
            if t.type == Tag.TAG
                s = t.name;
            else
                switch t.op
                    case '~'
                        s = ['(~' toStr(t.args{1}) ')'];
                    otherwise
                        s = ['(' toStr(t.args{1}) t.op toStr(t.args{2}) ')'];
                end
            end
        end
    end
    
end