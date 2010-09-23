classdef Units
   
    properties (Access = protected)
        units
    end
    
    methods
        function ul = Units()
            ul.units = containers.Map;
            ul.add(BaseUnit('dimensionless', '_'));
        end
        
        function add(ul, u)
            if isa(u, 'GenericUnit')
                if ul.units.isKey(u.id)
                    warning('Simatra:Units', 'Overriding definition of %s', ul.units(u.id).name);
                end
                ul.units(u.id) = u;
            end
        end
        
        function disp(ul)
            disp(' ');
            disp('Unit List:')
            values = ul.units.values;
            for i=1:length(values)
                fprintf(1, '  ');
                disp(values{i});

            end
            disp(' ');
            
        end

        function u = subsref(ul, i)
            if strcmp(i(1).type, '.')
                switch i(1).subs
                    case 'add'
                        add(ul, i(2).subs{1});
                    otherwise
                        error('Simatra:Units:subsref', 'Unexpected function call %s', i(1).subs);
                end
            elseif length(i) == 1 && strcmp(i(1).type, '()') && length(i.subs) == 1
                if ul.units.isKey(i.subs{1})
                    u = ul.units(i.subs{1});
                else
                    error('Simatra:Units:subsref', 'Unrecognized unit %s', i.subs{1});
                end
            else
                error('Simatra:Units:subsref', 'Unrecognized input format')
            end
        end
        
    end
    
    methods (Access = protected)
 
    end
    
end