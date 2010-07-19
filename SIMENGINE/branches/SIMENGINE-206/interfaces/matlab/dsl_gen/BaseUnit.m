classdef BaseUnit < GenericUnit
    
    properties
    end
    
    methods
        function u = BaseUnit(name, id)
            u.name = name;
            u.id = id; 
            u.data = struct('unit', u, 'prefix', 1, 'exp', 1);
        end
        
        function disp(u)
            disp(sprintf('Base Unit: %s (%s)', u.name, u.id))
        end
    end
    
    methods (Access = protected)
        
    end
    
end