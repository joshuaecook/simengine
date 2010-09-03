classdef IClamp < Model
   
    properties
        dt = 0.01
        del = 0
        dur = 0
        amp = 0
    end
    
    properties (Access = protected)
        t_exp
    end
    
    methods
        function m = IClamp(t_exp)
            [path, file, ext] = fileparts(tempname);
            
            % create a unique name
            id = ['IClamp_' file];
            
            % construct the model object
            m@Model(id);

            if 0 == nargin
                % create an iterator
                t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', m.dt);
            end
            
            m.DefaultIterator = t_exp;
            m.t_exp = t_exp;
            
            % build now, but we'll build again when we produce DSL code
            build(m);
        end
        
        function s = toStr(m)
            initializeModel(m);
            build(m); % build the model
            s = toStr@Model(m); % now, create the string representation
        end
        
    end
    
    methods (Access = protected)
        function build(m)
            equ = piecewise(m.amp, (m.time > m.del) & (m.time <= m.del + m.dur), 0);
            m.output('I', equ);
        end
    end
    
end