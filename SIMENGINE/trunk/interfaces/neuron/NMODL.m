classdef NMODL < Model
   
    properties (Access = public)
        v
        v0
        area
        celsius
        diam
        L
    end
    
    properties (Access = public)
        suffix = ''
    end
    
    properties (Access = protected)
        global_vars
        range_vars
        current_vars
    end
    
    methods (Access = public)
        
        function n = NMODL(name, iter)
            if nargin ~= 2
                error('Simatra:NMODL', 'Two arguments expected');
            end
            n@Model(name, iter);
            n.suffix = name;
            n.global_vars = containers.Map;
            n.range_vars = containers.Map;
            n.current_vars = containers.Map;
            
            n.v = n.input('v');
            %n.global_vars('v') = n.v;
            n.v0 = n.input('v0');
            %n.global_vars('v0') = n.v0;
            n.area = n.input('area');
            %n.global_vars('area') = n.area;
            n.celsius = n.input('celsius');
            n.global_vars('celsius') = n.celsius;
            n.diam = n.input('diam');
            n.global_vars('diam') = n.diam;
            n.L = n.input('L');            
        end
        
        function p = global_parameter(n, name, default)
            if nargin == 2
                p = n.input(name);
            elseif nargin == 3
                p = n.input(name, default);
            else
                error('Simatra:NMODL:global_parameter', 'global_parameter method requires a parameter name and an optional default value');
            end
            n.global_vars(name) = p;
        end
        
        function p = range_parameter(n, name, default)
            if nargin == 2
                p = n.input(name);
            elseif nargin == 3
                p = n.input(name, default);
            else
                error('Simatra:NMODL:range_parameter', 'global_parameter method requires a parameter name and an optional default value');
            end
            n.range_vars(name) = p;
        end

        function current(n, out, ion)
            % pull out the name as a string
            if ischar(out)
                name = out;
                n.output(name);
            else
                name = inputname(2);
                n.output(name, out);
            end
            
            % just ignore the ion for right now....
            % n.ions(ion) = ...
            n.current_vars(name) = out;
        end
        
        function params = getParams(n)
            params = [];
            % first go through global parameters and then range parameters
            inps = keys(n.global_vars);
            for i=1:length(inps)
                inp = n.Inputs(inps{i});
                %if isfinite(inp.default)
                    s = struct('name', inps{i}, 'default', inp.default, 'range', false);
                    if isempty(params)
                        params = s;
                    else
                        params(end+1) = s;
                    end
                %end
            end
            inps = keys(n.range_vars);
            for i=1:length(inps)
                inp = n.Inputs(inps{i});
                %if isfinite(inp.default)
                    s = struct('name', inps{i}, 'default', inp.default, 'range', true);
                    if isempty(params)
                        params = s;
                    else
                        params(end+1) = s;
                    end
                %end
            end
            
        end
        
        function currents = getCurrents(n)
            currents = keys(n.current_vars);
        end
        
    end
    
end