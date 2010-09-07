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
        
        function n = NMODL(varargin)
            
            if nargin == 1
                channels = varargin{1};
                % grab the name of the channel and look for it on the path or
                % just use it as a function handle
                if isa(channels, 'function_handle')
                    fhandle = channels;
                    name = func2str(channels);
                elseif ischar(channels) && exist(channels, 'file') == 2
                    fhandle = str2func(channels);
                    name = channels;
                else
                    error('Simatra:NEURON:Section:insert', 'Invalid insert statement - must specify either a function handle or string representing the MATLAB file with the channel is described.');
                end
                
                % execute the function by creating an NMODL object and
                % passing it into the function
                
                % first, do a preliminary check of the channel file -
                % expecting one argument that is an NMODL object, and
                % no output arguments
                if nargin(fhandle) ~= 1
                    error('Simatra:NEURON:Section:insert', 'The function passed to insert requires one input argument which will be an object of class NMODL');
                end
                if nargout(fhandle) ~= 0
                    error('Simatra:NEURON:Section:insert', 'The function passed to insert requires there to be no output arguments.  All changes are reflected in the input model argument.');
                end
                
                % instantiate the NMODL with the default explicit
                % solver
                iter = Iterator('t_exp', 'continuous','solver','forwardeuler','dt',0.01);
                
            elseif nargin ~= 2
                error('Simatra:NMODL', 'Two arguments expected, a name and a default iterator');
            else
                name = varargin{1};
                iter = varargin{2};
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
            
            % if it was called with a function handle argument, evaluate
            % that function now
            if nargin == 1
                feval(fhandle, n);
                vramp(n);
            end
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
        
        function vramp(n)
            currents = keys(n.current_vars);
            states = keys(n.States);
            for i=1:length(states)
                n.output(states{i},Exp(states{i}));
            end

            v_start = -90; v_end = 0;
            L = 18.8; diam = 18.8;
            area = 4*pi*(diam/2)^2;
            celsius = 6.3;
            ena = 50; ek = -77;
            
            % create top level model for simulation
            m = Model('vramp', n.DefaultIterator);
            v = m.state(v_start);
            ramp_val = m.input('ramp');
            m.diffequ(v, ramp_val);
            sm = m.submodel(n);
            
            % go through each of the inputs and define them with canonical value if they are not
            % already defined
            inps = keys(n.Inputs);
            for i=1:length(inps)
                inp = n.Inputs(inps{i});
                if ~isfinite(inp.default)
                    switch(inps{i})
                        case 'v'
                            sm.v = v;
                        case 'v0'
                            sm.v0 = v_start;
                        case 'area'
                            sm.area = area;
                        case 'diam'
                            sm.diam = diam;
                        case 'L'
                            sm.L = L;
                        case 'celsius'
                            sm.celsius = celsius;
                        case 'ena'
                            sm.ena = ena;
                        case 'ek'
                            sm.ek = ek;
                        case 'dt'
                            sm.dt = n.DefaultIterator.dt;
                        otherwise
                            error('Simatra:NEURON:NMODL:vramp', 'Unrecognized input %s, please add a default value to the model definition', inps{i});
                    end
                end
            end
            
            % now pull out the currents
            for i=1:length(currents)
                m.output(currents{i}, 1e6*area*sm.(currents{i}));
            end
            
            % pull out the states as well
            for i=1:length(states)
                m.output(states{i}, sm.(states{i}));
            end            
            
            function out = run_sim(ramp, count, text)
                % run the simulation
                tstop = (v_end-v_start)/ramp;
                out = m.simex(tstop, struct('ramp', ramp));
                
                % rescale the time to match voltage
                vars = fieldnames(out);
                for i=1:length(vars)
                    v = out.(vars{i});
                    max_t = v(end,1);
                    len = length(v(:,1));
                    x = v_start:(v_end-v_start)/(len-1):v_end;
                    v(:,1) = x;
                    if any(strcmp(currents, vars{i}))
                        v(:,2:end) = 1e-9*v(:,2:end);
                    end
                    out.(vars{i}) = v;
                end
                cur_traces = struct();
                state_traces = struct();
                for i=1:length(vars)
                    if any(strcmp(currents, vars{i}))
                        cur_traces.(vars{i}) = out.(vars{i});
                    else
                        state_traces.(vars{i}) = out.(vars{i});
                    end
                end
                has_states = ~isempty(fieldnames(state_traces));
                
                if ~has_states
                    subplot(1,2,count);
                else
                    subplot(2,2,(count-1)*2+1);
                end
                cur_names = fieldnames(cur_traces);
                simplot(cur_traces)
                xlabel('voltage (mV)');
                ylabel('current (nA)');
                legend(cur_names{:});
                title(sprintf('%s voltage ramp - %g mV/ms (tstop=%g ms)', text, ramp, tstop));
                if has_states
                    subplot(2,2,(count-1)*2+2);
                    simplot(state_traces);
                    xlabel('voltage (mV)');
                    state_names = fieldnames(state_traces);
                    legend(state_names{:});
                    title(sprintf('%s voltage ramp - %g mV/ms (tstop=%g ms)', text, ramp, tstop));
                end
            end
            
            run_sim(10, 1, 'Fast');
            run_sim(0.3, 2, 'Slow');
        end
        
    end
    
end