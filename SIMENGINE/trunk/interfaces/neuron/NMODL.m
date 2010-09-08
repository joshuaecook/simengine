classdef NMODL < Model
% NMODL - create ion channels and other cellular mechanisms for insertion
% into Neuron sections
% 
% NMODL Methods:
%   NMODL - construct and visualize NMODL files
%   range_parameter - define a range variable accessible to the segment
%   global_parameter - define a global variable accessible to the section
%   current - create an output current quantity
%
% NMODL Properties:
%   v       - voltage potential in the segment    (mV)
%   v0      - initial segment voltage             (mV)
%   area    - surface area of the segment         (um^2)
%   celsius - temperature of the cell             (C)
%   diam    - diameter of the segment             (um)
%   L       - length of the segment               (um)
%   suffix  - string appended to range variables
%
%  The Neuron syntax and semantics are based on the NEURON Simulator by
%  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
%  
%  Copyright (c) 2010 Simatra Modeling Technologies
%
%  Permission is hereby granted, free of charge, to any person obtaining a copy
%  of this software and associated documentation files (the "Software"), to deal
%  in the Software without restriction, including without limitation the rights
%  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%  copies of the Software, and to permit persons to whom the Software is
%  furnished to do so, subject to the following conditions:
% 
%  The above copyright notice and this permission notice shall be included in
%  all copies or substantial portions of the Software.
% 
%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%  THE SOFTWARE.

    properties (Access = public)
        v       % voltage potential in the segment    (mV)
        v0      % initial segment voltage             (mV)
        area    % surface area of the segment         (um^2)
        celsius % temperature of the cell             (C)
        diam    % diameter of the segment             (um)
        L       % length of the segment               (um)
    end
    
    properties (Access = public)
        suffix = '' % string appended to range variables
    end
    
    properties (Access = protected)
        global_vars
        range_vars
        current_vars
    end
    
    methods (Access = public)
        
        function n = NMODL(varargin)
            % NMODL - load a MOD function and characterize it through
            % voltage ramps
            %
            % Usage:
            %   NMODL(MOD) - verify and simulate the MOD file through a
            %   voltage clamp simulation
            %
            % Examples:
            %   NMODL('pas')
            %   NMODL('hh')
            %
            % See also Section/insert
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %            

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
            n.v0 = n.input('v0');
            n.area = n.input('area');
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
            % global_parameter - create a global parameter assigned to a
            % Neuron section
            %
            % Usage:
            %   p = global_parameter(name [, default]) - create a parameter
            %   with name and optional default variable.  When inserted
            %   into a section, it will be accessible as section_obj.name.
            %   The return value p is a Exp type.
            %
            % Description:
            %   A global_parameter is an extended version of a Model/input.
            %   It is defined by the calling model, which in this case is
            %   the Neuron Section.  As a global parameter, it is
            %   accessible to be read and written as a property of the
            %   Section object.
            %
            % Examples:
            %   ena = mdl.global_parameter('ena');
            %   ek = mdl.global_parameter('ek', -77);
            %
            % See also Section NMODL/range_parameter
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %            
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
            % range_parameter - creates a range parameter assigned to a
            % Neuron segment
            %
            % Usage:
            %   p = range_parameter(name [, default]) - create a parameter
            %   with name and optional default variable.  When inserted
            %   into a section, it will be accessible as section_obj.name_suffix.
            %   The return value p is a Exp type.
            %
            % Description:
            %   A range__parameter is an extended version of a Model/input.
            %   It is defined by the calling model, which in this case is
            %   the Neuron Section.  As a range parameter, it is
            %   accessible to be read and written as a property of the
            %   Section object.
            %
            % Examples:
            %   mdl.suffix = 'hh';
            %   gnabar = mdl.range_parameter('gnabar', 0.12);
            %   gkbar = mdl.range_parameter('gkbar', 0.036);
            %   % with section called soma
            %   soma.insert('hh');
            %   soma.gnabar_hh = soma.gnabar_hh*2;
            %
            % Known limitations:
            %   A range_parameter at this time can not be specified with a
            %   different value per segment.  There can only be one value
            %   per section.  In the future, this library will support
            %   syntax incorporating a position as shown below:
            %   soma.gnabar_hh(0) = soma.gnabar_hh/2;
            %   soma.gnabar_hh(0.5) = soma.gnabar_hh;
            %   soma.gnabar_hh(1) = 2*soma.gnabar_hh;
            %
            % See also Section NMODL/global_parameter
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %            
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
            % current - creates a current output 
            %
            % Usage:
            %   current(var [, ion]) - creates a current output based on
            %   the option ion specified
            %
            % Description:
            %   The output of an NMODL channel can only be a current which
            %   is then used in the computation of the membrane potential
            %   per segment.
            %
            % Examples:
            %   i = g*(mdl.v-e); % create a generic leakage current
            %   current(i);
            %   current(ina, 'na'); % assign the current ina to the 'na' ion.
            %
            % Known limitations:
            %   The ion value is currently ignored.  In the future, this
            %   can be used to keep track of, for example, Ca2+
            %   concentration in the segment.
            %
            % See also Section
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %            
            
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