classdef Section < Model
    
    properties (Access = public)
        dt = 0.01;
        nseg = 1
        diam = 500
        L = 100
        cm = 1
        Ra = 35.4
        ena = 50
        ek = -77
        celsius = 6.3
    end
    
    properties (Access = public)
        area
        unit_factor
    end
    
%     % for hh
%     properties (Dependent)
%         gnabar_hh
%         gkbar_hh
%         gl_hh
%         el_hh
%     end
%     
%     % for pas
%     properties (Dependent)
%         g_pas
%         e_pas
%     end
    
    % internal parameters
    properties (Access = private)
        hh_props = struct('gnabar', 0.12, 'gkbar', 0.036, 'gl', 0.0003, 'el', -54.3);
        pas_props = struct('g', 0.001, 'e', -70);
        params
        global_params
        params_map
        channels
    end
    
    properties (Access = protected)
        voltages = {};
        currents = {};
        pointCurrents = [];
        t_exp
        t_imp
        insert_pas = false;
        insert_hh = false;
    end
    
    properties (Access = public)
        connections = [];
        submodelInstance
    end
    
    methods (Access = public)
        function m = Section(id, t_imp, t_exp)
            m@Model(id, t_imp);
            m.t_imp = t_imp;
            m.t_exp = t_exp;
            m.params = containers.Map;
            m.global_params = containers.Map;
            m.params_map = containers.Map;
            m.channels = containers.Map;
        end
        
        function insert(m, channels)
            switch(channels)
%                 case 'hh'
%                     m.insert_hh = true;
%                     n = create_hh(m);
%                     addParams(m, n);
%                 case 'pas'
%                     m.insert_pas = true;
%                     n = create_pas(m);
%                     addParams(m, n);
                otherwise
                    if isa(channels, 'function_handle')
                        fhandle = channels;
                        fname = func2str(channels);
                    elseif ischar(channels) && exist(channels, 'file') == 2
                        fhandle = str2func(channels);
                        fname = channels;
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
                    n = NMODL(fname, m.t_exp);
                    feval(fhandle, n);
                    addParams(m, n);
                    m.channels(fname) = n;
            end
        end
        
        function addPointCurrent(m, mdl, pos)
            s = struct('model', mdl, 'I', 'I', 'pos', pos);
            if isempty(m.pointCurrents)
                m.pointCurrents = s;
            else
                m.pointCurrents(end+1) = s;
            end
        end
        
        
        function v_pos = v(m, pos)
            % function v_pos = V(pos)
            % Grab the voltage potential at a position
            if pos < 0 || pos > 1
                error('Simatra:Section:v', 'Position must be between zero and one');
            else
                v_pos = m.voltages{pos2ind(m.nseg, pos)};
            end
        end
        
        function addConnection(m, othersection, otherpos, pos)
            inputName = [othersection.Name '_v_' num2str(round(otherpos*1000))];
            s = struct('othersection', othersection, 'otherpos', otherpos, 'pos', pos, 'Vname', inputName);
            if isempty(m.connections)
                m.connections = s;
            else
                m.connections(end+1) = s;
            end
        end
        
        function s = toStr(m)
            initializeModel(m);
            build(m); % build the model
            s = toStr@Model(m); % now, create the string representation
        end

    end

    % Set/Get options
    methods
        
        function varargout = subsref(m, args)
            varargout = cell(1,nargout);
            for i=1:length(args)
                switch args(i).type
                    case '{}'
                        error('unexpected {} type');
                    case '()'
                        error('unexpected () type');
                    case '.'
                        %disp(sprintf('Finding %s', args(i).subs));
                        if any(strcmp(methods(m), args(i).subs))
                            if nargout > 0
                                [varargout{:}] = feval(args(i).subs, m, args(i+1).subs{:});
                            else
                                feval(args(i).subs, m, args(i+1).subs{:});
                            end
                        elseif any(strcmp(keys(m.params), args(i).subs))
                            p = m.params(args(i).subs);
                            varargout{1} = p.value;
                        elseif any(strcmp(keys(m.global_params), args(i).subs))
                            p = m.global_params(args(i).subs);
                            varargout{1} = p;
                        elseif any(strcmp(properties(m), args(i).subs))
                            varargout{1} = m.(args(i).subs);
                        else
                            warning('Simatra:NEURON:Section:subsref', ['Don''t recognize ''' args(i).subs ''''])
                        end
                        break;
                end
            end
        end

        
        function m = subsasgn(m, args, val)
            for i=1:length(args)
                switch args(i).type
                    case '{}'
                        error('unexpected {} type');
                    case '()'
                        error('unexpected () type');
                    case '.'
                        %disp(sprintf('Assigning %s', args(i).subs));
                        if any(strcmp(methods(m), args(i).subs))
                            error('Simatra:NEURON:Section:subsasgn', 'Can''t assign to method name %s', args(i).subs)
                        elseif any(strcmp(keys(m.params), args(i).subs))
                            p = m.params(args(i).subs);
                            p.value = val;
                            m.params(args(i).subs) = p;
                        elseif any(strcmp(keys(m.global_params), args(i).subs))
                            m.global_params(args(i).subs) = val;
                        elseif any(strcmp(properties(m), args(i).subs))
                            m.(args(i).subs) = val;
                        else
                            warning('Simatra:NEURON:Section:subsasgn', ['Don''t recognize ''' args(i).subs ''''])
                        end
                        break;
                end
            end
            
        end
                
        %
%         % HH set methods
%         function set.gnabar_hh(m, val)
%             if m.insert_hh
%                 m.hh_props.gnabar = val;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function set.gkbar_hh(m, val)
%             if m.insert_hh
%                 m.hh_props.gkbar = val;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function set.gl_hh(m, val)
%             if m.insert_hh
%                 m.hh_props.gl = val;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function set.el_hh(m, val)
%             if m.insert_hh
%                 m.hh_props.el = val;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         
%         % HH get methods
%         function val = get.gnabar_hh(m)
%             if m.insert_hh
%                 val = m.hh_props.gnabar;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function val = get.gkbar_hh(m)
%             if m.insert_hh
%                 val = m.hh_props.gkbar;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function val = get.gl_hh(m)
%             if m.insert_hh
%                 val = m.hh_props.gl;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
%         function val = get.el_hh(m)
%             if m.insert_hh
%                 val = m.hh_props.el;
%             else
%                 error('Simatra:NEURON:Section', 'HH channels not inserted');
%             end
%         end
% 
%         % PAS set methods
%         function set.g_pas(m, val)
%             if m.insert_pas
%                 m.pas_props.g = val;
%             else
%                 error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
%             end
%         end
%         function set.e_pas(m, val)
%             if m.insert_pas
%                 m.pas_props.e = val;
%             else
%                 error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
%             end
%         end
%         % PAS get methods
%         function val = get.g_pas(m)
%             if m.insert_pas
%                 val = m.pas_props.g;
%             else
%                 error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
%             end
%         end
%         function val = get.e_pas(m)
%             if m.insert_pas
%                 val = m.pas_props.e;
%             else
%                 error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
%             end
%         end

        
    end
    
    methods (Access = protected)
        
        function addParams(m, nmod)
            name = nmod.Name;
            parameters = nmod.getParams;
            suffix = nmod.suffix;
            c = cell(1,length(parameters));
            for i = 1:length(parameters)
                input = parameters(i).name;
                appended_name = [input '_' suffix];
                if parameters(i).range
                    m.params(appended_name) = struct('nmod', name, 'input', input, 'value', parameters(i).default);
                    c{i} = appended_name;
                else % then it is a global
                    if ~isKey(m.global_params, input)
                        if any(strcmp(properties(m), input))
                            m.global_params(input) = m.(input);
                        else
                            parameters(i).default
                            m.global_params(input) = parameters(i).default;
                        end
                    end
                    c{i} = input;
                end
            end
            m.params_map(name) = c;
        end
        

        
        function build(m)


            disp(['Building Section: ' m.Name]);
            % define the voltage states
            m.voltages = cell(1,m.nseg);
            v0 = -65;
            for i=1:m.nseg
               m.voltages{i} = m.state(-65, 'iter', m.t_imp);
            end
            
            % define surface areas
            if m.L == m.diam
                % use a sphere if the length and diameter were the same
                SAall_square_microns = 4*pi*(m.diam/2)^2;
            else
                SAall_square_microns = m.L*m.diam*pi; % um^2 (1e-6 ^ 2 = 1e-12)
                                                      % cm^2 (1e-2 ^ 2 = 1e-4)
            end
            SAall = SAall_square_microns * 1e-8;  % 1e-12 / 1e-4 = 1e-8
            m.area = SAall/m.nseg; % cm^2
            m.unit_factor = 1e6; % Conductance (S -> uS)
            
            % Ra - ohm*cm or ohm*cm^2/cm
            % m.L: um
            % m.diam: um
            % -> result: axial_resistance should be in M ohms
            resistance_scaling = 1e-6;
            length_scaling = 1e3;
            axial_resistance = (m.Ra)*resistance_scaling*length_scaling*(m.L/m.nseg)/(pi*(m.diam/2)^2);

            % Create all the required channel models
            if m.insert_pas
                pas = create_pas(m);
            end
            if m.insert_hh
                hh = create_hh(m);
            end

            
            % define the currents
            m.currents = cell(1,m.nseg);
            for i=1:m.nseg
                v = m.voltages{i};
                
                
                % initialize it
                m.currents{i} = Exp(0);
                % add the conductances
                if m.insert_pas
                    sm = m.submodel(pas);
                    sm.v = v;
                    sm.v0 = v0;
                    sm.diam = m.diam;
                    sm.area = m.area;
                    sm.celsius = m.celsius;
                    
                    params = m.params_map(pas.Name);
                    for pid=1:length(params) 
                        p = m.params(params{pid});
                        sm.(p.input) = p.value;
                    end
                    m.currents{i} = m.currents{i} - m.unit_factor * m.area * sm.ipas;
                end
                
                if m.insert_hh
                    sm = m.submodel(hh);
                    sm.v = v;
                    sm.v0 = v0;
                    sm.diam = m.diam;
                    sm.area = m.area;
                    sm.celsius = m.celsius;

                    params = m.params_map(hh.Name);
                    for pid=1:length(params)
                        p = m.params(params{pid});
                        sm.(p.input) = p.value;
                    end
                    m.currents{i} = m.currents{i} - m.unit_factor * m.area * (sm.ina + sm.ik + sm.ileak);
                end
                % add all the inserted channels
                inserted_names = keys(m.channels);
                for iid=1:length(inserted_names)
                    name = inserted_names{iid};
                    mdl = m.channels(name);
                    sm = m.submodel(mdl);
                     sm.v = v;
                     sm.v0 = v0;
%                     sm.diam = m.diam;
                     sm.area = m.area;
                     sm.L = m.L;
%                     sm.celsius = m.celsius;

                    parameters = m.params_map(name);
                    for pid=1:length(parameters)
                        if isKey(m.params, parameters{pid})
                            p = m.params(parameters{pid});
                            val = p.value;
                            inp = p.input;
                        elseif isKey(m.global_params, parameters{pid})
                            val = m.global_params(parameters{pid});
                            inp = parameters{pid};
                        else
                            error('Simatra:NEURON:Section:build', 'Input %s to submodel of type %s does not exist', inp, name);
                        end
                        if isfinite(val)
                            sm.(inp) = val;
                        else
                            error('Simatra:NEURON:Section:build', 'Input %s to submodel of type %s has not been defined', inp, name);
                        end

                    end
                    totalCurrent = 0;
                    channel_current_names = getCurrents(mdl);
                    for cid=1:length(channel_current_names)
                        totalCurrent = totalCurrent + sm.(channel_current_names{cid});
                    end
                    m.currents{i} = m.currents{i} - m.unit_factor * m.area * totalCurrent;
                end
                

                % add point currents
                begin_pos = (i-1)/m.nseg;
                end_pos = i/m.nseg;
                for j=1:length(m.pointCurrents)
                    pos = m.pointCurrents(j).pos;
                    if pos >= begin_pos && pos < end_pos
                        sm = m.submodel(m.pointCurrents(j).model);
                        m.currents{i} = m.currents{i} + sm.(m.pointCurrents(j).I);
                    end
                end
                
                % and now the intrasection currents
                if m.nseg > 1
                    switch i
                        case 1
                            intra = (m.voltages{i+1}-v)/axial_resistance;
                        case m.nseg
                            intra = (m.voltages{i-1}-v)/axial_resistance;
                        otherwise
                            intra = (m.voltages{i+1}-v)/axial_resistance + (m.voltages{i-1}-v)/axial_resistance;
                    end
                    m.currents{i} = m.currents{i} + intra;     
                end
                    
                % and the intersection currents
                for j=1:length(m.connections)
                    c = m.connections(j);
                    
                    % create an input for this connection
                    if m.Inputs.isKey(c.Vname)
                        inp = Exp(c.Vname);
                    else
                        inp = m.input(c.Vname);
                    end
                    
                    %Vpre = c.othersection.v(c.otherpos);
                    Vpre = inp;
                    pos_index = pos2ind(m.nseg,c.pos);
                    % Ra - ohm*m or ohm*m^2/m
                    p = c.othersection;
                    pre_axial_resistance = (p.Ra)*resistance_scaling*length_scaling*(p.L/p.nseg)/(pi*(p.diam/2)^2);
                    average_resistance = (pre_axial_resistance + axial_resistance)/2;
                    current = (Vpre-m.voltages{pos_index})/average_resistance;
                    m.currents{pos_index} = m.currents{pos_index} + current;
                end
                
                % create the differential equation for the segment
                % m.currents{i}: nA
                % m.cm: uF/cm^2
                % v: mV
                % dt: mV
                m.diffequ(v,(1/(m.cm*1e3*m.area))*m.currents{i});
            end
            
            % output all the voltage values
            for i=1:length(m.voltages)
                m.output(['v_' num2str(i)], m.voltages{i});
            end

        end
        
%         function initializeModel(m)
%             initializeModel@Model(m);
%             m.voltages = {};
%             m.currents = {};
%             m.pointCurrents = [];
%         end
        
%         % when we produce DSL code, we need to rebuild all voltage
%         % evolution equations.  This step is responsible for clearing the
%         % current values.
%         function clearVoltages(m)
%             voltage_ids = List.map (@toId, m.voltages);
%             function remove_id(id)
%                 if m.States.isKey(id)
%                     m.States.remove(id);
%                 end
%                 if m.DiffEqus.isKey(id)
%                     m.DiffEqus.remove(id);
%                 end
%             end
%             List.app (@remove_id, voltage_ids);
%         end
    end
    
end

% Don't allow singularites in the code
function r = trap(x, y)
  % math inspired from NEURON
  r = piecewise(y*(1-x/y/2), abs(x/y) < 1e-6, x/(exp(x/y)-1));
end

function ind = pos2ind(nseg, pos)
ind = max(1, ceil(pos*nseg));
end

function n = create_pas(section)

    n = NMODL('pas', section.t_exp);
    e_pas = n.input('e', -70);
    g_pas = n.input('g', 0.001);
    ipas = g_pas * (n.v - e_pas);
    n.output(ipas);
    
end

function n = create_hh(section)

    n = NMODL('hh', section.t_exp);
    
    % define the inputs
    gnabar = n.input('gnabar', 0.12);
    gkbar = n.input('gkbar', 0.036);
    gl = n.input('gl', 0.0003);
    el = n.input('el', -54.3);
    
    % compute temperature coefficient
    q10 = 3.0^((section.celsius-6.3)/10);
    
    % compute the rates
    m_alpha = n.equ(0.1*trap(-(n.v+40),10));
    m_beta = n.equ(4*exp(-(n.v+65)/18));
    m_sum = n.equ(m_alpha+m_beta);
    m_tau = n.equ(1/(q10*m_sum));
    m_inf = n.equ(m_alpha/m_sum);
    m_gate = n.state(0);
    n.diffequ(m_gate, (m_inf-m_gate)/m_tau);
    
    h_alpha = n.equ(0.07*exp(-(n.v+65)/20));
    h_beta = n.equ(1/(exp(-(n.v+35)/10)+1));
    h_sum = n.equ(h_alpha+h_beta);
    h_tau = n.equ(1/(q10*h_sum));
    h_inf = n.equ(h_alpha/h_sum);
    h_gate = n.state(0);
    n.diffequ(h_gate, (h_inf-h_gate)/h_tau);
    
    n_alpha = n.equ(0.01*trap(-(n.v+55),10));
    n_beta = n.equ(0.125*exp(-(n.v+65)/80));
    n_sum = n.equ(n_alpha+n_beta);
    n_tau = n.equ(1/(q10*n_sum));
    n_inf = n.equ(n_alpha/n_sum);
    n_gate = n.state(0);
    n.diffequ(n_gate, (n_inf-n_gate)/n_tau);
    
    % compute the currents
    ina = n.equ(gnabar * m_gate^3 * h_gate * (n.v - section.ena)); % nA
    ik = n.equ(gkbar * n_gate^4 * (n.v - section.ek)); % nA
    ileak = n.equ(gl * (n.v - el)); % nA
    
    n.output(ina);
    n.output(ik);
    n.output(ileak);
    
end
