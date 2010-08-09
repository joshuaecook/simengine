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
    
    % for hh
    properties (Dependent)
        gnabar_hh
        gkbar_hh
        gl_hh
        el_hh
    end
    
    % for pas
    properties (Dependent)
        g_pas
        e_pas
    end
    
    % internal parameters
    properties (Access = private)
        hh_props = struct('gnabar', 0.12, 'gkbar', 0.036, 'gl', 0.0003, 'el', -54.3);
        pas_props = struct('g', 0.001, 'e', -70);
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
        function m = Section(id)
            m@Model(id);
        end
        
        function insert(m, channels)
            switch(channels)
                case 'hh'
                    m.insert_hh = true;
                case 'pas'
                    m.insert_pas = true;
                otherwise
                    error('Simatra:NEURON:Section', 'Invalid import');
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
        % HH set methods
        function set.gnabar_hh(m, val)
            if m.insert_hh
                m.hh_props.gnabar = val;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function set.gkbar_hh(m, val)
            if m.insert_hh
                m.hh_props.gkbar = val;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function set.gl_hh(m, val)
            if m.insert_hh
                m.hh_props.gl = val;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function set.el_hh(m, val)
            if m.insert_hh
                m.hh_props.el = val;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        
        % HH get methods
        function val = get.gnabar_hh(m)
            if m.insert_hh
                val = m.hh_props.gnabar;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function val = get.gkbar_hh(m)
            if m.insert_hh
                val = m.hh_props.gkbar;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function val = get.gl_hh(m)
            if m.insert_hh
                val = m.hh_props.gl;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end
        function val = get.el_hh(m)
            if m.insert_hh
                val = m.hh_props.el;
            else
                error('Simatra:NEURON:Section', 'HH channels not inserted');
            end
        end

        % PAS set methods
        function set.g_pas(m, val)
            if m.insert_pas
                m.pas_props.g = val;
            else
                error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
            end
        end
        function set.e_pas(m, val)
            if m.insert_pas
                m.pas_props.e = val;
            else
                error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
            end
        end
        % PAS get methods
        function val = get.g_pas(m)
            if m.insert_pas
                val = m.pas_props.g;
            else
                error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
            end
        end
        function val = get.e_pas(m)
            if m.insert_pas
                val = m.pas_props.e;
            else
                error('Simatra:NEURON:Section', 'Passive (pas) channels not inserted');
            end
        end

        
    end
    
    methods (Access = protected)
        function build(m)
            % Create the iterators
            m.t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', m.dt);
            m.t_imp = Iterator('t_imp', 'continuous', 'solver', 'linearbackwardeuler', 'dt', m.dt);            
            m.DefaultIterator = m.t_exp;

            disp(['Building Section: ' m.Name]);
            % define the voltage states
            m.voltages = cell(1,m.nseg);
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
            SAseg = SAall/m.nseg; % cm^2
            
            % Ra - ohm*cm or ohm*cm^2/cm
            % m.L: um
            % m.diam: um
            % -> result: axial_resistance should be in M ohms
            resistance_scaling = 1e-6;
            length_scaling = 1e3;
            axial_resistance = (m.Ra)*resistance_scaling*length_scaling*(m.L/m.nseg)/(pi*(m.diam/2)^2);

            % define the currents
            m.currents = cell(1,m.nseg);
            for i=1:m.nseg
                v = m.voltages{i};
                unit_factor = 1e6; % Conductance (S -> uS)
                
                
                % initialize it
                m.currents{i} = Exp(0);
                % add the conductances
                if m.insert_pas
                    Ipas = m.g_pas * unit_factor * SAseg * (v - m.e_pas);
                    m.currents{i} = m.currents{i} - Ipas;
                end
                
                if m.insert_hh
                    % compute temperature coefficient
                    q10 = 3.0^((m.celsius-6.3)/10);

                    % compute the rates
                    m_alpha = m.equ(0.1*trap(-(v+40),10));
                    m_beta = m.equ(4*exp(-(v+65)/18));
                    m_sum = m.equ(m_alpha+m_beta);
                    m_tau = m.equ(1/(q10*m_sum));
                    m_inf = m.equ(m_alpha/m_sum);
                    m_gate = m.state(0, 'iter', m.t_exp);
                    m.diffequ(m_gate, (m_inf-m_gate)/m_tau);
                    
                    h_alpha = m.equ(0.07*exp(-(v+65)/20));
                    h_beta = m.equ(1/(exp(-(v+35)/10)+1));
                    h_sum = m.equ(h_alpha+h_beta);
                    h_tau = m.equ(1/(q10*h_sum));
                    h_inf = m.equ(h_alpha/h_sum);
                    h_gate = m.state(0, 'iter', m.t_exp);
                    m.diffequ(h_gate, (h_inf-h_gate)/h_tau);

                    n_alpha = m.equ(0.01*trap(-(v+55),10));
                    n_beta = m.equ(0.125*exp(-(v+65)/80));
                    n_sum = m.equ(n_alpha+n_beta);
                    n_tau = m.equ(1/(q10*n_sum));
                    n_inf = m.equ(n_alpha/n_sum);
                    n_gate = m.state(0, 'iter', m.t_exp);
                    m.diffequ(n_gate, (n_inf-n_gate)/n_tau);
                    
                    % compute the currents
                    INa = m.equ(m.gnabar_hh * unit_factor * SAseg * m_gate^3 * h_gate * (v - m.ena)); % nA
                    IK = m.equ(m.gkbar_hh * unit_factor * SAseg * n_gate^4 * (v - m.ek)); % nA
                    Ileak = m.equ(m.gl_hh * unit_factor * SAseg * (v - m.el_hh)); % nA
                    
                    % sum them up
                    m.currents{i} = m.currents{i} - (INa + IK + Ileak);
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
                m.diffequ(v,(1/(m.cm*1e3*SAseg))*m.currents{i});
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
