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
        t_exp
        t_imp
        insert_pas = false;
        insert_hh = false;
    end
    
    methods (Access = public)
        function m = Section(id)
            m@Model(id);
            m.t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', m.dt);
            m.t_imp = Iterator('t_imp', 'continuous', 'solver', 'linearbackwardeuler', 'dt', m.dt);            
            m.DefaultIterator = m.t_exp;
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
            % define the voltage states
            m.voltages = cell(1,m.nseg);
            for i=1:m.nseg
               m.voltages{i} = m.state(-65, 'iter', m.t_imp);
            end
            
            % define surface areas
            SAall_square_microns = m.L*m.diam*pi; % um^2 (1e-6 ^ 2 = 1e-12)
                                                  % cm^2 (1e-2 ^ 2 = 1e-4)
            SAall = SAall_square_microns * 1e-8;  % 1e-12 / 1e-4 = 1e-8
            SAseg = SAall/m.nseg;
            
            % define the currents
            m.currents = cell(1,m.nseg);
            for i=1:m.nseg
                v = m.voltages{i};
                
                % initialize it
                m.currents{i} = Exp(0);
                % add the conductances
                if m.insert_pas
                    Ipas = m.g_pas * SAseg * (v - m.e_pas);
                    m.currents{i} = m.currents{i} + Ipas;
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
                    INa = m.gnabar_hh * SAseg * m_gate^3 * h_gate * (v - m.ena);
                    IK = m.gkbar_hh * SAseg * n_gate^4 * (v - m.ena);
                    Ileak = m.gl_hh * SAseg * (v - m.el_hh);
                    
                    % sum them up
                    m.currents{i} = m.currents{i} + INa + IK + Ileak;
                end

                % and now the intrasection currents
                if m.nseg > 1
                    switch i
                        case 1
                            intra = (m.voltages{i+1}-v)/(m.Ra);
                        case m.nseg
                            intra = (v-m.voltages{i-1})/(m.Ra);
                        otherwise
                            intra = (m.voltages{i+1}-v)/(m.Ra) + (v-m.voltages{i-1})/(m.Ra);
                    end
                    m.currents{i} = m.currents{i} + intra;     
                end
                    
                % and the intersection currents
                
                % create the differential equation for the segment
                m.diffequ(v,(1/(m.cm*SAseg))*m.currents{i});
            end
            
            % output all the voltage values
            m.output('voltages', m.voltages);

        end
        
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
  r = piecewise(abs(x/y) < 1e-6, y*(1-x/y/2), x/(exp(x/y)-1));
end
