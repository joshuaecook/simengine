classdef Neuron < Model
% Neuron - generate neural morphologies following the NEURON paradigm
% 
% Neuron Methods:
%   Constructor:
%   Neuron - create a new Neuron model representing a single neuron
%
%   Model construction:
%   section - generate a section within the NEURON representing one or more
%   segments
%   connect - connect two sections together within a Neuron
%   IClamp - generate a current clamp object
%
%   Model simulation:
%   Model/simex - execute the simulation
%
% Neuron Properties:
%   celsius - temperature of the cell
%   dt      - time step of the simulator
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
        celsius = false
    end
    
    properties
        dt = 0.01;
    end  

    properties (Access = protected)
        sections
        points = []
        t_imp
        t_exp
        nmod
    end
    
    methods (Access = public)
        function n = Neuron(id)
            % Neuron - create a Neuron object
            %
            % Usage:
            %   n = Neuron(ID) - create a new NEURON object with name ID
            %
            % Description:
            %   The Neuron constructor creates a new Neuron object
            %   container. Within this container, sections and point
            %   processes can be added as needed.  When finished, the
            %   Neuron object can be simulated using the Model/simex
            %   method.
            %
            % Usage:
            %   n = Neuron('sthA'); % create the Neuron object
            %
            %   soma = n.section('soma'); % add a section called 'soma'
            %   soma.nseg = 1;
            %   soma.diam = 18.8;
            %   soma.L = 18.8;
            %   soma.Ra = 123;
            %   soma.insert('hh'); % insert hh conductances
            %
            %   stim = n.IClamp(soma, 0.5); % add an IClamp point process
            %   stim.del = 100;
            %   stim.dur = 100;
            %   stim.amp = 0.1;
            %
            %   tstop = 300;
            %   o = n.simex(tstop); % simulate the Neuron model
            %   simplot(o.v_soma);  % and plot the results
            %   title('Somatic Voltage');
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %

            % Call the parent Model class
            n@Model(id);
            % Create an empty container for the sections
            n.sections = containers.Map;
            
            % Create the iterators
            n.t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', n.dt);
            n.t_imp = Iterator('t_imp', 'continuous', 'solver', 'linearbackwardeuler', 'dt', n.dt);            
            n.DefaultIterator = n.t_exp;
            
            % Create the nmod container
            n.nmod = containers.Map;
            
        end
        
        
        function sec = section(n, id, num)
            % Section - create a Section within a Neuron object
            %
            % Usage:
            %   s = Section(id) - create one section inside the Neuron
            %   s = Section(id, N) - create N sections inside the Neuron
            %   and return s as a cell array of sections
            %
            % Description:
            %   The Section method creates a new section inside of a Neuron
            %   object.  The Sections are cylindrical models, unless the
            %   diameter equals the length, in which case it uses a
            %   spherical model.  Each Section can be broken up into one or more
            %   equal segments for increased spatial resolution.
            %
            % Examples:
            %   soma = n.section('soma');
            %   ndend = 2;
            %   dend = n.section('dend', ndend);
            %
            % See also Section Neuron/Connect
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            if nargin == 2
                num = 1;
            end
            if nargin < 2 || nargin > 3
                error('Simatra:NEURON:section', 'Argument Error');
            end
            
            % Create the sections
            if num > 1
                s = [];
                for i=1:num
                    sid = [id '_' num2str(i)];
                    sec = Section(sid, n.t_imp, n.t_exp);
                    sec.dt = n.dt;
                    if isempty(s)
                        s = struct('section', sec, 'num', num);
                    else
                        s(i) = struct('section', sec, 'num', num);
                    end
                end
            else
                sec = Section(id, n.t_imp, n.t_exp);
                if isnumeric(n.celsius)
                    sec.celsius = n.celsius; % set the temperature in each section
                end
                sec.dt = n.dt;
                sec_sm = n.submodel(sec);
                s = struct('section', sec, 'submodel', sec_sm, 'num', num);
            end
            n.sections(id) = s;
            
            if num > 1
                sec = cell(1,num);
                for i=1:num
                    sec{i} = s(i).section;
                end
            else
                sec = s.section;
            end
        end
        
        function stim = IClamp(n, sec, pos)
            % IClamp - add a current clamp point process to a section
            %
            % Usage:
            %   obj = IClamp(n, sec, pos) - add a current clamp point
            %   process to the specified section at the designated
            %   position (from 0 to 1).  The return argument is an IClamp
            %   obj.
            %
            % Examples:
            %   n = Neuron('myNeuron');
            %   soma = n.section('soma');
            %   stim = n.IClamp(soma, 0.5); % adds the current clamp to the
            %   center of the soma
            %   % define the parameters of the current clamp
            %   stim.del = 100; % time that the clamp is disabled
            %   stim.dur = 100; % duration that the clamp is enabled
            %   stim.amp = 0.1; % amplitude of the clamp when enabled
            %
            % See also Section IClamp
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            stim = IClamp(n.t_exp);
            s = struct('model', stim, 'section', sec, 'position', pos);
            if isempty(n.points)
                n.points = s;
            else
                n.points(end+1) = s;
            end
            sec.addPointCurrent(stim, pos);
        end
        
        function connect(n, sec1, pos1, sec2, pos2)
            % Connect - link two sections together within a neuron
            %
            % Usage:
            %   Connect(n, sec1, pos1, sec2, pos2) - connect the two
            %   sections together at the specified positions (from 0 to 1
            %   across the section).
            %
            % Examples:
            %   soma = n.section('soma');
            %   ndend = 2;
            %   dend = n.section('dend', ndend);
            %   % chain the two dendrites together
            %   connect(n, soma, 0, dend{1}, 0);
            %   connect(n, dend{1}, 1, dend{2}, 0);
            %
            % See also Neuron/Section Section
            %
            %  The Neuron syntax and semantics are based on the NEURON Simulator by
            %  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            
            % Call build to generate the sections, even if it will be thrown away
            initializeAndBuild(sec1);
            initializeAndBuild(sec2);
            
            % Now, the model is built, so we can appropriately add the
            % connections
            sec1.addConnection(sec2, pos2, pos1);
            sec2.addConnection(sec1, pos1, pos2);
        end
        
        function s = toStr(n)
            initializeModel(n);
            build(n); % build the model
            s = toStr@Model(n); % now, create the string representation
        end
        
    end

    methods
        function set.dt(n, val)
            % set.dt - update the dt value both as a property and within
            % the global implicit and explicit iterators
            n.dt = val;
            n.t_imp.dt = val;
            n.t_exp.dt = val;
        end
        
        function val = get.dt(n)
            val = n.dt;
        end
    end
    
    methods (Access = protected)
        function build(n)
            % build - protected function used to construct the internal
            % representation of the Neuron
            
            % Build all the sections
            ids = n.sections.keys();
            for i=1:length(ids)
                s = n.sections(ids{i});
                for j=1:length(s)
                    nseg = s(j).section.nseg;
                    if s(j).num > 1
                        output_name = ['v_' ids{i} num2str(j)];
                    else
                        output_name = ['v_' ids{i}];
                    end
                    output_args = cell(1, nseg);
                    sec = s(j).section;
                    initializeAndBuild(sec);
                    sm = n.submodel(sec);
                    sec.submodelInstance = sm;
                    for k=1:nseg
                        v = ['v_' num2str(k)];
                        output_args{k} = sm.(v);
                    end
                    n.output(output_name, output_args);

                end
            end
            
            % link up the connections
            for i=1:length(ids)
                s = n.sections(ids{i});
                for j=1:length(s)
                    nseg = s(j).section.nseg;
                    dest_sm = s(j).section.submodelInstance;
                    connections = s(j).section.connections;
                    for k=1:length(connections)
                        c = connections(k);
                        source_sm = c.othersection.submodelInstance;
                        v = ['v_' num2str(pos2ind(nseg,c.pos))];
                        dest_sm.(c.Vname) = source_sm.(v);
                    end
                end
            end
        end
    end
    
end
% pos2ind - convert a position value from 0 to 1 to an integer segment number
function ind = pos2ind(nseg, pos)
ind = max(1, ceil(pos*nseg));
end
