classdef Neuron < Model
   
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
            n@Model(id);
            n.sections = containers.Map;
            
            % Create the iterators
            n.t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', n.dt);
            n.t_imp = Iterator('t_imp', 'continuous', 'solver', 'linearbackwardeuler', 'dt', n.dt);            
            n.DefaultIterator = n.t_exp;
            
            % Create the nmod container
            n.nmod = containers.Map;
            
            
        end
        
        
        function sec = section(n, id, num)
            if nargin == 2
                num = 1;
            end
            if nargin < 2 || nargin > 3
                error('Simatra:NEURON:section', 'Argument Error');
            end
            
            % Create the sections
            if num > 1
                sec = cell(1,num);
                sec_sm = cell(1,num);
                s = [];
                for i=1:num
                    sid = [id '_' num2str(i)];
                    sec = Section(sid, n.t_imp, n.t_exp);
                    sec.dt = n.dt;
                    %sec_sm = n.submodel(sec);
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
            tempstr = toStr(sec1);
            tempstr = toStr(sec2);
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
            % Build all of the points
%             for i=1:length(n.points)
%                 p = n.points(i);
%                 p.section.addPointCurrent(p.model, p.position);
%             end
            
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
                    tmpstr = toStr(sec);
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
function ind = pos2ind(nseg, pos)
ind = max(1, ceil(pos*nseg));
end
