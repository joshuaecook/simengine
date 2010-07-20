classdef Neuron < Model
   
    properties (Access = public)
        dt = 0.01;
        celsius = false
    end
    
    properties (Access = protected)
        sections
        points = []
    end
    
    methods (Access = public)
        function n = Neuron(id)
            n@Model(id);
            n.sections = containers.Map;
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
                    sec = Section(sid);
                    sec.dt = n.dt;
                    %sec_sm = n.submodel(sec);
                    if isempty(s)
                        s = struct('section', sec, 'num', num);
                    else
                        s(i) = struct('section', sec, 'num', num);
                    end
                end
            else
                sec = Section(id);
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
            stim = IClamp();
            stim.dt = n.dt;
            s = struct('model', stim, 'section', sec, 'position', pos);
            if isempty(n.points)
                n.points = s;
            else
                n.points(end+1) = s;
            end
            sec.addPointCurrent(stim, pos);
        end
        
        function connect(n, sec1, pos1, sec2, pos2)
        end
        
        function s = toStr(n)
            initializeModel(n);
            build(n); % build the model
            s = toStr@Model(n); % now, create the string representation
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
                    for k=1:nseg
                        v = ['v_' num2str(k)];
                        output_args{k} = sm.(v);
                    end
                    n.output(output_name, output_args);
                end
            end
            
        end
    end
    
end