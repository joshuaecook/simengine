classdef GenericUnit
    
    properties
        id
        data
        flatdata
        flatfactor
    end
    
    properties (Access = protected)
        name
        scale = 1
        offset = 0
    end
    
    methods (Access = public)
                
        function u = mrdivide(u1,u2)
            u1 = toUnit(u1);
            u2 = toUnit(u2);
            u = u1*(u2^(-1));
        end
        
        function u = mtimes(u1,u2)
            u1 = toUnit(u1);
            u2 = toUnit(u2);
            s(1) = struct('unit', u1, 'prefix', 1, 'exp', 1);
            s(2) = struct('unit', u2, 'prefix', 1, 'exp', 1);
            u = Unit(s);
        end

        function u = mpower(u1, e)
            if ~isnumeric(e)
                error('Simatra:GenericUnit', 'Second argument for power must be numeric');
            end
            u1 = toUnit(u1);
            s = struct('unit', u1, 'prefix', 1, 'exp', e);
            u = Unit(s);
        end
    
        function er = toBase(u, e)
            er = u.scale*e + u.offset;
        end
        
    end
    
    methods (Access = protected)

        % flatten - reduces the data description down into something that
        % looks like just one list of multiplies and powers of base units
        %
        % assigns flatdata and flatfactor
        function [flatfactor, flatdata] = flatten(u)
            new_data = [];
            factor = 1;
            for i=1:length(u.data)
%                 disp(sprintf('i = %d', i));
%                 u.data(i)
                % u.data(i).unit
                u1 = u.data(i).unit;
                e = u.data(i).exp;
                if isa(u1, 'BaseUnit')
                    factor = factor * u.data(i).prefix^e;
                    new_data = append(new_data, struct('unit', u1, 'exp', e));
                elseif isa(u1, 'SIUnit')
                    orig_factor = factor;
                    
                    factor = factor * u1.factor^e;
                    u_base = Unit(u1.base);
                    factor = factor * u_base.flatfactor;
                    %disp(sprintf('Orig: %g, u1.factor: %g, u_base.flatfactor: %g, e: %g, New: %g', orig_factor, u1.factor, u_base.flatfactor, e, factor));
                    dat = u_base.flatdata;
                    for j=1:length(dat)
                        dat(j).exp = dat(j).exp * e;
                        new_data = append(new_data, dat(j));
                    end
                elseif isa(u1, 'Unit')
                     %fprintf(1, 'Found Unit (# data = %d) -> ', length(u1.flatdata));
                     %disp(u1)
                    factor = factor * u1.flatfactor^e;
                    for j=1:length(u1.flatdata)
                        d = u1.flatdata(j);
                        d.exp = d.exp * e;
                        %fprintf(1, ' -> Index #%d: ', j);
                        new_data = append(new_data, d);
                    end
                end
            end
            flatdata = new_data;
            flatfactor = factor;
            
        end
        
        function new_data = aggregate(u)
            d = u.flatdata;
            map = containers.Map;
            for i=1:length(d)
                id = d(i).unit.id;
                exp = d(i).exp;
                if map.isKey(id)
                    s = map(id);
                    s.exp = s.exp + exp;
                    map(id) = s;
                else
                    map(id) = d(i);
                end
            end
            keys = map.keys;
            new_data = [];
            for i=1:length(keys)
                v = map(keys{i});
                if v.exp ~= 0
                    if isempty(new_data);
                        new_data = v;
                    else
                        new_data(i) = v;
                    end
                end
            end
        end
        
    end
    
end

function ur = toUnit(u)
if isa(u, 'GenericUnit')
    ur = u;
elseif isnumeric(u)
    du = dimensionless;
    s = struct('unit', du, 'prefix', u, 'exp', 1);
    ur = Unit(s);
else
    error('Simatra:GenericUnit', 'Not a unit');
end
% 
% if isa(u, 'BaseUnit')
%     s = struct('unit', u, 'prefix', 1, 'exp', 1);
%     ur = Unit(s);
% elseif isa(u, 'SIUnit')
%     s = struct('unit', u, 'prefix', u.factor, 'exp', 1);
%     ur = Unit(s);
% else
%     ur = u;
% end

end

function du = dimensionless()
du = BaseUnit('dimensionless', '_');
end

function l2 = append(l1, element)
%disp('in append')
%element

if isempty(l1)
    l2 = element;
else
    l2 = l1;
    l2(end+1) = element;
end

end