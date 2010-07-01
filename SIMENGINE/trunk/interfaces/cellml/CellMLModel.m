classdef CellMLModel < Model
   
    properties
        units = containers.Map;
        xDoc
        cellMLStruct
    end
    
    methods
        % Constructor
        function c = CellMLModel(cellmlfile)
            [filepath, filename, fileext] = fileparts(cellmlfile);
            c@Model(['CellML_' filename]);
            [c.xDoc, c.cellMLStruct] = parse_xml(cellmlfile);
        end
        
        function addUnit(c, name, unit_list)
            s = struct();
            scaling = 1;
            for i = 1:length(unit_list)
                u = unit_list(i);
                s(i) = struct('unit', u.unit, 'prefix', SI_prefix(u.prefix), 'exponent', u.exponent);
                scaling = scaling * (s(i).prefix * s(i).exponent);
            end
            c.units(name) = struct('scaling', scaling, 'units', s);
        end
    end
    
end

function e = SI_prefix(str)
switch str
    case 'milli'
        e = -3;
    case 'micro'
        e = -6;
end

end