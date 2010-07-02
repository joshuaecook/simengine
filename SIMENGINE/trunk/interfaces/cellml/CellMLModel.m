classdef CellMLModel < Model
   
    properties
        units = containers.Map;
    end
    
    methods
        % Constructor
        function c = CellMLModel(cellmlfile)
            [filepath, filename, fileext] = fileparts(cellmlfile);
            c@Model(['CellML_' filename]);

	    dox = xmlread(cellmlfile);
	    [units,components,groups,connections,symbols] = cellml_parse_model(dox.getDocumentElement);

	    for comp = 1:length(components)
	      if ~isempty(components(comp).math)
		subm = Model(components(comp).name);

		for eqn = 1:length(components(comp).math)
		  if iscell(components(comp).math{eqn})
		    lhs = [];
		    rhs = [];
		    switch components(comp).math{eqn}{1}
		      case 'eq'
			lhs = components(comp).math{eqn}{2};
			rhs = components(comp).math{eqn}{3};
		    end

		    if isa(lhs,'Exp')
		      if isa(rhs,'Exp')
			subm.equ(lhs,rhs);
		      end
		    elseif iscell(lhs)
		      switch lhs{1}
			case 'diff'
			  indvar = lhs{2};
			  depvar = lhs{3};
			  if isa(depvar,'Exp') && depvar.isVar
			    state = subm.state(depvar.value,nan);
			    subm.diffequ(state, rhs);
			  end
		      end
		    end
		  end
		end

		for var = 1:length(components(comp).variables)
		  if strcmp('in',components(comp).variables(var).public_interface) || strcmp('in',components(comp).variables(var).private_interface)
		    subm.input(components(comp).variables(var).name, ...
			       components(comp).variables(var).initial_value);
		  elseif strcmp('out',components(comp).variables(var).public_interface) || strcmp('out',components(comp).variables(var).private_interface)
		    if ~subm.States.isKey(components(comp).variables(var).name)
		      subm.state(components(comp).variables(var).name,nan);
		    end
		    subm.output(components(comp).variables(var).name);
		  end
		end

		c.submodel(subm);
	      end
	    end

            %[c.xDoc, c.cellMLStruct] = parse_xml(cellmlfile);
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