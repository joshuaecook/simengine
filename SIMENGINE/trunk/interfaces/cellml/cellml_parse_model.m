function [units, components, groups, connections, symbols] = cellml_parse_model(modelNode)
  %% [units, components, groups, connections] = cellml_parse_model(modelNode)
  %
  % Traverses an XML DOM representation of a CellML model.
  if modelNode.getNodeType ~= modelNode.ELEMENT_NODE || ~strcmp('model',modelNode.getNodeName)
    error('Not a model node <%s>',modelNode.getNodeName)
  end

  symbols = containers.Map;
  units = struct('name',{},'units',{});
  components = struct('name',{},'variables',{},'math',{},'symbols',{});
  groups = struct('relationship_ref',{},'component_ref',{});
  connections = struct('components',{},'variables',{});

  elem = modelNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    switch char(elem.getNodeName)
      case 'units'
	[name, uns] = cellml_parse_units(elem);
	units(end+1).name = name;
	units(end).units = uns;
      case 'component'
	[name, vars, math, syms] = cellml_parse_component(elem);
	components(end+1).name = name;
	components(end).variables = vars;
	components(end).math = math;
	components(end).symbols = symbols;
        symbols(name) = components(end);
      case 'group'
	[rel_ref, comp_ref] = cellml_parse_group(elem);
	groups(end+1).relationship_ref = rel_ref;
	groups(end).component_ref = comp_ref;
      case 'connection'
	[comps, vars] = cellml_parse_connection(elem);
	connections(end+1).components = comps;
	connections(end).variables = vars;
    end

    elem = elem.getNextSibling;
  end
end


function [name,units] = cellml_parse_units(unitsNode)
  if ~strcmp('units',unitsNode.getNodeName)
    error('Not a units node')
  end

  name = char(unitsNode.getAttribute('name'));
  units = struct('exponent',{},'prefix',{},'units',{});

  elem = unitsNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    if strcmp('unit',char(elem.getNodeName))
      units(end+1).exponent = char(elem.getAttribute('exponent'));
      units(end).prefix = char(elem.getAttribute('prefix'));
      units(end).units = char(elem.getAttribute('units'));
    end

    elem = elem.getNextSibling;
  end
end

function [name,variables,math,symbols] = cellml_parse_component(compNode)
  if ~strcmp('component',compNode.getNodeName)
    error('Not a component node')
  end

  name = char(compNode.getAttribute('name'));
  variables = struct('name',{},'units',{},'initial_value',{},'private_interface',{},'public_interface',{});
  math = [];
  symbols = containers.Map;
  
  mathns = 'http://www.w3.org/1998/Math/MathML';

  elem = compNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    switch char(elem.getNodeName)
      case 'math'
	% Ought not to have to check the 'xmlns' attribute; is that a parser problem?
	if strcmp(mathns,elem.getNamespaceURI) || strcmp(mathns,elem.getAttribute('xmlns'))
	  math = cellml_parse_math(elem);
	end
      case 'variable'
	symbol = char(elem.getAttribute('name'));
	variables(end+1).name = symbol;
	variables(end).units = char(elem.getAttribute('units'));
	variables(end).initial_value = str2double(char(elem.getAttribute('initial_value')));
	variables(end).private_interface = char(elem.getAttribute('private_interface'));
	variables(end).public_interface = char(elem.getAttribute('public_interface'));
	symbols(symbol) = variables(end);
    end
    
    elem = elem.getNextSibling;
  end
end


function [components, variables] = cellml_parse_connection(connNode)
  if ~strcmp('connection',connNode.getNodeName)
    error('Not a connection node')
  end

  components = cell(1,2);
  variables = cell(0,2);

  elem = connNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    switch char(elem.getNodeName)
      case 'map_components'
	components{1} = char(elem.getAttribute('component_1'));
	components{2} = char(elem.getAttribute('component_2'));
      case 'map_variables'
	variables{end+1,1} = char(elem.getAttribute('variable_1'));
	variables{end,2} = char(elem.getAttribute('variable_2'));
    end

    elem = elem.getNextSibling;
  end
end

function [relationship_ref, component_ref] = cellml_parse_group(groupNode)
  if ~strcmp('group',groupNode.getNodeName)
    error('Not a group node')
  end

  relationship_ref = struct('name',{},'relationship',{});
  component_ref = struct('component',{},'component_ref',{});

  elem = groupNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    switch char(elem.getNodeName)
      case 'relationship_ref'
	relationship_ref(end+1).name = char(elem.getAttribute('name'));
	relationship_ref(end).relationship = char(elem.getAttribute('relationship'));
      case 'component_ref'
	[cmp, cmp_ref] = cellml_parse_component_ref(elem);
	component_ref(end+1).component = cmp;
	component_ref(end).component_ref = cmp_ref;
    end

    elem = elem.getNextSibling;
  end
end

function [component, component_ref] = cellml_parse_component_ref(node)
  if ~strcmp('component_ref',node.getNodeName)
    error('Not a component_ref node')
  end

  component = char(node.getAttribute('component'));
  component_ref = struct('component',{},'component_ref',{});
  
  elem = node.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    if strcmp('component_ref',elem.getNodeName)
      [cmp,cmp_ref] = cellml_parse_component_ref(elem);
      component_ref(end+1).component = cmp;
      component_ref(end).component_ref = cmp_ref;
    end
    elem = elem.getNextSibling;
  end

  if isempty(component_ref)
    component_ref = [];
  end
end
