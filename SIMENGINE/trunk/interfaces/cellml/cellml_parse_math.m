function [math] = cellml_parse_math(mathNode)
  %% [math] = cellml_parse_math(mathNode)
  %%
  %% Traverses an XML DOM representation of MathML excerpted from a CellML component.
  if mathNode.getNodeType ~= mathNode.ELEMENT_NODE || ~strcmp('math',mathNode.getNodeName)
    error('Not a math node')
  end

  math = cell(0);

  elem = mathNode.getFirstChild;
  while ~isempty(elem)
    if elem.getNodeType ~= elem.ELEMENT_NODE
      elem = elem.getNextSibling;
      continue;
    end

    expr = cellml_parse_math_expression(elem);
    math{end+1} = expr;
    
    elem = elem.getNextSibling;
  end
end

function [expression] = cellml_parse_math_expression(node)
  if node.getNodeType ~= node.ELEMENT_NODE
    error('Not a math expression')
  end

  switch char(node.getNodeName)
    case 'ci'
      expression = Exp(char(node.getTextContent));
    case 'cn'
      expression = Exp(str2double(char(node.getTextContent)));
    case 'bvar'
      operand = node.getFirstChild;
      while ~isempty(operand) && operand.getNodeType ~= node.ELEMENT_NODE
	operand = operand.getNextSibling;
      end
      if isempty(operand)
	error('Bound variable without content')
      end
      expression = {'bvar' cellml_parse_math_expression(operand)};
    case 'apply'
      operator = node.getFirstChild;
      operands = cell(0);
      
      while ~isempty(operator) && operator.getNodeType ~= node.ELEMENT_NODE
	operator = operator.getNextSibling;
      end
	
      elem = operator.getNextSibling;
      while ~isempty(elem)
	if elem.getNodeType == node.ELEMENT_NODE
	  expr = cellml_parse_math_expression(elem);
	  operands{end+1} = expr;
	end
	elem = elem.getNextSibling;
      end

      switch char(operator.getNodeName)
        case 'plus'
          expression = operands{1} + operands{2};
        case 'minus'
          if length(operands) == 1
            expression = -operands{1};
          else
            expression = operands{1} - operands{2};
          end
        case 'times'
          expression = operands{1} * operands{2};
        case 'divide'
          expression = operands{1} / operands{2};
        case 'power'
          expression = operands{1} ^ operands{2};
        case 'geq'
          expression = operands{1} >= operands{2};
        case 'leq'
          expression = operands{1} <= operands{2};
        case 'ge'
          expression = operands{1} >= operands{2};
        case 'le'
          expression = operands{1} <= operands{2};
        case 'and'
          expression = operands{1} & operands{2};
        case 'exp'
          expression = exp(operands{1});
	otherwise
	  expression = [char(operator.getNodeName) operands];
      end
    otherwise
      operands = {char(node.getNodeName)};

      elem = node.getFirstChild;
      while ~isempty(elem)
	if elem.getNodeType == node.ELEMENT_NODE
	  operands{end+1} = cellml_parse_math_expression(elem);
	end
	elem = elem.getNextSibling;
      end

      expression = operands;
  end
end
