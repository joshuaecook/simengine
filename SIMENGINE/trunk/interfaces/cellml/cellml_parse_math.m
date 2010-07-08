function [math] = cellml_parse_math(mathNode)
% [math] = cellml_parse_math(mathNode)
%
% Traverses an XML DOM representation of MathML excerpted from a CellML component.
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
%if node.getNodeType ~= node.ELEMENT_NODE
%    error('Simatra:cellml_parse_math', 'Not a math expression (%d)', node.getNodeType);
%end

switch char(node.getNodeName)
    case '#text'
        expression = cellml_parse_math_expression(node.getNextSibling);
    case 'ci'
        expression = Exp(strtrim(char(node.getTextContent)));
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
    case 'piecewise'
        child = node.getFirstChild;
        oper = char(child.getNodeName);
        %xDoc.methods
        i = 1;
        while ~isempty(child)
            subname = char(child.getNodeName);
            if ~strcmp(subname, '#text')
                if strcmp(subname, 'piece')
                    result = cellml_parse_math_expression(child);
                    args{i} = result{1};
                    i = i + 1;
                    args{i} = result{2};
                    i = i + 1;
                elseif strcmp(subname, 'otherwise')
                    result = cellml_parse_math_expression(child.getFirstChild);
                    args{i} = result;
                    i = i + 1;
                else
                    disp(['unexpected name ' subname]);
                    i = i + 1;
                end
            end
            child = child.getNextSibling;
        end
        expression = piecewise(args{:});
        
    case 'piece'
        child = node.getFirstChild;
        i = 1;
        value = false; condition = false;
        while ~isempty(child)
            name = char(child.getNodeName());
            if ~strcmp(name, '#text')
                if i == 1
                    value = cellml_parse_math_expression(child);
                else
                    condition = cellml_parse_math_expression(child);
                end
                i = i + 1;
            end
            child = child.getNextSibling;
        end
        expression = {value, condition};
        
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
                expression = foldl(@(elem, init)(elem + init), operands{1}, operands(2:end));
            case 'minus'
                if length(operands) == 1
                    expression = -operands{1};
                else
                    expression = operands{1} - operands{2};
                end
            case 'times'
                expression = foldl(@(elem, init)(elem * init), operands{1}, operands(2:end));
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
            case 'gt'
                expression = operands{1} > operands{2};
            case 'lt'
                expression = operands{1} < operands{2};
            case 'and'
                expression = foldl(@(elem, init)(elem & init), operands{1}, operands(2:end));
            case 'or'
                expression = foldl(@(elem, init)(elem | init), operands{1}, operands(2:end));
            case 'exp'
                expression = exp(operands{1});
            case 'ln'
                expression = log(operands{1});
            case 'log'
                expression = log10(operands{1});
            case 'cosh'
                expression = cosh(operands{1});
            case 'root'
                if length(operands) == 1
                    expression = operands{1}^0.5;
                else
                    expression = operands{1}^(1/operands{2});
                end
            case {'eq','diff'}
                expression = [char(operator.getNodeName) operands];
            otherwise
                op = char(operator.getNodeName);
                error('Simatra:cellml_parse_math','Unexpected operation %s with %d operands', op, length(operands));
        end
    otherwise
        name = char(node.getNodeName);
        disp(sprintf('operating on unknown %s', name));
        operands = {name};
        
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

% FOLDL - folds the list by executing the function and reducing it to one
% value
function r = foldl(fcn, init, list)

switch length(list)
    case 0
        r = init;
    case 1
        r = fcn(list{1},init);
    otherwise
        r = foldl(fcn, fcn(list{1}, init), list(2:end));
end

end
