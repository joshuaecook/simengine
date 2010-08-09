function [xDoc, s] = parse_xml(filename)

xDoc = xmlread(filename);
s = parse_node(xDoc);

end

function s = parse_node(xDoc, level)
    s = struct();
    index = false;
    %s = [];
    if nargin == 1
        level = 0;
    end
    indent = char(32*ones(1, 2*level));
    
    name = char(xDoc.getNodeName);
    switch name
        case 'variable'
            %disp(sprintf('%sAt node %s', indent, name));
            
            starting_struct = struct('public_interface', false, 'private_interface', false, 'initial_value', 1);
            s = attributes2struct(xDoc, starting_struct);
            s = recurse(xDoc, true, level, s);
        case 'units'
            %disp(sprintf('%sAt node %s', indent, name));
            
            starting_struct = struct();%'exponent', 1, 'prefix', '', 'units', 'dimensionless');
            s = attributes2struct(xDoc, starting_struct);
            s = recurse(xDoc, true, level, s);
        case 'unit'
            %disp(sprintf('%sAt node %s', indent, name));
            
            starting_struct = struct('exponent', 1, 'prefix', '', 'units', 'dimensionless');
            s = attributes2struct(xDoc, starting_struct);
            s = recurse(xDoc, true, level, s);
        case 'component'
            %disp(sprintf('%sAt node %s', indent, name));
            
            s = attributes2struct(xDoc);
            s.children = recurse(xDoc, true, level);
        case 'map_components'
            s = attributes2struct(xDoc);
            s = recurse(xDoc, true, level, s);
            
        case 'map_variables'
            s = attributes2struct(xDoc);
            s = recurse(xDoc, true, level, s);

        case {'documentation'} % ignore
            s = [];
        case {'model','#document', 'connection', 'group'}
            s = recurse(xDoc, true, level, s);
        case 'math'
            child = xDoc.getFirstChild;
            i = 1;
            exps = {};
            while ~isempty(child)
                
                name = char(child.getNodeName);
                if ~strcmp(name, '#text')
                    exps{i} = node2exp(child);
                    i = i + 1;
                end
                child = child.getNextSibling;
            end
            s.exp = exps;
%         case 'apply'
%             child = xDoc.getFirstChild;
%             s.oper = char(child.getNodeName);
%             i = 1;
%             while (~isempty(child))
%                 
%                 i = i + 1;
%                 child = child.getNextSibling;
%             end
        case 'ci'
            s = Exp(char(xDoc.getTextContent));
        case 'cn'
            s.exp = Exp(str2double(char(xDoc.getTextContent)));
        case {'relationship_ref', 'component_ref'}
            s = attributes2struct(xDoc);
            s.children = recurse(xDoc, true, level);
%         case {'ci'}
%             disp(sprintf('%sFound node %s', indent, name));
%             xDoc.methods
%             disp(sprintf('%s -> %s', char(xDoc.getNodeName), char(xDoc.getTextContent)));
%             s.children = recurse(xDoc, true, level);
%             
        otherwise
            disp(sprintf('%sIgnoring node %s', indent, name));
            sub_s = recurse(xDoc, true, level);
            if isempty(fieldnames(sub_s));
                s.text = char(xDoc.getTextContent);
            else
                s.children = sub_s;
            end
    end
    

end

function e = node2exp(xDoc)

name = char(xDoc.getNodeName);
switch(name)
    case 'apply'
        child = xDoc.getFirstChild;
        oper = char(child.getNodeName);
        %xDoc.methods
        i = 0;
        while ~isempty(child)
            subname = char(child.getNodeName);
            if ~strcmp(subname, '#text')
                if i == 0
                    oper = subname;
                    %disp(sprintf('operation %s', oper));
                else
                    args{i} = node2exp(child);
                    %disp(sprintf('i = %d (node=%s)', i, subname));
                end
                i = i + 1;
            end
            child = child.getNextSibling;
        end
        switch oper
            case 'plus'
                e = args{1} + args{2};
            case 'minus'
                if length(args) == 1
                    e = -args{1};
                else
                    e = args{1} - args{2};
                end
            case 'times'
                e = args{1} * args{2};
            case 'divide'
                e = args{1} / args{2};
            case 'power'
                e = args{1} ^ args{2};
            case 'geq'
                e = args{1} >= args{2};
            case 'leq'
                e = args{1} <= args{2};
            case 'ge'
                e = args{1} >= args{2};
            case 'le'
                e = args{1} <= args{2};
            case 'and'
                e = args{1} & args{2};
            case 'exp'
                e = exp(args{1});
            case 'eq'
                e.lhs = args{1};
                e.rhs = args{2};
                disp(sprintf('%s = %s', toStr(e.lhs), toStr(e.rhs)));
            case 'piecewise'
                if length(args) ~= 3
                    error('Simatra:parse_xml','Expecting three arguments for piecewise');
                end
                e = piecewise(args{1},args{2},args{3});
            otherwise
                disp(sprintf('Unrecognized operation %s', oper));
                e = Exp(0);
        end
    case 'ci'
        e = Exp(char(xDoc.getTextContent));
    case 'cn'
        e = Exp(str2double(char(xDoc.getTextContent)));
     case '#text'
         e = node2exp(xDoc.getNextSibling);
    case 'piece'
        
        child = xDoc.getFirstChild;
        i = 1;
        value = false; condition = false;
        while ~isempty(child)
            name = char(child.getNodeName());
            if ~strcmp(name, '#text')
                if i == 1
                    value = node2exp(child);
                else
                    condition = node2exp(child);
                end
                i = i + 1;
            end
            child = child.getNextSibling;
        end
%         value = node2exp(child);
%         child = child.getNextSibling;
%         condition = node2exp(child);
         e = {value, condition};
    case 'piecewise'
        child = xDoc.getFirstChild;
        oper = char(child.getNodeName);
        %xDoc.methods
        i = 1;
        while ~isempty(child)
            subname = char(child.getNodeName);
            if ~strcmp(subname, '#text')
                if strcmp(subname, 'piece')
                    result = node2exp(child);
                    args{i} = result{1};
                    i = i + 1;
                    args{i} = result{2};
                    i = i + 1;
                elseif strcmp(subname, 'otherwise')
                    result = node2exp(child.getFirstChild);
                    args{i} = result;
                    i = i + 1;
                else
                    disp(['unexpected name ' subname]);
                    i = i + 1;
                end
            end
            child = child.getNextSibling;
        end
        e = piecewise(args{:});
    otherwise 
        e = Exp(0);
        disp(sprintf('unrecognized tag %s',name));
end
end

function s = attributes2struct(xDoc, starting_struct)
    if 1 == nargin
        s = struct();
    else
        s = starting_struct;
    end
    attributes = xDoc.getAttributes;
    for j=0:(attributes.getLength-1)
        id = char(attributes.item(j).getName);
        value = char(attributes.item(j).getValue);
        if isValid(id)  && isempty(findstr(id, ':'))
            s.(id) = cast_str(value);
        end
    end
end

function s = recurse(xDoc, record, level, s)
if nargin == 1
    record = false;
    level = 0;
end
if nargin < 4
    s = struct();
end
name = char(xDoc.getNodeName);
childNode = xDoc.getFirstChild;
while ~isempty(childNode)
    subname = char(childNode.getNodeName);
    if isValid(name) && isValid(subname) && isempty(findstr(subname, ':'))
        sub_s = parse_node(childNode, level+1);
        if record
            if isfield(s, subname)
                s.(subname)(length(s.(subname))+1) = sub_s;
            else
                s.(subname) = sub_s;
            end
        end
    end
    childNode = childNode.getNextSibling;
end

end

function r = isValid(name)

r = ~isempty(name) && (strcmp(name, '#document') || name(1) ~= '#');

end

function s = cast_str(str)
num = str2double(str);

if isnan(num)
    s = str;
else
    s = num;
end
end