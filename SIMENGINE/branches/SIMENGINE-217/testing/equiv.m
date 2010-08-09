% Equivalency function
function e = equiv(a, b)

% if a or b are .mat files, use the data inside them
if isstr(a) && exist(a, 'file')
    try
        a_data = load(a);
        a = a_data;
    catch
        % couldn't load it, so no worries... treat as a string
    end
end
if isstr(b) && exist(b, 'file')
    try
        b_data = load(b);
        b = b_data;
    catch
        % couldn't load it, so no worries... treat as a string
    end
end
    

e = false;
if isnumeric(a) && isnumeric(b)
    if ndims(a) == ndims(b)
        if size(a) == size(b)
            if all(a == b)
                e = true;
            end
        end
    end
elseif isstr(a) && isstr(b)
    e = strcmp(a,b);
elseif isstruct(a) && isstruct(b)
    if length(a) ~= length(b)
        e = false;
        return;
    end
    % Structures of length > 1 should be compared iteratively
    if length(a) > 1
        for i=1:length(a)
            e = equiv(a(i), b(i));
            if not(e)
                return;
            end
        end
        return;
    end
    afields = fieldnames(a);
    bfields = fieldnames(b);
    if length(afields) == length(bfields)
        % Two empty structs are equivalent
        if length(afields) == 0
          e = true;
          return;
        end
        for i=1:length(afields)
            if isfield(b, afields{i}) 
                e = equiv(a.(afields{i}), b.(afields{i}));
                if not(e) 
                    return;
                end
            else
                e = false;
                return;
            end
        end
    end
elseif islogical(a) && islogical(b)
    e = a == b;
end


end


