% Approx Equivalency function
% e = approx_equiv(a, b, tol)
%   a,b: number, string, or .mat file
%   tol: percent above and below 'a'
function e = approx_equiv(a, b, tol)

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
            a_max = a.*(1+tol/100);
            a_min = a.*(1-tol/100);
            if all((a_max >= b & a_min <= b) | (a_max <= b & a_min >= b))
                e = true;
            end
        end
    end
elseif isstr(a) && isstr(b)
    e = strcmp(a,b);
elseif isstruct(a) && isstruct(b)
    afields = fieldnames(a);
    bfields = fieldnames(b);
    if length(afields) == length(bfields)
        for i=1:length(afields)
            if isfield(b, afields{i}) 
                e = approx_equiv(a.(afields{i}), b.(afields{i}),tol);
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


