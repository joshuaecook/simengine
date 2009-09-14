% Equivalency function
function e = equiv(a, b)

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
    afields = fieldnames(a);
    bfields = fieldnames(b);
    if length(afields) == length(bfields)
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


