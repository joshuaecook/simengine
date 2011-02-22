% Approx Equivalency function
% e = approx_equiv(a, b, tol)
%   a,b: number, string, or .mat file
%   tol: percent above and below 'a' (treat not an a per element
%   basis but instead base it on the global mins/maxes
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
          overall_max = max(a);
          overall_min = min(a);
          allowed_error = (overall_max-overall_min)*tol/100;
          % if there is only one value, the allowed_error will be
          % zero, therefore, take tol as a relative percentage.
          allowed_error(find(allowed_error==0)) = ...
              a(find(allowed_error==0))*tol/100;
          accepted_error = ones(length(a),1)*allowed_error;
          a_max = a+accepted_error;
            a_min = a-accepted_error;
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


