function e = all_equiv(sa)
% all_equiv Compares all copies of a struct to see if they are equivalent

if ~isstruct(sa)
    e = false;
    return;
end

if length(sa) == 1
    e = true;
    return;
end

for i=2:length(sa)
    e = equiv(sa(1),sa(2));
    if not(e)
        return;
    end
end

e = true;
return;