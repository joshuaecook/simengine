function [e] = simError(product, id, msg)
%
    
% exn = MException(['Simatra:' product ':' id], msg);
% exn.throw
    st = dbstack;
    stack = st;
    for i=1:length(st)
        % seek out when we first find simex, then we can use that to see
        % the remaining stack trace
        if strcmp(st(i).name, 'simex')
            stack = st(i:end);
            break;
        end
    end
    
    %stack = struct('file', which('simex'), 'name', 'simex', 'line', 1);
    e = struct('identifier', ['Simatra:' product ':' id], 'message', msg, ...
               'stack', stack);
    error(e);
end
