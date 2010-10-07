function [e] = simError(product, id, msg)
%
    
% exn = MException(['Simatra:' product ':' id], msg);
% exn.throw
    stack = struct('file', which('simex'), 'name', 'simex', 'line', 1);
    e = struct('identifier', ['Simatra:' product ':' id], 'message', msg, ...
               'stack', stack);
    error(e);
end
