function [s] = muse_eval (code)
try
    t = evalc(code);
catch err
    t = err.message;
end
nl = char(10);
if t(end) ~= nl; t = [t nl]; end
s = ['<<MUSE<' nl t '>ESUM>>'];
end
