function hh(mdl)

mdl.suffix = 'hh';

% Create parameters
ena = mdl.global_parameter('ena');
ek = mdl.global_parameter('ek');
gnabar = mdl.range_parameter('gnabar', 0.12);
gkbar = mdl.range_parameter('gkbar', 0.036);
gl = mdl.range_parameter('gl', 0.0003);
el = mdl.range_parameter('el', -54.3);

% Pull out default values
v = mdl.v;
v0 = mdl.v0;
celsius = mdl.celsius;

    function varargout = rates(v)
        q10 = 3^((celsius - 6.3)/10);
        %"m" sodium activation system
        alpha = .1 * vtrap(-(v+40),10);
        beta =  4 * exp(-(v+65)/18);
        sum = alpha + beta;
        mtau = 1/(q10*sum);
        minf = alpha/sum;
        %"h" sodium inactivation system
        alpha = .07 * exp(-(v+65)/20);
        beta = 1 / (exp(-(v+35)/10) + 1);
        sum = alpha + beta;
        htau = 1/(q10*sum);
        hinf = alpha/sum;
        %"n" potassium activation system
        alpha = .01*vtrap(-(v+55),10);
        beta = .125*exp(-(v+65)/80);
        sum = alpha + beta;
        ntau = 1/(q10*sum);
        ninf = alpha/sum;
        
        if nargout == 3
            varargout = {minf, hinf, ninf};
        elseif nargout == 6
            varargout = {minf, hinf, ninf, mtau, htau, ntau};
        end
    end

function z = vtrap(x,y)
    if isnumeric(x) && isnumeric(y)
        if (abs(x/y) < 1e-6)
                z = y*(1 - x/y/2);
        else
                z = x/(exp(x/y) - 1);
        end
    else
        z = piecewise(y*(1 - x/y/2), abs(x/y) < 1e-6, ...
                      x/(exp(x/y) - 1));
    end
end


% Initialize the states
[minf0, hinf0, ninf0] = rates(v0);
m = mdl.state('m', minf0);
h = mdl.state('h', hinf0);
n = mdl.state('n', ninf0);

% Evaluate the differential equations
[minf, hinf, ninf, mtau, htau, ntau] = rates(v);
mdl.diffequ(m, (minf-m)/mtau);
mdl.diffequ(h, (hinf-h)/htau);
mdl.diffequ(n, (ninf-n)/ntau);

% Evaluate the breakpoint
gna = gnabar*m^3*h;
ina = gna*(v-ena);
gk = gkbar*n^4;
ik = gk*(v-ek);
il = gl*(v-el);

% Return currents
mdl.current(ina, 'na');
mdl.current(ik, 'k');
mdl.current(il);

end