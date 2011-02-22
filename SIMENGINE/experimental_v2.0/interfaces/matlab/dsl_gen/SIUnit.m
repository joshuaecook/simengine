classdef SIUnit < GenericUnit
   
    properties
        prefix
        short
        factor
        base
    end
    
    methods
        function u = SIUnit(prefix, unit, name, id)
            u.base = unit;
            u.prefix = prefix;
            u.short = prefixToShort(prefix);
            u.factor = prefixToScale(prefix);
            if nargin < 3
                u.name = [prefix unit.name];
            else
                u.name = name;
            end
            if nargin < 4
                u.id = [u.short unit.id];
            else
                u.id = id;
            end
            u.data = struct('unit', unit, 'prefix', u.factor, 'exp', 1);
        end
        
        function disp(u)
            disp(sprintf('Derived Unit: %s (%s)', u.name, u.id))
        end

    end
    
end

function s = prefixToShort(prefix)

switch lower(prefix)
    case 'yotta'
        s = 'Y';
    case 'zetta'
        s = 'Z';
    case 'exa'
        s = 'E';
    case 'peta'
        s = 'P';
    case 'tera'
        s = 'T';
    case 'giga'
        s = 'G';
    case 'mega'
        s = 'M';
    case 'kilo'
        s = 'k';
    case 'hecto'
        s = 'h';
    case 'deca'
        s = 'da';
    case 'deci'
        s = 'd';
    case 'centi'
        s = 'c';
    case 'milli'
        s = 'm';
    case 'micro'
        s = 'u';
    case 'nano'
        s = 'n';
    case 'pico'
        s = 'p';
    case 'femto'
        s = 'f';
    case 'atto'
        s = 'a';
    case 'zepto'
        s = 'z';
    case 'yocto'
        s = 'y';
    otherwise
        error('Simatra:SIUnit', 'Unrecognized prefix %s', prefix);
end
             
end

function scale = prefixToScale(prefix)

switch lower(prefix)
    case 'yotta'
        s = 24;
    case 'zetta'
        s = 21;
    case 'exa'
        s = 18;
    case 'peta'
        s = 15;
    case 'tera'
        s = 12;
    case 'giga'
        s = 9;
    case 'mega'
        s = 6;
    case 'kilo'
        s = 3;
    case 'hecto'
        s = 2;
    case 'deca'
        s = 1;
    case 'deci'
        s = -1;
    case 'centi'
        s = -2;
    case 'milli'
        s = -3;
    case 'micro'
        s = -6;
    case 'nano'
        s = -9;
    case 'pico'
        s = -12;
    case 'femto'
        s = -15;
    case 'atto'
        s = -18;
    case 'zepto'
        s = -21;
    case 'yocto'
        s = -24;
    otherwise
        error('Simatra:SIUnit', 'Unrecognized prefix %s', prefix);
end

scale = 10^s;
             
end
