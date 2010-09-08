classdef IClamp < Model
% IClamp - current clamp point process for a Neuron model
% 
% IClamp Properties:
%   del - deletion time before the current clamp begins
%   dur - duration of the curent clamp
%   amp - amplitude of the current clamp
%
%  The Neuron syntax and semantics are based on the NEURON Simulator by
%  N.T. Carnevale and M.L. Hines at Yale University (www.neuron.yale.edu).
%  
%  Copyright (c) 2010 Simatra Modeling Technologies
%
%  Permission is hereby granted, free of charge, to any person obtaining a copy
%  of this software and associated documentation files (the "Software"), to deal
%  in the Software without restriction, including without limitation the rights
%  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%  copies of the Software, and to permit persons to whom the Software is
%  furnished to do so, subject to the following conditions:
% 
%  The above copyright notice and this permission notice shall be included in
%  all copies or substantial portions of the Software.
% 
%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%  THE SOFTWARE.
   
    properties
        dt = 0.01
        del = 0    % deletion time before the current clamp begins
        dur = 0    % duration of the curent clamp
        amp = 0    % amplitude of the current clamp
    end
    
    properties (Access = protected)
        t_exp
    end
    
    methods
        function m = IClamp(t_exp)
            [path, file, ext] = fileparts(tempname);
            
            % create a unique name
            id = ['IClamp_' file];
            
            % construct the model object
            m@Model(id);

            if 0 == nargin
                % create an iterator
                t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', m.dt);
            end
            
            m.DefaultIterator = t_exp;
            m.t_exp = t_exp;
            
            % build now, but we'll build again when we produce DSL code
            build(m);
        end
        
        function s = toStr(m)
            initializeModel(m);
            build(m); % build the model
            s = toStr@Model(m); % now, create the string representation
        end
        
    end
    
    methods (Access = protected)
        function build(m)
            equ = piecewise(m.amp, (m.time > m.del) & (m.time <= m.del + m.dur), 0);
            m.output('I', equ);
        end
    end
    
end