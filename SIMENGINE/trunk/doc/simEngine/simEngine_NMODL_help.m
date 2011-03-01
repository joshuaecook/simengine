%% MMODL - create ion channels for insertion into Neuron sections
%
%% Description
% <matlab:doc('NMODL') NMODL> files are used by NEURON to define cellular
% mechanisms and ion channels.  In the NEURON for simEngine interface,
% NMODL objects perform the same role.  To insert a mechanism, the file
% must be a function that takes in one argument, an NMODL object, and
% appends data to that object through its methods.
%
%% Methods
%
% * <matlab:doc('NMODL.NMODL') NMODL> - construct and visualize NMODL files
% * <matlab:doc('NMODL.range_parameter') range_parameter> - define a range
% variable accessible to each segment of the section 
% * <matlab:doc('NMODL.global_parameter') global_parameter> - define a
% global variable accessible to the section
% * <matlab:doc('NMODL.current') current> - create an output current quantity
%
%% Properties
% The following quantities are accessible as properties of the NMODL
% object:
%
% * v       - voltage potential in the segment    (mV)
% * v0      - initial segment voltage             (mV)
% * area    - surface area of the segment         (um^2)
% * celsius - temperature of the cell             (C)
% * diam    - diameter of the segment             (um)
% * L       - length of the segment               (um)
% * suffix  - string appended to range variables
%
%% Examples
%
% See a <matlab:edit('pas.m') passive channel> and the <matlab:edit('hh.m') Hodgkin-Huxley channels> NMODL files for
% examples.
%

% Visualize the pas channel
mod = NMODL(@pas);

%%

% Visualize the hh channel
mod = NMODL(@hh);

%% Credits
%
% The Neuron syntax and semantics are based on the NEURON Simulator by
% N.T. Carnevale and M.L. Hines at Yale University (<http://www.neuron.yale.edu>).
%
%% License
%  
% Copyright (c) 2010 Simatra Modeling Technologies
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
