%% Section - Generate a neural section in a Neuron
%
%% Description
% The <matlab:doc('Section') Section> class is used to build sections
% inside <simEngine_Neuron_help.html Neurons>.  A section is a cylindrical model and can be composed of
% one or more segments.  When there are multiple segments, each
% conductance inserted is distributed evenly across the segments.
%
%% Methods
%
% * <matlab:doc('Section.insert') insert> - insert mechanism (an NMODL compliant function) into each
%   segment
%
%% Properties
%
% * dt      - time step of the simulation         (ms)
% * nseg    - number of segments in the section
% * area    - surface area of the section         (um^2)
% * celsius - temperature of the cell             (C)
% * diam    - diameter of the section             (um)
% * L       - length of the section               (um)
% * cm      - section membrane capacitance        (uF/cm^2)
% * Ra      - axonal resistance                   (ohm*cm)
% * ena     - sodium reversal potential           (mV)
% * ek      - potassium reversal potential        (mV)
%
%% See also
% <simEngine_Neuron_help.html Neuron>
%
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