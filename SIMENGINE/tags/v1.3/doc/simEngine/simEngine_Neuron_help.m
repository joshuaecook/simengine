%% Neuron - Generate neural morphologies using simEngine
%
%% Description
% The <matlab:doc('Neuron') Neuron> class is used to generate neural
% morphologies following the NEURON paradigm.  The Neuron class is derived
% off the <simEngine_Model_help.m Model> class and retains all of it's
% methods and properties.
%
%% Methods
%
% *Constructor*
%
% * <matlab:doc('Neuron.neuron') Neuron> - create a new Neuron model
% representing a single neuron
%
% *Model construction*
%
% * <matlab:doc('Neuron.section') section> - generate a <simEngine_Section_help.m section> within the
% neuron representing one or more segments
% * <matlab:doc('Neuron.connect') connect> - connect two sections together
% within a neuron
% * <matlab:doc('Neuron.IClamp') IClamp> - generate a
% <simEngine_IClamp_help.html current clamp object> in a section
%
% *Model simulation*
% 
% * <matlab:doc('Model.simex') simex> - execute the simulation
%
%% Properties
%
% * celsius - temperature of the cell
% * dt      - time step of the simulator
%
%% Example
%

n = Neuron('sthA'); % create the Neuron object

soma = n.section('soma'); % add a section called 'soma'
soma.nseg = 1;
soma.diam = 18.8;
soma.L = 18.8; % create a spherical cell when L == diam
soma.Ra = 123;
soma.insert('hh'); % insert hh conductances

stim = n.IClamp(soma, 0.5); % add an IClamp point process
stim.del = 100;
stim.dur = 100;
stim.amp = 0.1;

tstop = 300;
o = n.simex(tstop); % simulate the Neuron model
simplot(o.v_soma);  % and plot the results
title('Somatic Voltage');

%% See also
% <simEngine_Section_help.html Section> <simEngine_IClamp_help.html
% IClamp> <simEngine_NMODL_help.html NMODL> 
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
