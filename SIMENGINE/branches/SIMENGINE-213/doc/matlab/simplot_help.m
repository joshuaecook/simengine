%% simplot
% 
% Displays and visualizes output data from |SIMEX| simulations.
%
%% Syntax
%
%     SIMPLOT(SIMEXOUT, ...)
%     SIMPLOT(SIMEXOUT, LINESPEC, ...)
%     SIMPLOT(MAT, ...)
%     SIMPLOT(MAT, LINESPEC, ...)
%     SIMPLOT(X, Y, ...)
%     SIMPLOT(X, Y, LINESPEC, ...)
% 
%% Description
%
% Like the |PLOT| command, |SIMPLOT| displays 2D data generated in the
% native structure output format of |SIMEX|.  |SIMPLOT| can also plot
% vectors and X-Y data pairs.
%
% |SIMPLOT| plots simulation output structures generated by |SIMEX|
% in the same manner that the |PLOT| command will plot X-Y data
% pairs.  |SIMPLOT| supports both grouped outputs and multiple
% outputs per structure.
%
% |SIMPLOT(SIMEXOUT, ...)| plots the simulation data structure
% returned by |SIMEX|.
%
% |SIMPLOT(MAT, ...)| plots an N-by-M matrix of data. If |MAT|
% contains one column, the values are plotted against their
% index. If |MAT| contains more than one column, the values of all
% additional columns are plotted against the first.
%
% |SIMPLOT(X, Y, ...)| plots the values of |Y| against those of |X|.
%
% See <matlab:doc('plot') plot> for an explanation of |LINESPEC|
% and other additional parameters.
%
%% Examples
%
% Run a simulation of the Fitzhugh-Nagumo nerve conduction model.

output = simex([simexamplepath '/FN/fn.dsl'], 100)


%%
% |SIMPLOT| can individually plot each of the output traces, which are
% time-value pairs.

figure
subplot(2,1,1); simplot(output.u); title('u trace')
subplot(2,1,2); simplot(output.w); title('w trace')

%%
% |SIMPLOT| can also plot multiple traces in one step, either by specifying
% them twice on the command line, or specifying the entire output structure.

figure
subplot(2,1,1); simplot(output.u, output.w); title('simplot(output.u, output.w)')
subplot(2,1,2); simplot(output); title('simplot(output)')

%%
% Line styles and colors can be added much like they are used in |PLOT|.

figure
simplot(output.u, 'r+', output.w, 'g*')


%% 
close all

%% See also
% <matlab:doc('plot') plot>, <simex.help.html |simex|>,  <simexamplepath_help.html
% |simexamplepath|>

%% 
% Copyright 2009,2010 Simatra Modeling Technologies, L.L.C.
%
% For more information, please visit
% <http://www.simatratechnologies.com>
%
% For additional help, please email
% <mailto:support@simatratechnologies.com support@simatratechnologies.com>