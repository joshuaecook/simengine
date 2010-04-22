%% simexamplepath
% Returns the path to the simEngine example models and demos
%
%
%% Usage
%      SIMEXAMPLEPATH - returns the path to the included examples and demos
%
%% Examples
%
% View the included DSL model files that can be compiled and simulated with
% SIMEX

ls('-1',[simexamplepath '/*/*.dsl'])

%%
% View and run the included MATLAB(TM) demos

ls('-1',[simexamplepath '/demos/*/*.m'])


%% See also
% <simex_help.html |simex|>,  <simplot_help.html |simplot|>

%% 
% Copyright 2009,2010 Simatra Modeling Technologies, L.L.C.
% For more information, please visit <http://www.simatratechnologies.com>
% For additional help, please email
% <mailto:support@simatratechnologies.com>