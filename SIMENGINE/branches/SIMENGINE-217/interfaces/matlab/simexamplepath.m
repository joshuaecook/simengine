function path = simexamplepath
% SIMEXAMPLEPATH - return the path to the simEngine installed
%   example and demo directory
%
% Copyright 2010 Simatra Modeling Technologies
%

% grab the simex path
simexpath = which('simex');
[installpath,filename,ext] = fileparts(simexpath);

% determine the example path
path = fullfile(installpath, 'examples');

end
