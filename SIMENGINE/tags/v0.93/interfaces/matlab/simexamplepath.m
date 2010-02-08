function path = simexamplepath

% grab the buildEngine path
buildenginepath = which('simex');
[installpath,filename,ext] = fileparts(buildenginepath);

% determine the example path
path = fullfile(installpath, 'examples');

end
