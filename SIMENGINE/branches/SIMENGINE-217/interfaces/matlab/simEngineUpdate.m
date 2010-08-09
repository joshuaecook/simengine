function simEngineUpdate(varargin)
% SIMENGINEUPDATE
%  Downloads and installs the latest version of simEngine
%
% Copyright 2010 Simatra Modeling Technologies
%

% Run simCheckVersion
latest = simCheckVersion('-quiet');
if ~isstruct(latest)
  error('Simatra:simEngineUpdate', ['Unable to find simEngine online.  ' ...
                      'Please download the new version of simEngine '...
                      'at www.simatratechnologies.com/simEngine.html.'])
end

% Grab the license information
license = simCheckLicense('-SIMATRAINTERNALCOMMAND!!!');
% we can ignore the license info here

% Pull does the lastest version
status = downloadSimEngine(latest);
if status
    [file_path, fcn_name, ext] = fileparts(latest.file);
    try
        feval(fcn_name);
        delete(latest.file);
    catch e
      disp(sprintf('While upgrading, caught error: %s\n', e.message));
      disp('Upgrade has failed.  Please try again or contact Simatra Technologies for support.');
      if exist(latest.file) ~= 0        
        delete(latest.file);
      end
    end
else
    error('Simatra:simEngineUpdate', '')
end
end


% Download the latest version
function status = downloadSimEngine(latest)
    % Grab the download location
    url = execSimEngine('settings.installation.updateURL.getValue() + "?" + settings.installation.updateQuery.getValue()', true);
    if ~url
      url = 'http://www.simatratechnologies.com/download-install_simEngine.php';
    end

    disp(sprintf(['Downloading v%g.%g%s'], latest.major, latest.minor, latest.revision));
    [f, status] = urlwrite(url, latest.file);
    
end


% Run simEngine command and return result
function result = execSimEngine(cmd, quiet)

[pathstr, name, ext] = fileparts(which('simex'));
simEngine = fullfile(pathstr, 'bin', 'simEngine');
print_cmd = ['println(' cmd ')'];
echo_cmd = ['echo ''' print_cmd ''''];
arguments = [' -batch - -startupmessage=false'];

full_cmd = [echo_cmd ' | ' simEngine arguments];

[status, result] = system(full_cmd);

% chomp the trailing new line
if 10 == double(result(end))
    result = result(1:(end-1));
end

% % DEBUG INFO
%if not(quiet)
%    disp(['Default download location: ' result]);
%end
%

if status ~= 0
    if not(quiet)
        disp(['Command: ' full_cmd]);
        disp(['Status: ' num2str(status)]);
        disp(['Result: ' result]);
        warning('Simatra:simCheckVersion', ['simEngine did not execute properly']);
    end
    result = false;
end

end
