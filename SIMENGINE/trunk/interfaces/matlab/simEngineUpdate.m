% SIMENGINEUPDATE
%  Downloads and installs the latest version of simEngine
%
function simEngineUpdate

% Run simCheckVersion
latest = simCheckVersion('-quiet');

% Grab the license information
license = simCheckLicense('-SIMATRAINTERNALCOMMAND!!!');

% Pull does the lastest version
status = downloadSimEngine(latest);
if status
    [file_path, fcn_name, ext] = fileparts(latest.file);
    try
        feval(fcn_name);
        delete(latest.file);
    catch
        delete(latest.file);
    end
else
    error('Simatra:simEngineUpdate', '')
end
end


% Download the latest version
function status = downloadSimEngine(latest)
    % Grab the download location
    url_from_simEngine = execSimEngine('settings.installation.updateURL.getValue()', true);
    if url_from_simEngine
        if 10 == double(url_from_simEngine(end))
            url_from_simEngine = url_from_simEngine(1:(end-1));
        end
        url = url_from_simEngine;
    else
        site = 'http://www.simatratechnologies.com';
        url = [site '/images/simEngine/' version_filename];
    end

    
    % Estimate how long it will take
    x = [100 500];
    y = [0 0];
    tic
    [s, stat] = urlread([url '/OneHundredK.p']);
    y(1) = toc;
    tic
    [s, stat] = urlread([url '/FiveHundredK.p']);
    y(2) = toc;
    est_time = interp1(x, y, latest.size/1024, 'linear', 'extrap');
    disp(sprintf('Downloading v%g.%g%s (will take approximately %0.1f s)', latest.major, latest.minor, latest.revision, est_time));
    [f, status] = urlwrite([url '/' latest.file], latest.file);
    
end


% Run simEngine command and return result
function output = execSimEngine(cmd, quiet, varargin)

if nargin > 2
  additional_options = varargin{1};
else
  additional_options = '';
end

TOKEN = 'STARTING HERE:';
[pathstr, name, ext] = fileparts(which('simex'));
simEngine = fullfile(pathstr, 'bin', 'simEngine');
print_cmd = ['println(\"' TOKEN '\" + ' cmd ')'];
echo_cmd = ['echo "' print_cmd '"'];
arguments = [' -batch - -startupmessage=false ' additional_options];

full_cmd = [echo_cmd ' | ' simEngine arguments];

[status, result] = system(full_cmd);

% chomp the trailing new line
if 10 == double(result(end))
    result = result(1:(end-1));
end

% pull out the last part
last_line = result(findstr(result, TOKEN):end);
output = regexprep(last_line, TOKEN, '');

% % DEBUG INFO
%if not(quiet)
%    disp(['Default download location: ' result]);
%end
%

if status ~= 0 || ~isempty(strfind(result, 'ERROR')) || ~isempty(strfind(result, 'FAILURE'))
    if not(quiet)
        disp(['Command: ' full_cmd]);
        disp(['Status: ' num2str(status)]);
        disp(['Result: ' result]);
        warning('Simatra:simEngineUpdate', 'simEngine did not execute properly');
    end
    result = false;
end

end
