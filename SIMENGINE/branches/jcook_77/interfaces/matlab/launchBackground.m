function [status] = launchBackground(command, workingDir, progressLabel)
    logFile = fullfile(workingDir, 'logfile');
    progressFile = fullfile(workingDir, 'progress');
    statusFile = fullfile(workingDir, 'status');
    pidFile = fullfile(workingDir, 'pid');

    system(['touch ' logFile]);
    command = ['(' command ' &>' logFile ' & pid=$! ; echo $pid > ' pidFile ' ; wait $pid; echo $? > ' statusFile ')&'];
    [stat, ignore] = system(command);
    while ~exist(pidFile,'file') || isempty(fileread(pidFile))
        pause(0.1);
    end
    % Ignore the newline
    pid = num2str(str2num(fileread(pidFile)));
    % Remove the file to prevent crosstalk across launchBackground calls
    delete(pidFile);

    c = onCleanup(@()cleanupBackgroundProcess(pid));

    outputlen = 0;
    messagelen = 0;
    while(processRunning(pid))
        if(~exist('m','var') && exist(progressFile,'file'))
            m = memmapfile(progressFile, 'format', 'double');
        end
        if(exist('m','var'))
            progress = 100*sum(m.Data)/length(m.Data);
            message = sprintf([progressLabel ': %0.2f %%'], progress);
            messagelen = statusBar(message, messagelen);
        end
        try
            log = fileread(logFile);
        catch it
            simFailure('launchBackground', 'Process log file does not exist.')
        end
        if length(log) > outputlen
            fprintf('%s', log(outputlen+1:end));
            outputlen = length(log);
        else
            pause(0.1);
        end
    end
    try
        log = fileread(logFile);
    catch it
        simFailure('launchBackground', 'Process log file does not exist.')
    end
    if length(log) > outputlen
        fprintf('%s', log(outputlen+1:end));
    end
    try
        status = str2num(fileread(statusFile));
        % Prevent any crosstalk between launchBackground calls
        delete(statusFile);
        if(exist(progressFile,'file'))
            messagelen = statusBar('', messagelen);
            delete(progressFile);
        end
        delete(logFile);
    catch it
        simFailure('launchBackground', 'Process status file does not exist.')
    end
end

function [running] = processRunning(pid)
    [stat, ignored] = system(['ps -p ' pid ' -o pid=']);
    running = not(stat);
end

function cleanupBackgroundProcess(pid)
% kill is called unconditionally, on CTRL+C the simulation is stopped
% For normal execution, the process will have exited and the kill won't do anything
    command = sprintf('kill -9 %s', pid);
    [stat, result] = system(command);
    if ~stat
        disp('User terminated simulation.')
    end
end
