function [status] = simCompile (options, progressLabel)
%  SIMCOMPILE invokes simEngine to produce a compiled simulation
    command = [options.simengine ' --inferior-mode --simex ' options.model ...
               ' --outputdir ' options.outputs ' ' options.args];
    if options.debug
        disp(['Running <' command '>'])
    end
    status = launchBackground(command, options.outputs, progressLabel);
    if(128 == status)
        simEngineError('simCompile', ['Model ' options.dslfile ' can not be '...
                            'compiled']);
    elseif (129 == status)
        disp(['Please execute ''help simex'' for information on using '...
              'simEngine in MATLAB.'])
        simEngineError('simCompile', ['Can not execute simEngine due to invalid command line options'])
    elseif (0 ~= status)
        simFailure('simCompile', ['SimEngine internal error.']);    
    end
end
