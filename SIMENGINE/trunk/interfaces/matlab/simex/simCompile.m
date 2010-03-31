function [status] = simCompile (options, progressLabel)
%  SIMCOMPILE invokes simEngine to produce a compiled simulation
    command = [options.simengine ' --simex ' options.model ...
               ' --outputdir ' options.outputs ' ' options.args];
    if options.debug
        disp(['Running <' command '>'])
    end
    status = launchBackground(command, options.outputs, progressLabel);
    if(128 == status)
        simEngineError('simCompile', ['Model ' options.dslfile ' can not be '...
                            'compiled']);
    elseif (0 ~= status)
        simFailure('simCompile', ['SimEngine internal error.']);    
    end
end
