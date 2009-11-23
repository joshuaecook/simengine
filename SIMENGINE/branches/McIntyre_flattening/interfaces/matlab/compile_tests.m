function compile_tests

% grab the buildEngine path
buildenginepath = which('buildEngine');
[installpath,filename,ext] = fileparts(buildenginepath);

% determine the example path
examplepath = fullfile(installpath, 'examples');

dsl_files = dir(fullfile(examplepath,'*.dsl'));

error_list = {};

run_count = 0;
error_count = 0;
success_count = 0;
for i=1:length(dsl_files)
    run_count = run_count + 1;
    [path, name, ext] = fileparts(dsl_files(i).name);
    disp(sprintf('Running model: %s (test %d of %d)',name,run_count,length(dsl_files)));
    success = false;
    if in_octave
      try
        filename = fullfile(examplepath, [name ext]);
        s = buildEngine(filename);
        success = true;
        success_count = success_count + 1;
        disp('SUCCESS');
      catch
        disp('ERROR FOUND');
        error_count = error_count + 1;
        error_list{length(error_list)+1} = struct('name',name);          
      end
    else
      try
        filename = fullfile(examplepath, [name ext]);
        s = buildEngine(filename);
        success = true;
        success_count = success_count + 1;
        disp('SUCCESS');
      catch me
        disp('ERROR FOUND');
        error_count = error_count + 1;
        error_list{length(error_list)+1} = struct('name',name, ...
                                                  'me',me);
      end
    end
end

if error_count > 0
    for i=1:length(error_list)
      if in_octave
        disp(sprintf(['Error encountered when running %s'], error_list{i}.name))      
      else
        disp(sprintf(['Error encountered when running %s: ''%s'', ' ...
                      '%s'], error_list{i}.name, error_list{i}.me.identifier, error_list{i}.me.message));
      end
    end
    disp(sprintf('Errors found!!! %d/%d tests passed', success_count, run_count));
else
    disp(sprintf('All success!!!  %d tests passed', success_count));
end

end

% check if in octave
function y = in_octave

y = exist('octave_config_info');

end


