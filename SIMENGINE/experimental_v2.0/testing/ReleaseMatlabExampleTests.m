% ReleaseMatlabExampleTests - create a set of tests to verify that all the
% example m files run without errors
function s = ReleaseMatlabExampleTests

% pull out all the m-files
m_files_str = ls('-1', fullfile(simexamplepath, '/*/*.m'), fullfile(simexamplepath, '/*/*/*.m'));
m_files = strread(m_files_str, '%s', 'delimiter', sprintf('\n'));

% create a suite of tests
s = Suite('Release MATLAB Example Tests');

% create an exclusion list
exclusion_list = {'exploreEleakGleak', 'Capacitor', 'CurrentSource', 'Ground', 'Inductor', 'Resistor', 'VoltageSource'};

% add each of the dsl files to a run script
for i=1:length(m_files)
    [path, name, ext] = fileparts(m_files{i});
    if isempty(name)
        warning('Simatra:ReleaseMatlabExampleTests', 'Unexpected file %s', m_files{i});
    elseif name(1) ~= '.'
        switch name
            case exclusion_list
                % ... don't do anything here
            otherwise
                s.add(Test(['MatlabModel-' name], @()(run_mfile(m_files{i})), '-withouterror'));
        end
    end
end

end

function r = run_mfile(mfile)

disp(['Running: ' mfile]);
run(mfile);
close all;
r = mfile;

end