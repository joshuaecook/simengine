% RELEASESIMULATETESTS runs a simulation battery of tests
% 
% Usage
%   RELEASESIMULATETESTS('-plot') will output all the results as plots
%   RELEASESIMULATETESTS('-create') will create template mat files
%   s = RELEASESIMULATETESTS will return the test suite
%
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
%
function s = ReleaseSimulateTestsGPU(varargin)

suitename = 'Release Simulate Tests GPU';
templatedir = 'ReleaseSimTestsExp';

RUNTESTS = 0;
PLOT = 1;
CREATE = 2;

% Create the return argument
s = Suite(suitename);


% Process arguments
if nargin == 0 
    mode = RUNTESTS;
elseif strcmpi(varargin{1}, '-plot')
    mode = PLOT;
elseif strcmpi(varargin{1}, '-create')
    mode = CREATE;
else
    error('simEngine:ReleaseSimulateTests', 'Unexpected argument, expecting none, -plot, or -create');
end

% Create template directory if it doesn't exist
if not(exist(templatedir, 'dir'))
    mkdir(templatedir);
end

% Create test list
testInfos = createTestList;

% Run the tests
for i=1:length(testInfos)
    info = testInfos(i);

    % do different tasks depending on the mode
    switch mode
        case PLOT,
            disp(['Simulating and plotting ' info.name]);
            if isempty(info.states)
                o = simex(info.model, info.time, info.inputs);
            else
                o = simex(info.model, info.time, info.inputs, info.states);
            end
            figure;
            simplot(reduceDataSet(o));
            title(sprintf('Plot of ''%s''',info.name));
        case CREATE,
            if isempty(info.states)
                o = simex(info.model, info.time, info.inputs);
            else
                o = simex(info.model, info.time, info.inputs, info.states);
            end
            o_reduced = reduceDataSet(o);
            matfile = fullfile(templatedir, [info.name '_exp.mat']);
            save(matfile, '-struct', 'o_reduced');
            disp(['Created expected results for model ' info.name ' in ' matfile]);
        case RUNTESTS,
            % create function handle to run simulation
            if isempty(info.states)
                f = @()(reduceDataSet(simex(info.model, info.time, ...
                                            info.inputs, '-gpu')));
            else
                f = @()(reduceDataSet(simex(info.model, info.time, ...
                                            info.inputs, info.states, '-gpu')));
            end
            matfile = fullfile(templatedir, [info.name '_exp.mat']);
            s.add(Test(info.name, f, '-approxequal', matfile, 5));
    end
end



end


% Create a list of tests to run through the system
function t = createTestList()

i = 0;

% FN Model
i = i + 1;
t(i) = struct();
t(i).name = 'FN';
t(i).model = fullfile(simexamplepath, 'FN/fn.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 100;

% HH Model
i = i + 1;
t(i).name = 'HH';
t(i).model = fullfile(simexamplepath, 'HH/hh.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 100;

% BRK Model Iext = 10
i = i + 1;
t(i).name = 'BRK_I10';
t(i).model = fullfile(simexamplepath, 'BRK/brk.dsl');
t(i).inputs = struct('Iext', 10);
t(i).states = [];
t(i).time = 100;

% BRK Model Iext = 30
i = i + 1;
t(i).name = 'BRK_I30';
t(i).model = fullfile(simexamplepath, 'BRK/brk.dsl');
t(i).inputs = struct('Iext', 30);
t(i).states = [];
t(i).time = 100;

% STG Model
i = i + 1;
t(i).name = 'PD';
t(i).model = fullfile(simexamplepath, 'PD/pd.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 1500;

% Timing Network Model (HN)
% Have to remove this test - it's just too sensitive to variations
% across processors
%i = i + 1;
%t(i).name = 'HN';
%t(i).model = fullfile(simexamplepath, 'HN/timingNetwork.dsl');
%t(i).inputs = struct();
%t(i).states = [];
%t(i).time = 10;

% Leaky Membrane
i = i + 1;
t(i).name = 'Leaky Membrane';
t(i).model = fullfile(simexamplepath, 'tutorial/leakyMembrane.dsl');
t(i).inputs = struct('Iext',0);
t(i).states = [];
t(i).time = 1;

% Two Cell Network (Tutorial)
i = i + 1;
t(i).name = 'Two Cell Network';
t(i).model = fullfile(simexamplepath, 'tutorial/twoCellNetwork.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 100;

% Van der Pol
i = i + 1;
t(i).name = 'Van der Pol';
t(i).model = fullfile(simexamplepath, 'VDP/vdpex.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 3000;

% Yeast
i = i + 1;
t(i).name = 'Yeast';
t(i).model = fullfile(simexamplepath, 'yeast/yeast.dsl');
t(i).inputs = struct();
t(i).states = [];
t(i).time = 1;


end

