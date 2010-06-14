% SOLVERTESTS runs a model across each solver and verifies the results
%
% Usage
%   SOLVERTESTS('-plot') will output all the results as plots
%   SOLVERTESTS('-create') will create template mat files
%   s = SOLVERTESTS will return the test suite
%
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
%
function s = SolverTests(varargin)

suitename = 'Solver Tests';
templatedir = 'SolverTestsExp';

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

% Run just one model, FN, across each of the solvers
solvers = {'forwardeuler', 'rk4', 'ode23', 'ode45', 'expeuler', 'cvode', ...
           'cvode_stiff', 'cvode_nonstiff', 'cvode_diag', 'cvode_tridiag'};
precisions = {'single', 'double'};
for i=1:length(solvers)
    solver = solvers{i};
    for j=1:length(precisions)
        precision = precisions{j};
        name = ['fn_' solver '_' precision];
        model = ['models_SolverTests/fn_' solver '.dsl'];
        time = 100;
        
        % do different tasks depending on the mode
        switch mode
            case PLOT,
                disp(['Simulating and plotting ' name]);
                o = simex(model, time);
                figure;
                simplot(reduceDataSet(o));
                title(sprintf('Plot of ''%s'' (%s)',['fn: ' solver], precision));
            case CREATE,
                o = simex(model, time, ['-' precision]);
                o_reduced = reduceDataSet(o);
                matfile = fullfile(templatedir, [name '_exp.mat']);
                save(matfile, '-struct', 'o_reduced');
                disp(['Created expected results for model ' name ' in ' matfile]);
            case RUNTESTS,
                % create function handle to run simulation
                f = @()(reduceDataSet(simex(model, time, ['-' precision])));
                matfile = fullfile(templatedir, [name '_exp.mat']);
                s.add(Test(name, f, '-approxequal', matfile, 5));
        end
    end


end


end


function reduced = reduceDataSet(dataset)

num_points = 100;
fields = fieldnames(dataset);
reduced = struct();
for i=1:length(fields)
    data_mat = dataset.(fields{i});
    if size(data_mat,1) > num_points
        if size(data_mat,2) == 1
            reduced.(fields{i}) = interp1(1:size(data_mat,1),data_mat,1:num_points);
        else
            min_t = data_mat(1,1);
            max_t = data_mat(end,1);
            new_data_mat = zeros(num_points,size(data_mat,2));
            new_data_mat(:,1) = linspace(min_t,max_t,num_points);
            for j=2:(size(data_mat,2))
                new_data_mat(:,j) = interp1(data_mat(:,1),data_mat(:,j),new_data_mat(:,1));
            end
            reduced.(fields{i}) = new_data_mat;
        end
    else
        reduced.(fields{i}) = dataset.(fields{i});
    end
end

end