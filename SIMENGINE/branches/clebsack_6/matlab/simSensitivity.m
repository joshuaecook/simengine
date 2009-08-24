%% simSensitivity - run a sensitivity analysis around a given dsl file
function s = simSensitivity(dslfile, varargin)

% Parse all the arguments, returning a parse structure
p = parseArgs(dslfile, varargin);
    
% First, run simex and get the return data structure of the model
m = runSimex(dslfile);

% Next, verify that the arguments are all good
args = verifyArgs(m, p.Results);

% Run the sensitivity analysis over the final states or metrics
s = runSensitivity(args);

end


%% parseArgs - parse all the arguments to the function
function p = parseArgs(dslfile, args)

p = inputParser; % Create an instance of the inputParser
p.addRequired('dslfile', @ischar);
p.addRequired('time', @isnumeric);
p.addOptional('target', 'openmp', @(x)any(strcmpi(x,{'openmp','gpu'})))
p.addParamValue('precision', 'single', @(x)any(strcmpi(x,{'single','float','double'})))
p.addParamValue('inputs', struct(), @isstruct)
p.addParamValue('states', [], @isnumeric)
p.addParamValue('perturbation', 1, @isnumeric)
p.addParamValue('input_mode', 'states', @(x)any(strcmpi(x,{'states','inputs'})))
p.addParamValue('output_mode', 'final_state', @(x)any(strcmpi(x,{'final_state','metrics'})))
p.addParamValue('metrics', {}, @iscell)

p.parse(dslfile, args{:});

% Display all arguments.
disp ' '
disp 'List of all arguments:'
disp(p.Results)

end

%% runSimex
function m = runSimex(dslfile)

m = simex(dslfile)

end


%% verifyArgs
function new_args = verifyArgs(m, args)

% write args to the new_args
new_args = args;

% update states
if isempty(args.states) 
    new_args.states = m.default_states;
elseif length(args.states) ~= length(m.default_states) 
    error('Simatra:simSensitivity:argError', 'States have wrong vector length (received %d, should be %d))', length(args.states), length(m.default_states));
end

if isempty(fieldnames(args.inputs))
    new_args.inputs = m.default_inputs;
else
    new_args.inputs = m.default_inputs;
    fields = fieldnames(args.inputs);
    for i=1:length(fields)
        if isfield(new_args.inputs, fields{i})
            new_args.inputs.(fields{i}) = args.inputs.(fields{i});
        else
            error('Simatra:simSensitivity:argError', 'Invalid field %s in input list', fields{i});
        end
    end
end
    
end


%% sensitivityFinalState
function s = runSensitivity(args)

% Create the input and state structures and vectors, respectively
if strcmpi(args.input_mode,'inputs')
    inputs = permuteInputs(args);
    states = args.states;
elseif strcmpi(args.input_mode,'states')
    states = permuteStates(args);
    inputs = args.inputs;
end

% execute simex to perform the parallel computation
[o, final_states] = simex(args.dslfile, args.time, ['-' args.target], ['-' args.precision], inputs, states);

% generate the output sensitivities
if strcmpi(args.output_mode, 'final_state')
    s = computeFinalStateSensitivities(args, final_states);
else
    s = [];
end

end

%% permuteInputs
function inputs = permuteInputs(args)

count = 1+2*length(fieldnames(args.inputs));
perturb = args.perturbation/100;

% allocate the initial inputs
fields = fieldnames(args.inputs);
inputs = struct();
for i=1:length(fields)
    inputs.(fields{i}) = ones(count,1) * args.inputs.(fields{i});
end

% go through each input and update accordingly
for j=1:length(fields)
    a = inputs.(fields{i});
    a(1+2*(j-1)) = args.inputs.(fields{i})*(1.00+perturb);
    a(2+2*(j-1)) = args.inputs.(fields{i})*(1.00-perturb);
    inputs.(fields{i}) = a;
end


end

%% permuteStates
function states = permuteStates(args)

count = 1+2*length(args.states);
perturb = args.perturbation/100;

% allocate the initial states
states = (args.states' * ones(1,count))';

% go through each state and update
for i = 1:length(args.states)
   states(1+2*(i-1),i) = args.states(i)*(1.00+perturb);
   states(2+2*(i-1),i) = args.states(i)*(1.00-perturb);   
end

end


%% computeFinalStateSensitivities
function s = computeFinalStateSensitivities(args, final_states);

count = size(final_states,1);
perturb = args.perturbation/100;
statemode = strcmpi(args.input_mode, 'states');

if statemode 
    s = zeros(length(args.states),size(final_states,2));
else
    s = zeros(length(fieldnames(args.inputs)),size(final_states,2));
end

% go first for state mode
if statemode
   for i=1:length(args.states)
       x = [args.states(i)*(1.00-perturb) args.states(i) args.states(i)*(1.00+perturb)];
       for j=1:length(args.states)
           y = [final_states(2+2*(j-1),i) final_states(count,i) final_states(1+2*(j-1),i)];
           p = polyfit(x,y,1);
           %disp(sprintf('(%d, %d): x=[%g,%g,%g],y=[%g,%g,%g], line=%g * x + %g]\n', i, j, x(1), x(2), x(3), y(1), y(2), y(3), p(1), p(2)));
           s(i,j)=p(1);
       end
   end
else
    fields = fieldnames(args.params);
    for i=1:length(fields)
        val = args.params.(fields{i});
        x = [val*(1.00-perturb) val val*(1.00+perturb)];
       for j=1:length(args.states)
           y = [final_states(2+2*(j-1),i) final_states(count,i) final_states(1+2*(j-1),i)];
           p = polyfit(x,y,1);
           %disp(sprintf('(%d, %d): x=[%g,%g,%g],y=[%g,%g,%g], line=%g * x + %g]\n', i, j, x(1), x(2), x(3), y(1), y(2), y(3), p(1), p(2)));
           s(i,j)=p(1);
       end
   end
end


end













