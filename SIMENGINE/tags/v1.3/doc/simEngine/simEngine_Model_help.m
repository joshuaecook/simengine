%% Model - DIESEL for MATLAB modeling framework
%
%% Description
% The <matlab:helpwin('Model') Model> class programmatically builds DSL models for simEngine. 
%
%% Methods
% *Constructor*
%
% * <matlab:doc('Model.model') Model> - create a new model object
%
% *Model Elements* 
%
% * <matlab:doc('Model.input') input> - define a model input
% * <matlab:doc('Model.state') state> - define a state of the system
% * <matlab:doc('Model.output') output> - define a model output
% * <matlab:doc('Model.random') random> - define a random number
% * <matlab:doc('Model.equ') equ> - define an intermediate equation
% * <matlab:doc('Model.diffequ') diffequ> - define a differential equation
% * <matlab:doc('Model.recurrenceequ') recurrenceequ> - define a recurrence (difference) equation
% * <matlab:doc('Model.update') update> - define when a state variable updates
% * <matlab:doc('Model.submodel') submodel> - instantiate a sub model
%
% *Model Query*
%
% * <matlab:doc('Model.time') time> - return the default iterator as an expression
% * <matlab:doc('Model.timeIterator') timeIterator> - return the default iterator as an iterator
% * <matlab:doc('Model.interface') interface> - return a listing of the inputs and outputs
%
% *Model Simulation*
%
% * <matlab:doc('Model.simex') simex> - execute the simulation of the model
%
% *Model Processing*
%
% * <matlab:doc('Model.type') type> - display the generated model
% * <matlab:doc('Model.toDSL') toDSL> - write the model to a file
% * <matlab:doc('Model.disp') disp> - display information about the model
%
%% Properties
%
% * solver - choose the solver for the simulation
% * dt - specify the time step, if it's a fixed time step solver
%
