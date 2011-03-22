%% Iterator - DIESEL for MATLAB Iterator class
%
%% Description
% The <matlab:doc('Iterator') Iterator> class builds iterators, or
% independent variables to be used when building <simEngine_Model_help.html
% Model> objects.  Iterators can be either continuous, for use with
% differential equations, or discrete, for solving recurrence
% relationships.
%
%% Methods
%
% *Constructor*
% 
% * <matlab:doc('Iterator.iterator') Iterator> - create a new iterator
% object
%
%% Properties
%
% *solver* - string name of the numerical method.  Valid numerical methods are:
%
% * _forwardeuler_ - first order, fixed time step method
% * _heun_ - second order, fixed time step, predictor-corrector method
% * _rk4_ - 4th order, Runge-Kutta fixed time step method
% * _ode23_ - 2nd order, Bogacki-Shampine pair, variable time step method
% * _ode45_ - 2nd order, Dormand-Prince pair, variable time step method
% * _exponentialeuler_ - first order, linearly stiff solver, equations must
% fit within the form $y' = a \cdot y + b$
% * _linearbackwardeuler_ - first order, implicit method for linear systems
% * _cvode_ - CVode method from SUNDIALS(TM) offering both explicit and
% implicit methods.  Set the 'cv_lmm' property to either 'CV_ADAMS' or 'CV_BDF' and
% the 'cv_iter' property to 'CV_NEWTON' or 'CV_FUNCTIONAL'.  'CV_BDF' and
% 'CV_NEWTON', the recommended settings for implicit systems, is configured
% by default.
%
% *dt* - set the timestep for the simulation. This is a valid property for
% all fixed-time step solvers and _cvode_.  For _cvode_, the *dt* property
% will set how often the CVode iterator pauses to sync data between other
% iterators.
%
% *reltol*, *abstol* - available for _ode23_ and _ode45_ to set relative
% and absolute tolerances, respectively.  *reltol* is set to 1e-3 by
% default while *abstol* is set to 1e-6 by default.
%
% *sample_period*, *sample_frequency* - mutually exclusive properties
% available for discrete iterators.  The frequency is defined to be the
% inverse of the period.
%
%% Examples

% define a common iterator and update it's properties
t = Iterator('continuous', 'solver', 'ode45');
t.reltol = 1e-8; % set the relative tolerance
t.abstol = 1e-8; % set the absolute tolerance

% define an implicit method to use across stiff states with a linear
% relationship
t_implicit = Iterator('continuous', 'solver', 'linearbackwardeuler', 'dt', 0.01);

% use the CVode method from SUNDIALS(TM)
t_cvode = Iterator('continuous', 'solver', 'cvode');

% create an iterator for discrete simulations
n = Iterator('discrete', 'sample_frequency', 8192);
