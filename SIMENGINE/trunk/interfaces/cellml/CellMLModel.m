classdef CellMLModel < Model
    
    properties
        units
        components
        groups
        connections
    end
    
    properties (Access = protected)
        generatedSubModels
        containment
        time
    end
    
    methods
        % Constructor
        function [obj] = CellMLModel(name,units,components,groups,connections)
            disp(['Constructing CellMLModel: ' name])
            obj@Model(name);
            
            % instantiate object variables
            obj.generatedSubModels = containers.Map;
            obj.containment = containers.Map;
           
            % create default iterator
            obj.time = Iterator('t', 'continuous', 'solver', 'ode45');
            
            % generate internal data structures
            obj.add_units(units);
            obj.add_connections(connections);
            obj.add_groups(groups);
            obj.add_components(components);

        end
        
        function [obj] = add_units(obj, units)
            obj.units = units;
        end
        
        function [obj] = add_components(obj, components)
            %obj.components = components;
            obj.components = containers.Map;
            for cid = 1:length(components)
                c = components(cid);
                obj.components(c.name) = c;
            end

            % create a mapping of submodels
            function traverseGroup(group)
                name = group.component;
                refs = group.component_ref;
                clist = cell(1, length(refs));
                for rid=1:length(refs)
                    clist{rid} = refs(rid).component;
                    traverseGroup(refs(rid));
                end
                obj.containment(name) = clist;
            end
                
            top_level = false;
            for gid = 1:length(obj.groups)
                if strcmp(obj.groups(gid).relationship_ref.relationship, 'containment')
                    if isfield(obj.groups(gid), 'component_ref')
                        ref = obj.groups(gid).component_ref;
                        top_level = ref.component;
                        traverseGroup(ref);
                    else
                        % might just be empty
                    end
                else
                    % ignoring 'encapsulation'
                end
            end
            
%             disp('SUBMODELS')
%             keys = submodels.keys;
%             for i=1:length(keys)
%                 fprintf(1, ' %s: ', keys{i});
%                 display(submodels(keys{i}));
%                 fprintf(1, '\n');
%             end
            
            % need to grab just the top level
            if ischar(top_level)
                top_mod = component_to_model(obj, top_level);
            else
                error('Simatra:CellMLModel', 'No top level model found');
            end
            
            % instantiate the submodel
            create_top_level(obj, top_mod);
            
        end
        
        function create_top_level(obj, top_mod)
            inst = obj.submodel(top_mod);
            [inputs, outputs] = interface(top_mod);
            obj.equ('time', Exp(obj.time));
            for i=1:length(inputs)
                inp = obj.input(inputs{i});
                inst.(inputs{i}) = inp;
            end
            for i=1:length(outputs)
                obj.equ(outputs{i}, inst.(outputs{i}));
                obj.output(outputs{i});
            end
        end
            
        
        function [obj] = add_connections(obj, connections)
            %obj.connections = connections;
            obj.connections = containers.Map;
            for i=1:length(connections)
                %disp(sprintf('Connection #%d', i));
                c = connections(i);
                parent = c.components{1};
                child = c.components{2};
                if isKey(obj.connections, parent)
                    p = obj.connections(parent);
                    p(child) = c.variables;
                else
                    obj.connections(parent) = containers.Map;
                    p = obj.connections(parent);
                    p(child) = c.variables;
                end
            end
        end
        
        function [obj] = add_groups(obj, groups)
            obj.groups = groups;
        end
        
        function [mod] = component_to_model(obj, name)
            if isKey(obj.generatedSubModels, name)
                mod = obj.generatedSubModels(name);
            else
                mod = create_model(obj, obj.components(name), obj.containment(name));
            end
        end
        
        function [mod] = create_model(obj, component, submodels)
            disp(['Create component: ' component.name]);
            mod = Model(component.name);
            
            % create a variable list
            inpvarlist = containers.Map;
            varlist = containers.Map;
            
            % generate inputs and outputs
            for vid = 1:length(component.variables)
                var = component.variables(vid);
                if strcmp('in',var.public_interface) || strcmp('in',var.private_interface)
                    % ADD UNITS
                    if isnan(var.initial_value);
                        inpvarlist(var.name) = false;
                        %mod.input(var.name);
                    else
                        inpvarlist(var.name) = var.initial_value;
                        %mod.input(var.name, var.initial_value);
                    end
                    %mod.add_input(var.name, Input(var.initial_value));
                else
                    % ADD UNITS
                    %state = mod.state(var.name, var.initial_value);
                    %disp(sprintf('Found variable name: %s', var.name));
                    varlist(var.name) = var.initial_value;
                    %mod.add_state(var.name, State(var.initial_value));
                end
                if strcmp('out',var.public_interface) || strcmp('out',var.private_interface)
                    % ADD UNITS
                    mod.output(var.name, Exp(var.name));
                    %mod.add_output(var.name);
                end
            end
            
            if isKey(obj.connections, component.name)
                mod_connections = obj.connections(component.name);
                % generate submodels
                for smid = 1:length(submodels)
                    % first instantiate the submodel
                    sm_name = submodels{smid};
                    sm = mod.submodel(component_to_model(obj, sm_name));
                    
                    % now go through all the connections
                    connect_variables = mod_connections(sm_name);
                    
                    for cvid = 1:length(connect_variables)
                        pvar = connect_variables{cvid,1};
                        cvar = connect_variables{cvid,2};
                        if isInput(sm, cvar)
                            sm.(cvar) = pvar;
                        elseif isOutput(sm, cvar)
                            mod.equ(pvar, sm.(cvar));
                            if isKey(varlist, pvar)
                                % we're defining this variable, so we can
                                % now ignore it
                                varlist.remove(pvar); 
                            end
                            if isKey(inpvarlist, pvar)
                                inpvarlist.remove(pvar);
                            end
                        else
                            error('Simatra:CellMLModel:create_model', '%s is neither an input nor output of %s', cvar, sm_name);
                        end
                    end
                end
                
                % TODO - handle environment here
                if isKey(mod_connections, 'environment')
                    connect_variables = mod_connections('environment');
                    if size(connect_variables,1) ~= 1 || ~strcmp(connect_variables{1,2}, 'time')
                        connect_variables
                        error('Simatra:CellMLModel:create_model', 'Can not find time in evironment');
                    end
                    pvar = connect_variables{1,1};
                    cvar = connect_variables{1,2};
                    mod.equ(pvar, Exp(obj.time));
                    if isKey(inpvarlist, pvar)
                        inpvarlist.remove(pvar);
                    end
                end
            else
                % there are no connections, so there can't be any sub
                % models
            end
            
            % add equations
            for eqid = 1:length(component.math)
                eqn = component.math{eqid};
                if iscell(eqn)
                    if strcmp('eq',eqn{1})
                        lhs = eqn{2};
                        rhs = eqn{3};
                        if iscell(rhs)
                            error('Simatra:CellMLModel:add_component', 'Unexpected operation %s (%d arguments)', rhs{1}, length(rhs{2}));
                        end
                        if iscell(lhs)
                            if strcmp('diff',lhs{1})
                                id = toStr(lhs{3});
                                %disp(sprintf('Found differential equation with lhs: %s', id));
                                state = mod.state(id, varlist(id));
                                varlist.remove(id);
                                var = Exp(lhs{3});
                                mod.diffequ(var, rhs);
                                %var = mod.get_var(lhs{3});
                                %var.diffeq = rhs;
                            end
                        else
                            if isa(lhs, 'Exp')
                                %disp(sprintf('Found equation with lhs: %s', toStr(lhs)));
                                varlist.remove(toStr(lhs));
                            end
                            mod.equ(lhs, rhs);
                            %var = mod.get_var(lhs);
                            %var.eqn = rhs;
                        end
                    end
                end
            end
            
            % add the inputs now
            keys = inpvarlist.keys;
            for i=1:length(keys)
                val = inpvarlist(keys{i});
                if islogical(val) && ~val
                    mod.input(keys{i});
                else
                    mod.input(keys{i}, val);
                end
            end
            
            % add dummy variables for the time being
            keys = varlist.keys;
            for i=1:length(keys)
                disp(sprintf('Adding undefined equation for %s', keys{i}));
                mod.equ(keys{i}, varlist(keys{i}));
            end
                
            mod.order_equations;
            %type(mod)
        end
        
    end
    
end
