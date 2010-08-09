classdef List
    
    methods (Static)

        function r = map(fcn, l)
            r = cell(1,length(l));
            for i=1:length(l)
                r{i} = fcn(l{i});
            end
        end
        
        function app(fcn, l)
            for i=1:length(l)
                fcn(l{i});
            end
        end
        
        
        function r = foldl(fcn, init, list)
            % FOLDL - folds the list by executing the function and reducing it to one
            % value
            switch length(list)
                case 0
                    r = init;
                case 1
                    r = fcn(list{1},init);
                otherwise
                    r = List.foldl(fcn, fcn(list{1}, init), list(2:end));
            end
            
            
        end
        
        
        function s = stringConcatWith(delim, l)
            if isempty(l)
                s = '';
            else
                s = List.foldl(@(elem,base_str)([base_str delim elem]), l{1}, l(2:end));
            end
        end
        
    end
    
    
end