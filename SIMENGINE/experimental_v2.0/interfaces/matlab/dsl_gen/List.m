% List - collection of functional list manipulation methods loosely based on
% ML-style languages
%
% List Methods:
%    map - evaluate a function across all elements of a cell array
%    app - evaluate a function across all elements of a cell array
%    returning no arguments
%    foldl - successively evaluate a function across a list, returning an
%    aggregated value
%    filter - return only those elements matching a condition
%    partition - separate elements in a list based on if they match a
%    condition
%    exists - returns true if any element matches the condition
%    all - returns true if all elements match the condition
%
% List Utilities:
%    cell2str - Convert a cell array into a string representation
%    stringConcatWith - Concatenate a cell array of strings with a given
%    delimeter
%
% Copyright 2010 Simatra Modeling Technologies
%
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

        function r = filter(fcn, l)
            % FILTER - returns a filtered cell array of only those elements
            % that when applied to the function are true
            %
            % Examples:
            %   List.filter(@(x)(isprime(x)), num2cell(1:100)) - return a
            %   list of all prime numbers from 1 to 100
            %
            logicals = false(1, length(l));
            for i=1:length(l)
                logicals(i) = logical(fcn(l{i}));
            end
            r = l(logicals);
        end
        
        function does_exist = exists(fcn, l)
            does_exist = false;
            for i=1:length(l)
                if logical(fcn(l{i}))
                    does_exist = true;
                    break;
                end
            end
        end

        function all_exist = all(fcn, l)
            [matching, notmatching] = List.partition(fcn, l);
            all_exist = isempty(notmatching);
        end

        
        function [matching, notmatching] = partition(fcn, l)
            logicals = false(1, length(l));
            for i=1:length(l)
                logicals(i) = logical(fcn(l{i}));
            end
            matching = l(logicals);
            notmatching = l(~logicals);
        end
        
        function s = stringConcatWith(delim, l)
            if isempty(l)
                s = '';
            else
                s = List.foldl(@(elem,base_str)([base_str delim elem]), l{1}, l(2:end));
            end
        end
        
        function str = cell2str(c)
            function s = toStr(var)
                if isnumeric(var)
                    s = num2str(var);
                elseif iscell(var)
                    s = cell2str(var);
                elseif ischar(var)
                    s = [''''  var ''''];
                else
                    warning('Simatra:List:cell2str:toStr', 'Don''t understand class type %s', class(var));
                    s = '';
                end
            end
            
            % Turn everything into a string
            str_list = List.map (@toStr, c);
            
            str = ['{' List.stringConcatWith(', ', str_list) '}'];
        end
        
    end
    
    
end