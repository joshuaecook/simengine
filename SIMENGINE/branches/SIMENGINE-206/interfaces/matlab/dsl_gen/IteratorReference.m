classdef IteratorReference
    
    properties
        iterator
        delay
    end
    
    methods
        function iterref = IteratorReference(iter, delay)
            iterref.iterator = iter;
            iterref.delay = delay;
        end
        
        function str = toStr(iterref)
            if iterref.iterator.isDiscrete
                if iterref.delay < 0
                    str = [iterref.iterator.id num2str(iterref.delay)];
                elseif iterref.delay == 0
                    str = iterref.iterator.id;
                else
                    str = [iterref.iterator.id '+' num2str(iterref.delay)];
                end
            else
                if iterref.delay == 0
                    str = iterref.iterator.id;
                else
                    str = [iterref.iterator.id '[' num2str(iterref.delay) ']'];
                end
            end
        end
        
        function disp(iterref)
            disp(toStr(iterref))
        end
    end
    
end