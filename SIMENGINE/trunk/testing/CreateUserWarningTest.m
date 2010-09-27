% CreateUserWarningTest - Creates a test case (suite) to search for
% a warning message
function s = CreateUserWarningTest(id, dslmodel, expectedstring)
dsl = fullfile('models_MessageTests', dslmodel);

s = Suite(id);
t1 = Test('Completes', @()(simex(dsl)), '-withouterror');
t2 = Test('UserWarning', @()(dispAndReturn(t1.Output)), '-regexpmatch', 'WARNING');
t3 = Test('AppropriateMessage', @()(dispAndReturn(t1.Output)), '-regexpmatch', ...
          expectedstring);

s.add(t1);
s.add(t2);
s.add(t3);

end

function y = dispAndReturn(str)
disp(str);
y = true;
end