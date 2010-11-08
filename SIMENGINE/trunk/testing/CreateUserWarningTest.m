% CreateUserWarningTest - Creates a test case (suite) to search for
% a warning message
function s = CreateUserWarningTest(id, dslmodel, expectedstring)
if isa(dslmodel, 'Model')
    mdl = dslmodel;
else
    mdl = fullfile('models_MessageTests', dslmodel);
end

s = Suite(id, {'messages'});
t1 = Test('Completes', @()(simex(mdl)), '-withouterror');
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