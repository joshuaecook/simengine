% CreateUserErrorTest - Creates a test case (suite) to search for an error message
function s = CreateUserErrorTest(id, dslmodel, expectedstring)
if isa(dslmodel, 'Model')
    mdl = dslmodel;
else
    mdl = fullfile('models_MessageTests', dslmodel);
end

s = Suite(id);
t1 = Test('UserError', @()(simex(mdl)), '-regexpmatch', 'USER ERROR');
t2 = Test('AppropriateMessage', @()(dispAndReturn(t1.Output)), '-regexpmatch', ...
          expectedstring);
t3 = Test('NoFailure', @()(dispAndReturn(t1.Output)), '-regexpmatch', 'FAILURE:|Exception');
t3.ExpectFail = true;

s.add(t1);
s.add(t2);
s.add(t3);

end

function y = dispAndReturn(str)
disp(str);
y = true;
end