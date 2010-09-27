% CreateUserErrorTest - Creates a test case (suite) to search for an error message
function s = CreateUserErrorTest(id, dslmodel, expectedstring)
dsl = fullfile('models_MessageTests', dslmodel);

s = Suite(id);
t1 = Test('UserError', @()(simex(dsl)), '-regexpmatch', 'USER ERROR');
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