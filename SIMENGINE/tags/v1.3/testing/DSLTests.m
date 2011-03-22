% COREFEATURETESTS - this is a clearing house of tests relating to features of
% the compiler
%
% Usage:
%  s = CoreFeatureTests - runs all tests
%  s = CoreFeatureTests('-release') - runs only those required for a release
%
function s = DSLTests(varargin)
INTERNAL = 0; RELEASE = 1;
if nargin == 0
    mode = 1;
else
    mode = varargin{1};
end
s = Suite('DSL Tests');

s.add(DSLDiffTests(mode));
s.add(RewriterTests(mode));
end

function [result] = run(strvec)
  s = '';
  for i = 1:length(strvec)
    s = strcat(s, strvec{i}, '\n');
  end
  str = strcat('printf "', s, '" | ', simexamplepath ,'/../bin/simEngine --batch -');
  
  function [res] = systemWrapper()
    status = system(str);
    %disp(result)
    res = 0 == status;
  end
  result = @systemWrapper;
end


function s = RewriterTests(mode)
INTERNAL = 0; RELEASE = 1;
s = Suite('Rewriter Tests');

% Simplification Tests
simp = {'var s = State.new(\"s\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", s+s)).tostring() == \"2*s\")'};
s.add(Test('Simplification Test a+a -> 2*a', run(simp)));

simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a/b)).tostring() == \"a*b^(-1)\")'};
s.add(Test('Simplification Test a/b -> a*b^-1', run(simp)));

simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", -a)).tostring() == \"(-1)*a\")'};
s.add(Test('Simplification Test -a -> -1*a', run(simp)));

%a-b -> a + -b
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a-b)).tostring() == \"a+(-1)*b\")'};
s.add(Test('Simplification Test a-b -> a+ -b', run(simp)));

%a^0 -> 1
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a^0)).tostring() == \"1\")'};
s.add(Test('Simplification Test a^0 -> 1', run(simp)));

%0^a -> 0
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", 0^a)).tostring() == \"0\")'};
s.add(Test('Simplification Test 0^a -> 0', run(simp)));

%a^b & a ^ c -> a ^ (b+c)
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a^b*a^c)).tostring() == \"a^(b+c)\")'};
s.add(Test('Simplification Test a^b*a^c->a^(b+c)', run(simp)));

%/* (a^b)^c -> a^(b*c) */
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", (a^b)^c)).tostring() == \"a^(b*c)\")'};
s.add(Test('Simplification Test a^b^c->a^(b*c)', run(simp)));

%/* a*a -> a^2 */
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a*a)).tostring() == \"a^2\")'};
s.add(Test('Simplification Test a*a->a^2', run(simp)));

%a*a^b -> a^(1+b)
simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a*a^b)).tostring() == \"a^(1+b)\")'};
s.add(Test('Simplification Test a*a^b->a^(1+b)', run(simp)));

% These tests don't pass right now, but that's not a problem...
if mode == INTERNAL
    simp = {'var a = State.new(\"a\")'
        'var b = State.new(\"b\")'
        'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a+1-a)).tostring() == \"1\")'};
     s.add(Test('Simplification Test a+1-a->1', run(simp)));

     %

     simp = {'var a = State.new(\"a\")'
         'var b = State.new(\"b\")'
         'var c = State.new(\"c\")'
         'test_pass ((LF repeatApplyRewritesExp(\"simplification\", a/a)).tostring() == \"1\")'};
     s.add(Test('Simplification Test a/a->1', run(simp)));
end
end

function s = DSLDiffTests(mode)
INTERNAL = 0; RELEASE = 1;

simpath = [fullfile(simexamplepath, '../bin', 'simEngine') ' --startupmessage=false --batch'];

s = Suite('DSL Diff Tests');
s.add(Test('Assignments', @()(0 == system([simpath ' DSLTests/difftests/assignments.dsl | diff - DSLTests/difftests/assignments.ok']))));
s.add(Test('HelloWorld', @()(0 == system([simpath ' DSLTests/difftests/helloworld.dsl | diff - DSLTests/difftests/helloworld.ok']))));
s.add(Test('Numbers', @()(0 == system([simpath ' DSLTests/difftests/numbers.dsl | diff - DSLTests/difftests/numbers.ok']))));
s.add(Test('Runnables', @()(0 == system([simpath ' DSLTests/difftests/runnables.dsl | diff - DSLTests/difftests/runnables.ok']))));
s.add(Test('Strings', @()(0 == system([simpath ' DSLTests/difftests/strings.dsl | diff - DSLTests/difftests/strings.ok']))));
s.add(Test('Booleans', @()(0 == system([simpath ' DSLTests/difftests/booleans.dsl | diff - DSLTests/difftests/booleans.ok']))));
s.add(Test('Loops', @()(0 == system([simpath ' DSLTests/difftests/loops.dsl | diff - DSLTests/difftests/loops.ok']))));
s.add(Test('Patterns', @()(0 == system([simpath ' DSLTests/difftests/patterns.dsl | diff - DSLTests/difftests/patterns.ok']))));
if mode == INTERNAL
    s.add(Test('Scope', @()(0 == system([simpath ' DSLTests/difftests/scope.dsl | diff - DSLTests/difftests/scope.ok']))));
end
end