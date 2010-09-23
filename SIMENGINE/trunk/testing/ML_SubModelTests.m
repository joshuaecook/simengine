function s = ML_SubModelTests(varargin)
%SUBMODELTESTS Tests the basic features of submodels
%   Detailed explanation goes here
%
% Usage:
%  s = SubModelTests - runs all tests
%  s = SubModelTests('-release') - runs only those required for a release
%

INTERNAL = 0; RELEASE = 1;

if nargin == 0
    target = '-cpu';
    mode = RELEASE;
elseif nargin == 1
    target = varargin{1};
    mode = RELEASE;
else
    target = varargin{2};
    switch lower(varargin{2})
        case '-release'
            mode = RELEASE;
        case '-internal'
            mode = INTERNAL;
        otherwise
            error('Simatra:MessageTests:ArgumentError', 'Invalid mode %s', varargin{1});
    end
end

s = Suite(['MATLAB Sub-model Feature Tests ' target]);
s.add(BasicSubModelTests);
s.add(HierarchySubModelTests);
s.add(AlgebraicSubModelTests);

    function s = BasicSubModelTests
        
        s = Suite(['MATLAB Basic Sub-Model Tests ' target]);

        function m = SubModelTest1
            sm = Model('sub');
            x = sm.state(0);
            sm.diffequ(x, 1);
            sm.output(x);
            
            m = Model('SubModelTest1');
            m.solver = 'forwardeuler';
            m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 2);
            s = m.submodel(sm);
            x2 = s.x;
            m.output('y', x1, x2);
        end
        
        s.add(Test('BasicSubModelTest', @()(simex(SubModelTest1, 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));

        function m = SubModelTest2
            sm = Model('sub');
            x = sm.state(0);
            step = sm.input('step');
            sm.diffequ(x, step);
            sm.output(x);
            
            m = Model('SubModelTest2');
            m.solver = 'forwardeuler';
            m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 2);
            s = m.submodel(sm);
            s.step = 1;
            x2 = s.x;
            m.output('y', x1, x2);
        end

        s.add(Test('SubModelInputTest', @()(simex(SubModelTest2, 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
        
        function m = SubModelTest3
            sm = Model('sub');
            x = sm.state(0);
            step = sm.input('step');
            sm.diffequ(x, step);
            sm.output(x);
            
            m = Model('SubModelTest3');
            m.solver = 'forwardeuler';
            m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 2);
            s1 = m.submodel(sm); s1.step = 1;
            s2 = m.submodel(sm); s2.step = 3;
            x2 = s1.x;
            x3 = s2.x;
            m.output('y', x1, x2, x3);
        end
        
        s.add(Test('DuplicateSubModelInputTest', @()(simex(SubModelTest3, 10,target)), '-equal', ...
            struct('y', [0:10; 0:2:20; 0:10; 0:3:30]')));

        
        function m = SubModelTest5
            sm = Model('sub');
            x = sm.state(0);
            step = sm.input('step', 2);
            sm.diffequ(x, step);
            sm.output(x);
            
            m = Model('SubModelTest5');
            m.solver = 'forwardeuler';
            m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 2);
            s1 = m.submodel(sm);
            s2 = m.submodel(sm); s2.step = 1;
            m.output('y', x1, s1.x, s2.x);
        end
        
        s.add(Test('SubModelDefaultInputTest', @()(simex(SubModelTest5, 10,target)), '-equal', ...
            struct('y', [0:10; 0:2:20; 0:2:20; 0:10]')));
        
        function m = SubModelTest6
            sm = Model('sub');
            start = sm.input('start');
            x = sm.state(start);
            sm.diffequ(x, 1);
            sm.output(x);
            
            m = Model('SubModelTest6');
            m.solver = 'forwardeuler';
            m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 2);
            s = m.submodel(sm); s.start = 0;
            x2 = s.x;
            m.output('y', x1, x2);
        end
        
        % this is related to init values driven by inputs
        s.add(Test('SubModelInputToInitTest', @()(simex(SubModelTest6, 10,target)), '-equal', struct('y', [0:10; 0:2:20; 0:10]')));
        
    end

    function s = HierarchySubModelTests
        
        s = Suite(['MATLAB Hierarchy Sub-Model Tests ' target]);

        function mdl3 = HierarchySubModelTest1
            mdl1 = Model('Bottom');
            step = mdl1.input('step');
            y = mdl1.state(0);
            mdl1.diffequ(y, step);
            mdl1.output(y);
            
            mdl2 = Model('Middle1');
            step = mdl2.input('step');
            b1 = mdl2.submodel(mdl1); b1.step = step;
            b2 = mdl2.submodel(mdl1); b2.step = 2*step;
            b3 = mdl2.submodel(mdl1); b3.step = 3*step;
            mdl2.output('y1', b1.y);
            mdl2.output('y2', b2.y);
            mdl2.output('y3', b3.y);
            
            mdl3 = Model('HierarchySubModelTest1');
            mdl3.solver = 'forwardeuler'; mdl3.dt = 1;
            step = mdl3.input('step', 1);
            m1 = mdl3.submodel(mdl2); m1.step = step;
            b1 = mdl3.submodel(mdl1); b1.step = 3*step;
            mdl3.output('y', b1.y, m1.y1)
        end
        
        s.add(Test('DeepHierarchy', ...
            @()(simex(HierarchySubModelTest1, 10,target)), ...
            '-equal', struct('y', [0:10; 0:3:30; 0:10]')));
        
        
        function mdl3 = HierarchySubModelTest2
            mdl1 = Model('Bottom');
            step = mdl1.input('step');
            y = mdl1.state(0);
            mdl1.diffequ(y, step);
            mdl1.output(y);
            
            mdl2 = Model('Middle1');
            step = mdl2.input('step');
            b1 = mdl2.submodel(mdl1); b1.step = step;
            b2 = mdl2.submodel(mdl1); b2.step = 2*step;
            b3 = mdl2.submodel(mdl1); b3.step = 3*step;
            y = mdl2.state(0);
            mdl2.diffequ(y, 4*step);
            mdl2.output('y1', b1.y);
            mdl2.output('y2', b2.y);
            mdl2.output('y3', b3.y);
            mdl2.output('y4', y);
            
            mdl3 = Model('HierarchySubModelTest2');
            mdl3.solver = 'forwardeuler'; mdl3.dt = 1;
            step = mdl3.input('step', 1);
            m1 = mdl3.submodel(mdl2); m1.step = step;
            b1 = mdl3.submodel(mdl1); b1.step = 3*step;
            mdl3.output('y', b1.y, m1.y1, m1.y4)
        end        
        
        s.add(Test('DeepHybridHierarchy', ...
            @()(simex(HierarchySubModelTest2, 10,target)), ...
            '-equal', struct('y', [0:10; 0:3:30; 0:10; 0:4:40]')));

        function mdl3 = HierarchySubModelTest3
            mdl1 = Model('Bottom');
            step = mdl1.input('step');
            y = mdl1.state(0);
            mdl1.diffequ(y, step);
            mdl1.output(y);
            
            mdl2a = Model('Middle1');
            step = mdl2a.input('step');
            b1 = mdl2a.submodel(mdl1); b1.step = step;
            b2 = mdl2a.submodel(mdl1); b2.step = 2*step;
            b3 = mdl2a.submodel(mdl1); b3.step = 3*step;
            mdl2a.output('y1', b1.y);
            mdl2a.output('y2', b2.y);
            mdl2a.output('y3', b3.y);
            
            mdl2b = Model('Middle2');
            step = mdl2b.input('step');
            n1 = mdl2b.submodel(mdl2a); n1.step = step;
            n2 = mdl2b.submodel(mdl2a); n2.step = 2*step;
            b1 = mdl2b.submodel(mdl1); b1.step = 3*step;
            mdl2b.output('y1', n1.y1);
            mdl2b.output('y2', n2.y2);
            mdl2b.output('y3', b1.y);
            
            mdl3 = Model('HierarchySubModelTest3');
            mdl3.solver = 'forwardeuler'; mdl3.dt = 1;
            step = mdl3.input('step', 1);
            m2 = mdl3.submodel(mdl2b); m2.step = 2*step;
            m1 = mdl3.submodel(mdl2a); m1.step = step;
            b1 = mdl3.submodel(mdl1); b1.step = 3*step;
            mdl3.output('y', b1.y, m2.y2, m1.y1)
        end        
 
        s.add(Test('DeeperHierarchy', ...
            @()(simex(HierarchySubModelTest3, 10,target)), ...
            '-equal', struct('y', [0:10; 0:3:30; 0:8:80; 0:10]')));
        
    end

    function s = AlgebraicSubModelTests
        s = Suite(['MATLAB Algebraic Sub-Model Tests ' target]);

        function m = AlgebraicSubModelTest1
            sm = Model('Fcn');
            x = sm.input('x');
            z = x^2;
            sm.output('y', z);
            
            m = Model('AlgebraicSubModelTest1');
            m.solver = 'forwardeuler'; m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 1);
            f = m.submodel(sm);
            f.x = x1;
            m.output('y', x1, f.y);
        end
        
        s.add(Test('BasicAlgebraicSubModelTest', ...
            @()(simex(AlgebraicSubModelTest1, 10, target)), ...
            '-equal', struct('y', [0:10; 0:10; (0:10).^2]')));
        
        function m = AlgebraicSubModelTest2
            sm = Model('Fcn');
            x = sm.input('x');
            z = sm.equ(x^2);
            sm.output('y', 3*z);
            
            m = Model('AlgebraicSubModelTest2');
            t1 = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);
            t2 = Iterator('continuous', 'solver', 'forwardeuler', 'dt', 1);
            x1 = m.state(0, 'iter', t1);
            m.diffequ(x1, 1);
            x2 = m.state(0, 'iter', t2);
            m.diffequ(x2, 1);
            f1 = m.submodel(sm); f1.x = x1;
            f2 = m.submodel(sm); f2.x = x2;
            m.output('y', x1, f1.y, f2.y);
        end        
        
        s.add(Test('MultipleIterators', ...
            @()(simex(AlgebraicSubModelTest2, 10, target)), ...
            '-equal', struct('y', [0:10; 0:10; (0:10).^2.*3; (0:10).^2.*3]')));
        
        
        function m = AlgebraicSubModelTest3
            sm = Model('Fcn');
            x = sm.input('x');
            y = sm.state(0);
            sm.diffequ(y, x);
            sm.output(y);
            
            m = Model('AlgebraicSubModelTest3');
            m.solver = 'forwardeuler'; m.dt = 1;
            s = m.submodel(sm); s.x = 4;
            y = s.y;
            m.output(y);
        end        
        
        function m = AlgebraicSubModelTest4
            sm = Model('Fcn');
            x = sm.input('x');
            z = sm.equ(z, x^2);
            sm.output('y', z);
            
            m = Model('AlgebraicSubModelTest3');
            m.solver = 'forwardeuler'; m.dt = 1;
            x1 = m.state(0);
            m.diffequ(x1, 1);
            f = m.submodel(sm); f.x = x1;
            m.output('y', f.y);
            m.output('z', x1);
        end        
        
        
        s.add(Test('IteratorOfInput', ...
            @()(simex(AlgebraicSubModelTest4, 10, target)), ...
            '-equal', struct('y', [0:10; (0:10).^2]', ...
            'z', [0:10; 0:10]')));
    end


end