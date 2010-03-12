function s = MathFeatureTests(varargin)
%MATHTESTS Tests the basic features of submodels
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
  target = varargin{1};
  mode = varargin{2};
end

s = Suite(['Math Feature Tests ' target]);
s.add(DerivativeTests(mode, target));

end

function s = DerivativeTests(mode, target)

s = Suite(['Derivative Tests ' target]);

s.add(Test('OutputDerivative', @()(simex(['models_FeatureTests/' ...
                    'DerivativeTest1.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:1:10; ones(1,11)]')));
s.add(Test('IntermediateDerivative', @()(simex(['models_FeatureTests/' ...
                    'DerivativeTest2.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:1:10; ones(1,11)]')));
s.add(Test('ForcedDerivative', @()(simex(['models_FeatureTests/' ...
                    'DerivativeTest3.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:1:10; ones(1,11)]')));
s.add(Test('DerivativeWithBackwardEuler', @()(simex(['models_FeatureTests/' ...
                    'DerivativeTest4.dsl'], 10,target)), '-equal', struct('y', [0:10; 0:1:10; ones(1,11)]')));

end
