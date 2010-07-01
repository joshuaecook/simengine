m = Unit('meter', 'm');
s = Unit('second', 's');
g = Unit('gram', 'g');
kg = SIUnit('kilo', g);
ohm = Unit('ohm', 'ohms');
amp = Unit('amp', 'A');
rad = Unit('radians', 'rad');

v = m/s
a = v/s
N = kg*a
J = N*m
W = J/s



% u = Units;
% 
% u.add(BaseUnit('meter', 'm'));
% u.add(BaseUnit('second', 's'));
% u.add(BaseUnit('gram', 'g'));
% u.add(SIUnit('kilo',u('g')));
% u.add(BaseUnit('ohm', 'ohms'));
% u.add(BaseUnit('amp', 'A'));
% u.add(BaseUnit('radians', 'rad'));
% 
% v = u('m')/u('s');
% a = v/u('s');
% % F = ma
% F = kg*a;
% disp(F)


% u.add(Unit('Velocity', 'v', velocity));


% disp(u);

% disp('================');
% velocity*1
% 
% v = Unit('Velocity', 'v', v)
% N = Unit('Force', 'N', kg*m/s^2)
% disp(v)
