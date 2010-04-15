% SOLARSYSTEM - Shows orbital paths of planets (and our moon) around the sun
%
% Copyright 2010 Simatra Modeling Technologies
function solarsystem
  o = simex('solarsystem.dsl', 1e8);
  simplot(o.moondata(2:end,2:3), o.earthdata(2:end,2:3), o.sundata(2:end,2:3), o.marsdata(2:end,2:3), o.venusdata(2:end,2:3), o.mercurydata(2:end,2:3), o.jupiterdata(2:end,2:3), o.saturndata(2:end,2:3), o.uranusdata(2:end,2:3), o.neptunedata(2:end,2:3));
end
