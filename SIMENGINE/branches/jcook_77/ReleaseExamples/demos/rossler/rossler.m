function rossler
  o = simex('rossler.dsl', 1e3);
  figure;
  simplot(o.x(:,2), o.y(:,2));
  figure;
  simplot(o.x(:,2), o.z(:,2));
  figure;
  simplot(o.y(:,2), o.z(:,2));
end