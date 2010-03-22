model (x,y,amass,x_vel, y_vel) = planet (mass, externalForceX, externalForceY, initX, initY, initXv, initYv)

  input externalForceX with {default=0}
  input externalForceY with {default=0}

  input initXv with {default=0}
  input initYv with {default=0}

  state x = 0
  state y = 0

  state x_vel = 0
  state y_vel = 0

  state init = 0

  equation init = init+1

  equation x_vel' = externalForceX / mass /1e3
  equation y_vel' = externalForceY / mass /1e3

  equation x' = x_vel
  equation y' = y_vel

  equation x = initX when init == 0
  equation y = initY when init == 0

  equation x_vel = initXv when init == 0 or init==1
  equation y_vel = initYv when init == 0 or init ==1

  equation amass=mass
end

model (lunadata, earthdata, sundata, mercurydata, venusdata, marsdata, phobosdata, deimosdata) = innersystem
  constant G = 6.67e-11

  function connectPlanets (planets)
    foreach planet in planets do
      foreach otherplanet in planets do
        if planet <> otherplanet then
          var distanceSqr = ((planet.x*1e3 - otherplanet.x*1e3)^2 + (planet.y*1e3 - otherplanet.y*1e3)^2)
          
          var force = -G * planet.amass * otherplanet.amass / distanceSqr
          var distance = distanceSqr^0.5
          
          var xcoeff = (planet.x*1e3-otherplanet.x*1e3) / distance
          var ycoeff = (planet.y*1e3-otherplanet.y*1e3) / distance

          planet.externalForceX = planet.externalForceX + xcoeff * force
          planet.externalForceY = planet.externalForceY + ycoeff * force
        end
      end
    end
  end


// all numbers taken fron Wikipedia, mass in kg, velocity in km/s

constant AU = 1.52e8 //km
constant lunadist = 3.84e5
constant earthspeed = 29.783  //km/s
constant lunaspeed = 1.022 //km/s 
  submodel planet sun with {mass=1.989e30, initX = 0, initY=0}

  submodel planet mercury with {mass=3.3e23,initX = 0, initY = 0.387*AU, initXv=47.87}
  submodel planet venus with {mass=4.868e24,initX = 0, initY = 0.723*AU, initXv=35.02}

  submodel planet earth with {mass=5.9e24,initX = 0, initY = AU, initXv=earthspeed}
  submodel planet luna with {mass=7.36e22, initX = 0, initY = AU+lunadist, initXv = earthspeed+lunaspeed}

  submodel planet mars with {mass=6.419e23,initX = 0, initY = 1.523*AU, initXv=24.08}

  submodel planet phobos with {mass=1.07e16, initX = 0, initY = 1.523*AU+9377, initXv=24.08+2.138}
  submodel planet deimos with {mass=1.48e15, initX = 0, initY = 1.523*AU+2.346e4, initXv=24.08+1.35}


  connectPlanets [sun, earth, luna, mercury,venus, mars, deimos, phobos]

  iterator t2 with {continuous, solver=forwardeuler {dt=60*60}}

  output earthdata[t2]= (earth.x, earth.y)
  output lunadata[t2] = (luna.x, luna.y)

  output sundata[t2] = (sun.x, sun.y)

  output mercurydata[t2] = (mercury.x, mercury.y)
  output venusdata[t2] = (venus.x, venus.y)
  output marsdata[t2] = (mars.x, mars.y)

  output phobosdata[t2] = (phobos.x, phobos.y)
  output deimosdata[t2] = (deimos.x, deimos.y)

  solver= heun {dt=60}
  //solver=ode23{dt=24*60*60}
end

