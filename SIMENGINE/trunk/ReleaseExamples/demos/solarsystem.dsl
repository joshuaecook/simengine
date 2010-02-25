//settings.debug.logdof.setValue(true)

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

model (moondata, earthdata, sundata/*, xs,ys,xvs,yvs*/) = solarsystem
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

          planet.externalForceX = planet.externalForceX.inputVal + xcoeff * force
          planet.externalForceY = planet.externalForceY.inputVal + ycoeff * force
        end
      end
    end
  end



constant AU = 1.52e8
constant moondist = 3.84e5
constant earthspeed = 29.783  //km/s
constant moonspeed = 1.022 //km/s 
  submodel planet earth with {mass=5.9e24,initX = 0, initY = AU, initXv=earthspeed}
  submodel planet moon with {mass=7.36e22, initX = 0, initY = AU-moondist, initXv = earthspeed+moonspeed}

  submodel planet sun with {mass=1.989e30, initX = 0, initY=0}

  connectPlanets [sun, earth, moon]
//  connectPlanets [earth, sun]

  iterator t2 with {continuous, solver=forwardeuler {dt=24*60*60}}

  output earthdata[t2]= (earth.x, earth.y)
  output moondata[t2] = (moon.x, moon.y)

  output sundata[t2] = (sun.x, sun.y)

  solver=forwardeuler {dt=60}
end

