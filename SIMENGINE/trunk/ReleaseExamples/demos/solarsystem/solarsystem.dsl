/*
 * solarsystem - Simulates the motion of planets in our solar system using 
 *               Newtonian gravity.  Also includes our moon.
 * 
 *               This model demonstrates how to use inputs to solve an 
 *               n-body problem.
 *
 * Copyright 2010 Simatra Modeling Technologies
 */

//This model represents a planet.  It takes in a starting position, starting velocity, mass, and external forces acting upon it.  It computes it's position and velocity, over time, as a function of these external forces. 

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


// This model represents a very simplified view of our solar system.  
// Each data output is a pair of x and y coordinates.
model (moondata, earthdata, sundata, mercurydata, venusdata, marsdata, jupiterdata, saturndata, uranusdata, neptunedata) = solarsystem
  constant G = 6.67e-11


  //This function is used to connect a vector of masses via their external force inputs.
  //  Note how external forces are repeatedly updated for each new force
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
constant moondist = 3.84e5
constant earthspeed = 29.783  //km/s
constant moonspeed = 1.022 //km/s 
  submodel planet sun with {mass=1.989e30, initX = 0, initY=0}

  submodel planet mercury with {mass=3.3e23,initX = 0, initY = 0.387*AU, initXv=47.87}
  submodel planet venus with {mass=4.868e24,initX = 0, initY = 0.723*AU, initXv=35.02}

  submodel planet earth with {mass=5.9e24,initX = 0, initY = AU, initXv=earthspeed}
  submodel planet moon with {mass=7.36e22, initX = 0, initY = AU-moondist, initXv = earthspeed+moonspeed}

  submodel planet mars with {mass=6.419e23,initX = 0, initY = 1.523*AU, initXv=24.08}

  submodel planet jupiter with {mass=1.8986e27, initX = 0, initY = 5.2*AU, initXv=13.07}
  submodel planet saturn with {mass=5.685e26, initX = 0, initY = 9.58*AU, initXv=9.69}
  submodel planet uranus with {mass=8.6810e25, initX = 0, initY = 19.23*AU, initXv=6.81}
  submodel planet neptune with {mass=1.024e26, initX = 0, initY = 30.1*AU, initXv=5.43}


  connectPlanets [sun, earth, moon, mercury,venus,mars, jupiter, saturn, uranus, neptune]

  //t2 is used to perform downsampling of the outputs
  iterator t2 with {continuous, solver=forwardeuler {dt=24*60*60}} // output every day

  output earthdata[t2]= (earth.x, earth.y)
  output moondata[t2] = (moon.x, moon.y)

  output sundata[t2] = (sun.x, sun.y)

  output mercurydata[t2] = (mercury.x, mercury.y)
  output venusdata[t2] = (venus.x, venus.y)
  output marsdata[t2] = (mars.x, mars.y)

  output jupiterdata[t2] = (jupiter.x, jupiter.y)
  output saturndata[t2] = (saturn.x, saturn.y)
  output uranusdata[t2] = (uranus.x, uranus.y)
  output neptunedata[t2] = (neptune.x, neptune.y)


  solver= rk4 {dt=15*60} // update every 15 minutes
end

