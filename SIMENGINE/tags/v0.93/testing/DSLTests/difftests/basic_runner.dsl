global quantity dt = 0.1
global quantity t = 0


task add_fwd_euler (eq)
  Equation.new (eq.assigned_state, 
		eq.assigned_state.currentval + dt * eq.expression, 
		eq.assigned_model)
end

function fwd_euler (eqs) = eqs.map add_fwd_euler


function computeCurrentVal(eq) = (eq.assigned_state, eq.regenerate())

task updateState (astate: State, value)
  astate.currentval = value
end

function run_fwd_euler (eqs) = ((eqs.map add_fwd_euler).map computeCurrentVal).map updateState



/*
function rk4_next (step) = 

task rk4_next (step, eqsAndPrevVals)
  foreach pair in eqsAndPrevVals do
    pair(1).assigned_state.currentval = pair(2) + step * pair(1).assigned_state.currentval
  end

  ((eqsAndPrevVals.map (lambdafun(pair) = pair(1))).map computeCurrentVal).map updateState
end

task run_rk4 (eqs) 
  eqsAndPrevVals = eqs.map (lambdafun (eq) = (eq, eq.assigned_state.currentval))

  rk4_next (0,    eqsAndPrevVals) //k1
  rk4_next (dt/2, eqsAndPrevVals) //k2
  rk4_next (dt/2, eqsAndPrevVals) //k3
  rk4_next (dt,   eqsAndPrevVals) //k4
  // FUDGE, I FORGOT THE LAST STEP!!!, I NEED A META WAY TO GENERATE THE EQS FOR THIS SHIT AND SWAP IN THE CORRECT STATE READS!!!
end 
*/

class Rk4eq 
  quantity eq
  quantity slopes

  constructor (eq)
    slopes = []
    self.eq = eq
  end

//  function resultant_eq () = eq.copyWithNewExp ((slopes(1) + 2 * slopes(2) + 2 * slopes(3) + slopes(4))/6)

  task resultant_eq ()
    if slopes.length() == 4 then
      eq.copyWithNewExp (eq.assigned_state.currentval + dt / 6 * (slopes(1) + 2 * slopes(2) + 2 * slopes(3) + slopes(4)))
    else
      error ("Invalid number of rk slopes.  Expected 4, found " + (slopes.length()))
    end
  end

  function latest_slope() = {0 when slopes.length() == 0,
			       slopes.last() otherwise}


end

task rk4 (eqs: Vector of Equation)
  quantity rk4eqs = []//eqs.map (Rk4eq.new) //TODO: FIX THIS BUG

  foreach eq in eqs do
    rk4eqs.push_back (Rk4eq.new eq)
  end

  quantity coeffs = [0, dt/2, dt/2, dt]

  //k1..k4
  foreach coeff in coeffs do
    foreach rk4eq in rk4eqs do
      rk4eq.eq.assigned_state.readval = rk4eq.eq.assigned_state.currentval + coeff * rk4eq.latest_slope()
    end

    foreach rk4eq in rk4eqs do
      rk4eq.slopes.push_back (rk4eq.eq.regenerate())
    end
  end

  rk4eqs.map (lambdafun(eq) = eq.resultant_eq())
  //[rk4eqs(1).resultant_eq()]
  //rk4eqs.map (lambdafun(eq) = eq.resultant_eq())
  //rk4eqs.map (lambdafun(eq) = eq.eq)
  
end


function reset_eq (eq) = eq.assigned_state.reset()

class Log
  quantity astate
  quantity values
  
  constructor (astate)
    self.astate = astate
    values = []
  end
 
  task addStateValue()
    values.push_back (astate.currentval)
  end
end

task basic_runner (m: Model, num_iterations: Number) 
  //quantity log = m.eqs.map (lambdafun (eq) = Log.new(eq.assigned_state))

  quantity method = rk4
  //quantity method = fwd_euler

  quantity log = []
  foreach eq in m.eqs do
    log.push_back (Log.new (eq.assigned_state))
  end

  m.eqs.map reset_eq

  log.map (lambdafun (entry) = entry.addStateValue())

  foreach i in 1 .. num_iterations do
    //run_fwd_euler (m.eqs)
    print "."
    ((method(m.eqs)).map computeCurrentVal).map updateState

    log.map (lambdafun (entry) = entry.addStateValue())
  end

  foreach entry in log do
    println ("State " + entry.astate.name)

    foreach value in entry.values do
      print (value + ", ")
    end
    println ""
  end

  log
end


