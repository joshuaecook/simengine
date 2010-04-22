// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
// ADSR Envelope (attack, decay, sustain, release)

// key = whether key is currently pressed or not, evaluated as a boolean
// attackSlope = how fast the peak is reached
// decaySlope = how fast the sustainLevel is reached after peaking
// sustainLevel = magnitude of signal after decay phase
// releaseSlope = how fast signal returns to 0 when key is released
// amplitude = output signal of signal strength

model (amplitude) = adsr(key, attackSlope, decaySlope, sustainLevel, releaseSlope)
 iterator n_in with {discrete, sample_period=1/64}
 input key with {iter=n_in, halt_when_exhausted}

 input attackSlope with {default=15}
 input decaySlope with {default=-2}
 input sustainLevel with {default=0.75}
 input releaseSlope with {default=-5}

 state peaked = 0

 state amplitude = 0

 equations
   peak = 0.95

   peaked = {0 when key == 0,
	     1 when amplitude >= peak,
             peaked otherwise}

   attacking = {1 when (key <> 0 and (amplitude < peak) and peaked == 0),
                0 otherwise}

   sustaining = {1 when (key <> 0 and peaked == 1 and amplitude <= sustainLevel),
                 0 otherwise}

   decaying = {1 when (key <> 0 and (amplitude >= peak or peaked == 1) and not(sustaining)),
               0 otherwise}

   releasing = {1 when key == 0 and amplitude > 0,
                0 otherwise}

   dy = {attackSlope when attacking,
	 0 when sustaining,
         releaseSlope when releasing,
         decaySlope when decaying,
         0 otherwise}

   amplitude' = dy
   amplitude = 0 when amplitude < 0
   amplitude = peak when amplitude > peak
   amplitude = sustainLevel when (decaying and amplitude < sustainLevel)
 end

 output amplitude

 solver = forwardeuler
 solver.dt = 1/48e3

end
