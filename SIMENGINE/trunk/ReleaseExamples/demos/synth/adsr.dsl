
model (amplitude) = adsr(key, attackDuration, attackSlope, decayDuration, decaySlope, sustainSlope, releaseDuration, releaseSlope)

input key with {default = 1}

state amplitude = 0
state pressTime = 0
state releaseTime = 0

equations
pressTime = {t when (key <> 0 and pressTime == 0), pressTime when key <> 0, 0 otherwise}
releaseTime = {t when (key == 0 and pressTime <> 0), releaseTime when key == 0, 0 otherwise}

dy = {attackSlope when key <> 0 and (t - pressTime) < attackDuration,
      decaySlope when key <> 0 and (t - pressTime) <= (decayDuration + attackDuration),
      sustainSlope when key <> 0,
      releaseSlope when (t - releaseTime) <= releaseDuration,
      0 otherwise}

amplitude' = dy
amplitude = 0 when amplitude < 0
end

solver = forwardeuler
solver.dt = 1

end
