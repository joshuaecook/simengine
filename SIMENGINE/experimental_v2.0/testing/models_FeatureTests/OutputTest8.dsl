model (times) = OutputTest8()

state x = 0
equation x' = 1

output times = t with {condition= x%3==0}

solver=forwardeuler{dt=1}

end
