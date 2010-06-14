model (times) = OutputTest9()

state x = 0
equation x' = 1

output times = () with {condition= x%3==0}

solver=forwardeuler{dt=1}

end
