model (y) = OutputTest4

state y = 0

equation y' = 1

output y with {condition= (y >=5 )}

solver=forwardeuler{dt=1}

end

