model (x)=UpdateContinuousIteratorTest1

    state x = 0
    
    equation x' = 1
    equation x = 0 when x >= 4

    solver=forwardeuler{dt=1}
end
	