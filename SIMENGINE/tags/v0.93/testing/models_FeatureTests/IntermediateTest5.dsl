model (y,I) = IntermediateTest5

    state y = 0
    equation I = {0 when t < 5,
		  1 otherwise}
    equation y' = I
    
    solver=forwardeuler{dt=1}

end
