//settings.ir.aggregate.setValue(true)
model (x)=UpdateContinuousIteratorTest7

    state x = 10

    equations
	x' = 1
    
	cond = t % 4 == 0
	x = 0 when cond
    end

    solver=forwardeuler{dt=1}
end
	
