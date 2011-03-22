model (y) = delay(x)
    
    equation myx = x
    equation y = myx[t[-1]]
    
end

model (y) = IteratorSubModelTest1()
    
    solver=forwardeuler{dt=1}

    state x = 0
    submodel delay s with {x=x}
    equation x' = 1
    output y = (x, s.y)

end
