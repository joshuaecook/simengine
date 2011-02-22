model (x) = Sub(step)

    state x = 0
    equation x' = step
    equation x = 0 when x >= 4
    
end

model (y)=TemporalIteratorSubModelsTest1

    submodel Sub s with {step=1}
    
    output y = s.x
    t {solver=forwardeuler{dt=1}}
end
