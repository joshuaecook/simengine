model (x) = Sub(step)
    state x = 0
    equation x' = step
    output x = x[t[-2]]
end

model (y)=TemporalIteratorSubModelsTest5

    submodel Sub s with {step=1}
    
    output y = s.x
    t {solver=forwardeuler{dt=1}}
end
