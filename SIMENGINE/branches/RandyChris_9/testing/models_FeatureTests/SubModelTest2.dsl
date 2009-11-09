model (x)=Sub(step)
    state x = 0
    equation x' = step
end

model (y)=SubModelTest2

    state x1 = 0
    equation x1' = 2

    submodel Sub s with {step=1}
    equation x2 = s.x

    output y = (x1, x2)
    t {solver=forwardeuler{dt=1}}
end
