model (x)=Sub(step)
    state x = 0
    equation x' = step
end

model (y)=SubModelTest4

    state x1 = 0
    equation x1' = 2

    submodel Sub s1 with {step=1}
    submodel Sub s2 with {step=3}

    output y = (x1, s1.x, s2.x)
    t {solver=forwardeuler{dt=1}}
end
