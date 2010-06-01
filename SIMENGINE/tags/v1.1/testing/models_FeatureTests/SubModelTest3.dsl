model (x)=Sub(step)
    state x = 0
    equation x' = step
end

model (y)=SubModelTest3

    state x1 = 0
    equation x1' = 2

    submodel Sub s1 with {step=1}
    submodel Sub s2 with {step=3}
    equation x2 = s1.x
    equation x3 = s2.x

    output y = (x1, x2, x3)
    t {solver=forwardeuler{dt=1}}
end
