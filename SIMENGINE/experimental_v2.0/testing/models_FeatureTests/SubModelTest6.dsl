model (x)=Sub(start)
    state x = start
    equation x' = 1
end

model (y)=SubModelTest6

    state x1 = 0
    equation x1' = 2

    submodel Sub s with {start=0}
    equation x2 = s.x

    output y = (x1, x2)
    t {solver=forwardeuler{dt=1}}
end
