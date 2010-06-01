model (x)=Sub
  equation x = t
end

model (y)=FlattenSubModelTest2

    state x1 = 0
    equation x1' = 2

    submodel Sub s
    equation x2 = s.x

    output y = (x1, x2[t])
    t {solver=forwardeuler{dt=1}}
end
