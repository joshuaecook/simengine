model (x1,x2)=TwoTemporalIteratorTest11

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}

    output x1[t1] = t1 + t2
    output x2[t2] = t1 + t2

end
