model (x) = Sub1(step)

    iterator t1 with {continuous,solver=forwardeuler{dt=1}}
    state x = 0 with {iter=t1}
    equation x' = step
    
end
model (x) = Sub2(step)

    iterator t2 with {continuous,solver=forwardeuler{dt=1}}
    state x = 0 with {iter=t2}
    equation x' = step
    
end

model (y1,y2)=TemporalIteratorSubModelsTest2

    iterator t1 with {continuous,solver=forwardeuler{dt=1}}
    iterator t2 with {continuous,solver=forwardeuler{dt=1}}

    submodel Sub1 s1 with {step=1}
    submodel Sub2 s2 with {step=2}
    
    output y1 = s1.x
    output y2 = s2.x

end
