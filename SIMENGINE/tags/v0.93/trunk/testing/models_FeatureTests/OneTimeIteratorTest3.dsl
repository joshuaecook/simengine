settings.debug.logdof.setValue(true)
model (x)=OneTimeIteratorTest3

iterator t with {continuous,solver=forwardeuler{dt=1}}

state x = 0
equation x' = 1

end
