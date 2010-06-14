settings.debug.logdof.setValue(true)
model (x)=IteratorSyntaxTest2

iterator n1 with {discrete}

state x = 1 with {iter=n1}
equation x' = 1

end
