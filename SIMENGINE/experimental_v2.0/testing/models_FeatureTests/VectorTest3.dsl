model VectorTest3
    
    iterator i = 1..10

    state y[i] = 0
    equation y'[i] = {y[i] when i == i.low or i == i.high,
    	     	      (y[i-1] + y[i+1]) / 2 otherwise}


    state yn[i] = 0 with {iter = n}
    equation yn[n,i] = {yn[i] when i == i.low or i == i.high,
    	     	      (yn[n,i-1] + yn[n,i+1]) / 2 otherwise}

    state x[i] = 0
    equation x[i=i.low+1 .. i.high-1]' = (x[i-1] + x[i+1]) / 2
    equation x[i=i.low]' = x[i]
    equation x[i=i.high]' = x[i]

    state xn[i] = 0 with {iter = n}
    equation xn[n, i=i.low+1 .. i.high-1]' = (xn[i-1] + xn[i+1]) / 2
    equation x[n, i=i.low] = x[i]
    equation x[n, i=i.high] = x[i]

    solver=forwardeuler{dt=1}
end
