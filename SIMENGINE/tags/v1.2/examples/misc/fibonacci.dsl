model (y) = fibonacci

state y1 = 0
state y = 1

equation y[n+1] = y[n] + y1
equation y1[n+1] = y[n]

end
