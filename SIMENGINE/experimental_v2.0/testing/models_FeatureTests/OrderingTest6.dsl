model (y) = sub1(x)
  state y = 0
  equation y' = x + 1
end


model (y) = OrderingTest6 ()
  submodel sub1 s1
  submodel sub1 s2 with {x=s1.y}
  submodel sub1 s3 with {x=s2.y}
  s1.x = s3.y

  output y = s1.y

  solver=forwardeuler{dt=1}
end
