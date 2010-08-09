model (y) = sub1(x)

  state y = 0
  equation y' = 1

end

model (y) = sub2(x)

  constant y = 0

end

model (y) = OrderingTest5()

  submodel sub1 s1
  submodel sub2 s2 with {x=s1.y}
  s1.x = s1.y

  output y = s1.y

  solver=forwardeuler{dt=1}
end
