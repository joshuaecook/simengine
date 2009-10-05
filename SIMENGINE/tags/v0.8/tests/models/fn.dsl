task fn (num_iterations: Number)
  quantity u := 1
  quantity w := 1

  //constants(params)
  quantity I := 2
  quantity e := 0.1
  quantity b0 := 2
  quantity b1 := 1.5

  quantity dt := 0.1

  quantity u_log := [u]
  quantity w_log := [w]

  foreach i in 1 .. num_iterations do
    quantity u_new := u - u*u*u / 3 - w + I
    quantity w_new := e * (b0 + b1*u - w)
    
    u := u + dt * u_new
    w := w + dt * w_new

    u_log := u_log.push_front u
    w_log := w_log.push_front w
  end
/*
  print "u is"
  foreach elem in u_log.rev() do
    print elem
    print "  "
  end

  print "             and w is"
  foreach elem in w_log.rev() do
    print elem
    print "  "
  end
*/
end;

fn(1500);
