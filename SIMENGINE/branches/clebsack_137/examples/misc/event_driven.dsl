model (x, y, five, six, seven)=event_driven(gain)

  state x = 0
  state y = 10

  state five = 5
  state six = 6
  state seven = 7

  input gain with {default=0.1}

  equation x' = gain
  equation y' = 0 - gain

  equation five' = 0
  equation six' = 0
  equation seven' = 0



  solver = forwardeuler {dt=0.1}

  equation x_cond = x >= y
  equation five_cond = y < 1
  equation six_cond = not(x_cond)
  equation y_cond = five_cond or six_cond

  output x with {condition=(x>=y)}
  output five with {condition=y<1}
  output six with {condition=not(x_cond)}
  output y with {condition=five_cond or six_cond}
  output seven with {condition=not(y_cond)}

//  x.setIsVisible(x >= y)

//  five.setIsVisible(y < 1)
//  six.setIsVisible(not(x.getIsVisible()))

//  y.setIsVisible((five.getIsVisible()) or (six.getIsVisible()))
//  seven.setIsVisible(not(y.getIsVisible()))

end