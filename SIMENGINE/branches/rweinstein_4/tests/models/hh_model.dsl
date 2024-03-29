// model definition

model HH
  constant Cm  = 1

  parameter I_app (0 to 40 by 0.1) = 20

  parameter g_Na (0 to 240 by 0.01) = 120
  parameter g_K  (0 to 72 by 0.01) = 36
  parameter g_L  (0 to 0.6 by 0.01) = 0.3

  constant E_Na = 50
  constant E_K = -77
  constant E_L = -54.4
  
  state Vm (-100 to 100 by 0.0001) = -60
  
  state h (0 to 1 by 0.0001) = 0.9
  state m (0 to 1 by 0.00001) = 0.1
  state n (0 to 1 by 0.0001) = 0.1

  function x_inf(alpha, beta) = alpha / (alpha + beta)
  function x_tau(alpha, beta) = 1 / (alpha + beta)

  task gate_var(alpha, beta, x)
    constant x_inf = alpha / (alpha + beta)
    constant x_tau = 1 / (alpha + beta)

    (x - x_inf) / x_tau
  end

  equations
    alpha_m = 0.1 * (Vm + 40) / (1 - exp(-(Vm + 40)/(10)))
    beta_m  = 4 * exp(-(Vm + 65)/(20))
    alpha_h  = 0.07 * exp(-(Vm + 65)/(20))
    beta_h   = 1/(1 + exp(-(Vm + 35)/(10)))
    alpha_n  = (0.01 * (Vm + 55))/(1 - exp(-(Vm + 55)/(10)))
    beta_n   = 0.125 * exp(-(Vm + 65)/(80))
    
    I_Na = g_Na * m*m*m * h * (Vm - E_Na)
    I_K  = g_K * n*n*n*n * (Vm - E_K)
    I_L  = g_L * (Vm - E_L)
  
    I_sum = -(I_Na + I_K + I_L - I_app)

    Vm' = I_sum / Cm

    m_inf = alpha_m / (alpha_m + beta_m)
    m_tau = 1 / (alpha_m + beta_m)

    h_inf = alpha_h / (alpha_h + beta_h)
    h_tau = 1 / (alpha_h + beta_h)

    n_inf = alpha_n / (alpha_n + beta_n)
    n_tau = 1 / (alpha_n + beta_n)

    //m' = gate_var(alpha_m, beta_m, m)
    m' = (m_inf - m) / m_tau
    h' = (h_inf - h) / h_tau
    n' = (n_inf - n) / n_tau
  end

  t.setPrecision (Range.new(0, 1000, 0.0005))

  settings.integrationMethod.dt = dt
  dt.setInitialValue(0.001)
  dt.setPrecision(Range.new(0,1,0.0005))

  setVisible |Vm|
  setVisible |?|
  setVisible |t|
end
