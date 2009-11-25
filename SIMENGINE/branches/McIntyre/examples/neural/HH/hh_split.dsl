// Hodgkin & Huxley Giant Squid Axon Model (J Physiol, 1952)
//
// Adapted for use with simEngine
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
//
// model definition

model (Vm) = hh_split(I_app, g_Na, g_K, g_L)
  constant Cm  = 1

  iterator t_exp with {continuous, solver=forwardeuler{dt=0.01}}
  iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.01}}

  input I_app with {default=20}

  input g_Na with {default=120}
  input g_K with {default=36}
  input g_L with {default=0.3}

  constant E_Na = 50
  constant E_K = -77
  constant E_L = -54.4
  
  state Vm = -73.494 with {iter=t_imp}
  
  state h = 0.11646 with {iter=t_exp}
  state m = 0.044793 with {iter=t_exp}
  state n = 0.67952 with {iter=t_exp}

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

    m' = (m_inf - m) / m_tau
    h' = (h_inf - h) / h_tau
    n' = (n_inf - n) / n_tau
  end

  //solver = ode45

end
