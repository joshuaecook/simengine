/*
 * CircuitElements - Includes all the basic circuit elements that can be
 * combined in a circuit model
 * 
 * Copyright 2010 Simatra Modeling Technologies
 */
model (I) = VoltageSource(V1, V2, Vs)

      // Create a small internal resistance (10 mOhm)
      constant Rinternal = 0.01
      
      // Create a state for I, have it adjust as necessary to produce the desired voltage
      state I = 0
      equation I' = ((V2-V1)-Vs)/Rinternal
      
end

model (I) = Ground(V1)

      // Create a small internal resistance (10 mOhm)
      constant Rinternal = 0.0001
            
      // Create a state for I, have it adjust as necessary to produce the desired voltage
      state I = 0
      equation I' = (-V1)/Rinternal  

end

model (I) = CurrentSource(V1, V2, Is)
      
      equation I = Is      

end


model (I) = Resistor(V1, V2, R)

  equation I = (V2-V1)/R

end

model (I) = Capacitor(V1, V2, C)

  equations
    //dt = t - t[-1]
    dt = 0.0001
    myV1 = V1
    dV1 = V1 - myV1[t[-1]]
    myV2 = V2
    dV2 = V2 - myV2[t[-1]]
    dVdt = (dV2-dV1)/dt
    I = C*dVdt
  end

end

model (I) = Inductor(V1, V2, L)

    // V = L*I'
    // I = (1/L)*Int[V(t),t]
    state integral_V = 0
    equations
	integral_V' = V2-V1 // Int[V'(t),t] => V(t) = V2(t)-V1(t)
	I = (1/L)*integral_V
    end
end

