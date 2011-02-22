/* 
Calcium Buffer Object - created by S.S. Feng 01/2007

070305 - Altered instantiation statement to include passing in of B value, since GENESIS seems to do it wrong

This calcium buffer is a single shell model for Ca concentration. It is based on the GENESIS 2.3 version of a calcium buffer. 
It will keep track of the Ca2+ coming in from the calcium channels and apply buffering.

It solves: 	dC/dt = B*Ik - C/tau
		Ca = Ca_Base + C.

Note: this follows SI units, concentration is moles/m^3 (milli-moles/liter). 

tau = time constant of decay
Ca_base = resting (base level) concentration
B = 1/(ion charge * Faraday * volume) // Note that this should start off as 5.2e-6/(shell volume)

Also calculates the Nernst potential for the given ionic concentrations and temperature:

E = scale * (R/zF) * (T + 273.15) * ln (Cout/Cin)

R = universal gas constant = 1.9872 cal/mol deg
F = Faraday's constant = 23061 cal/volt mol. 
scale = Scale Factor in Volts (scale = 1 -> volts, scale = 1e3 -> mV)

The temperature should be entered in Celsius, converted to Kelvin internally. 

*/

/***********************************************************************************************/
model (E, Ca_in) = Ca_Concen (shell_volume, B_vol, Ik)
//model Ca_Concen (DYNAMIC dt, DYNAMIC t, CONSTANT integrate)
// For the GP model. Ik is the current from the Ca_HVA channel

// constants for Ca2+ buffer
constant B = 5.2e-12 // determined experimentally, from simdefaults.g
constant Ca_base = 5e-05 // mM -> 50 nM AFFECTS RANGE OF Ca_in
//B_vol = B/shell_volume
//B_vol = B
//Ik = B
//constant B_vol = 3.439612723e10

// constants for nernst battery
constant R = 1.9872 // cal mol^-1 deg ^-1
constant F = 23061 // cal V^-1 mol ^-1
constant temp = 32 // deg C
constant valency = 2 // Ca2+
constant scale = 1 // generates Volts

// parameters for Ca2+ buffer
//PARAMETER tau (0.001 TO 0.005 BY 0.0005) = 0.001 //s
constant tau = 0.001 //s 

// parameters for nernst battery
//PARAMETER Ca_out (0.1 TO 5 BY 1e-6) = 2 // mM/l?
constant Ca_out = 2

// states for buffer and battery
//STATE E (-0.05 TO 0.15 BY 0.001) = 0
state e = 0.001
//state Ca_in = 5e-5
//STATE C (0 TO 3 BY 1e-6) = 0 //Mol/m^3 or mM/l
state C = 1e-6

equations
  C' = B_vol*Ik - C/tau
  Ca_in = Ca_base + C
//E = scale * E
//E = scale * (R/(valency*F)) * (temp + 273.15)
  E = scale * (R/(valency*F)) * (temp + 273.15) * (ln(Ca_out/Ca_in))
end

end
/***********************************************************************************************/
