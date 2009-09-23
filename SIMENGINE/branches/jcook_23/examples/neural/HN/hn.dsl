settings.debug.logdof.setValue(true)
settings.debug.generateC.setValue(true)
import "somaCompartment.dsl"
import "synapseCompartment.dsl"
import "compartment.dsl"

function join1(c1, c2)
  //function to join a section to another section
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2)
end

function join2(c1, c2, c3)
  //function to join a section to 2 other sections
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2) + (c1.Vm - c3.Vm)/((c1.Raxonal + c3.Raxonal)/2)
end

function join3(c1, c2, c3, c4)
  //function to join a section to 3 other sections
  c1.Iaxonal = (c1.Vm - c2.Vm)/((c1.Raxonal + c2.Raxonal)/2) + (c1.Vm - c3.Vm)/((c1.Raxonal + c3.Raxonal)/2) + (c1.Vm - c4.Vm)/((c1.Raxonal + c4.Raxonal)/2)
end

model (VmS, VmA) = hn(Istim, Vpre)
	input Istim with {default = 0}
	input Vpre with {default = -100}

	constant soma_gK1 = 1.99
	constant soma_gK2 = 2.30
	constant soma_gKA = 13.00

	constant neurite_gK1 = 2.80
	constant neurite_gK2 = 7.50
	constant neurite_gKA = 8.78
	constant neurite_gh = 0.42
	constant neurite_gP = 0.11
	constant neurite_gCaF = 0.13
	constant neurite_gCaS = 0.11

	constant neurite2_gK1 = 1.20
	constant neurite2_gK2 = 5.22
	constant neurite2_gKA = 1.69
	constant neurite2_gh = 0.03
	constant neurite2_gP = 0.07
	constant neurite2_gCaF = 0.28
	constant neurite2_gCaS = 0.37

	constant axon_gNa = 7.4
	constant axon_gK1 = 1.22
	constant axon_gK2 = 6.49
	constant axon_gKA = 4.50

	constant cell_Eleak = -66.69
	constant cell_Rm = 1.24e4

	submodel somaCompartment soma with {Istim = Istim,
												gK1 = soma_gK1,
												gK2 = soma_gK2,
												gKA = soma_gKA,
												Rm = cell_Rm,
												Eleak = cell_Eleak}
	
	submodel compartment neuriteSoma with {length =  29.58, 
														diameter = 3.81, 
														gNa = 0.25*axon_gNa, 
														gK1 = (soma_gK1 + neurite_gK1)/2,
														gK2 = (soma_gK2 + neurite_gK2)/2,
														gKA = (soma_gKA + neurite_gKA)/2,
														gh = neurite_gh/2, 
														gP = neurite_gP/2, 
														gCaF = neurite_gCaF/2, 
														gCaS = neurite_gCaS/2,
														Rm = cell_Rm,
														Eleak = cell_Eleak}

	submodel compartment neurite1 with {	length =  199.28, 
														diameter = 5.43, 
														gNa = 0.5*axon_gNa, 
														gK1 = neurite_gK1,
														gK2 = neurite_gK2,
														gKA = neurite_gKA,
														gh = neurite_gh, 
														gP = neurite_gP, 
														gCaF = neurite_gCaF, 
														gCaS = neurite_gCaS,
														Rm = cell_Rm,
														Eleak = cell_Eleak}	

	submodel compartment neuriteAxon1 with {	length =  28.49, 
															diameter = 2.79, 
															gNa = 0.75*axon_gNa,
															gK1 = (3/3)*neurite_gK1+(0/3)*axon_gK1,
															gK2 = (3/3)*neurite_gK2+(0/3)*axon_gK2,
															gKA = (3/3)*neurite_gKA+(0/3)*axon_gKA,
															gh = (2/3)*neurite_gh, 
															gP = (2/3)*neurite_gP, 
															gCaF = (2/3)*neurite_gCaF, 
															gCaS = (2/3)*neurite_gCaS,
															Rm = cell_Rm,
															Eleak = cell_Eleak}

	submodel compartment neuriteAxon2 with {	length =  14.76, 
															diameter = 2.38, 
															gNa = axon_gNa,
															gK1 = (3/3)*neurite_gK1+(0/3)*axon_gK1 ,
															gK2 = (3/3)*neurite_gK2+(0/3)*axon_gK2,
															gKA = (3/3)*neurite_gKA+(0/3)*axon_gKA,
															gh = (1/3)*neurite_gh, 
															gP = (1/3)*neurite_gP, 
															gCaF = (1/3)*neurite_gCaF, 
															gCaS = (1/3)*neurite_gCaS,
															Rm = cell_Rm,
															Eleak = cell_Eleak} 
															
	submodel compartment axon with {		length =  154.09, 
													diameter = 8.09, 
													gNa = axon_gNa,
													gK1 = axon_gK1,
													gK2 = axon_gK2,
													gKA = axon_gKA,	
													gh = 0, 
													gP = 0, 
													gCaF = 0, 
													gCaS = 0,
													Rm = cell_Rm,
													Eleak = cell_Eleak}

	submodel compartment neurite2 with {			length =  253.07, 
																diameter = 11.68, 
																gNa = 0, 
																gK1 = neurite2_gK1,
																gK2 = neurite2_gK2,
																gKA = neurite2_gKA,
																gh = neurite2_gh, 
																gP = neurite2_gP, 
																gCaF = neurite2_gCaS, 
																gCaS = neurite2_gCaF,
																Rm = cell_Rm,
																Eleak = cell_Eleak}

	submodel synapseCompartment synaptic with {	length =  66.89, 
														diameter = 2.38, 
														gNa = 0, 
														gK1 = neurite2_gK1,
														gK2 = neurite2_gK2,
														gKA = neurite2_gKA,
														gh = neurite2_gh, 
														gP = neurite2_gP, 
														gCaF = neurite2_gCaS, 
														gCaS = neurite2_gCaF,
														Rm = cell_Rm,
														Eleak = cell_Eleak,
														Vpre = Vpre}

	
    equation myVmA = axon.Vm
    output VmS = soma.Vm
	output VmA = myVmA
	//output VmN1 = neurite1.Vm
	//output VmN2 = neurite2.Vm

	join1(soma, neuriteSoma)
	//output Isoma = (soma.Vm - neuriteSoma.Vm)/((soma.Raxonal + neuriteSoma.Raxonal)/2)
	join2(neuriteSoma, soma, neurite1)
	join3(neurite1, neuriteSoma, neuriteAxon1, neurite2)	
	join2(neuriteAxon1, neurite1, neuriteAxon2)
	join2(neuriteAxon2, neuriteAxon1, axon)
	join1(axon, neuriteAxon2)
	//output Iaxon = (axon.Vm - neuriteAxon2.Vm)/((axon.Raxonal + neuriteAxon2.Raxonal)/2)

	join2(neurite2, neurite1, synaptic)
	join1(synaptic, neurite2)

	//solver = ode23 {abstol=1e-5, reltol=1e-9}
	solver = cvode//ode23//forwardeuler
	solver.dt = 1e-5
end
