/*

Globus Pallidus neuron - created by S.S. Feng 01/2007
This simulation is intended to be a 6 compartment reduced version of the GENESIS model published in Hanson, Smith, and Jaeger 2004. 
The reduced form of the model was originally created by Eric Hendrickson in GENESIS. This model description also uses GP_comp.dyn, 
GP_chans.dyn, Ca_Concen.dyn. 

070306 - Added proximal and distal dendritic compartments. Valided 6 cmpt model. 

070305 - added two new proximal dendrite compartments, modified GP_comp definition to recieve the parent compartment's
	 Vm and the child's RA and Vm. This is designed in the same way the "readcell" command in GENESIS defines parents 
	 and children. The current compartment passes and recieves current to its parents via its own RA.
	 *** actually just passed in axial current for now...handled outside

070202 - solved all channel precision problems, except for chanSK, which does not really cause an precision problems

070129 - h_HCN current causes precision problems. This is a soma only version.

070123 - IMPORT function does not allow for nested import of files and nested instantiation of systems. As such, 
	the GP_comp system definition has been moved into GP_neuron.dyn to bypass this. Keep in mind that when building a network,
	the channel defintions and Ca_Concen definition will probably be moved into here as well. 

Globus Pallidus prototypic compartment - created by S.S. Feng 10/2006
Prototypic compartment, use comp_prop vector to set morphological properties, and chan_dens to determine channel densities,
By using a vector of channel densities, can selectively add or subtract channels. All channel prototypes are defined in GP_chans.dyn
This is the asymmetric compartment, with Ra on one side.

GENESIS integrates this equation:
C*dV/dt = (Em-Vm)/Rm + SUM(Ek-Vm)*Gk + inject

070116 - The import of GP_chans.dyn has now been set specifically for S. Feng's home folder. Cannot seem to get around this issue 
061227 - While channel testing, built and simulated channels one by one, some, not all validated, all units associated with circuits corrected

"Im" includes the current flowing through the segment due to the voltage difference between this segment and its connecting segments and the axial resistance Ra
"Im" also includes the SUM(Ek-Vm)*Gk

*/

//FUN pow (x, y) = 
//	exp (y * log (x));

import "GP_comp.dsl"
import "Ca_Concen.dsl"
settings.debug.logdof.setValue(true)
settings.debug.generateC.setValue(true)

/***********************************************************************************************/
model (Vm)=GP_neuron (I_cmd, I_cmd_Rdy, soma_V_clamp, soma_V_clamp_isRdy) 	
// AS OF NOW, ONLY SOMA HAS CURRENT INJECTION AND VOLTAGE CLAMP

input I_cmd with {default=0}
input I_cmd_Rdy with {default=0}
input soma_V_clamp with {default=-0.6}
input soma_V_clamp_isRdy with {default=0}

constant PI = 3.14159

// overall reversal potentials taken from simdefaults.g
constant E_Na = 0.050 //V
constant E_K = -0.090 //V
//PARAMETER E_Ca (-0.05 TO 0.15 BY 0.001) = 0.130 //V not used, calculated by Ca_concen
constant E_H = -0.030 //V
constant G_mult = 			1

///////////////******************************************************************////////////////
// build spherical soma 

// taken from eric_temp.par 
constant G_mult_Na_soma = 		21.2746 
constant G_mult_Kdr_soma =		21.629
constant G_mult_KA_soma = 		3.2682
constant G_mult_KCNQ_soma = 		1.8021
constant G_mult_SK_soma =		30.2618
constant G_mult_Ca_soma = 		1.9494
constant G_mult_HCN_soma =		1

// taken from GP1_defaults.g
constant RA_sd = 1.04 
constant RM_sd = 1.6108
constant CM_sd = 0.0398
constant E_LEAK_sd = -0.06 
constant E_REST_ACT = -0.06

// taken from GP1.p and GPcomps.g
constant soma_len = 0
constant soma_dia = 13.4e-6
constant soma_shell_thick = 20e-9


// intermediates
equations
	soma_rad = soma_dia / 2
	soma_core_rad = soma_rad - soma_shell_thick

	soma_surf_area = 4 * PI * soma_rad^2
	soma_vol = (4/3) * PI * soma_rad^3
	soma_core_vol = (4/3) * PI * soma_core_rad^3
	soma_shell_vol = soma_vol - soma_core_vol

	RM_norm_soma = RM_sd/soma_surf_area
	// taken from GPcomps.g
	RA_norm_soma = RA_sd*8/(soma_dia*PI)
	CM_norm_soma = CM_sd*soma_surf_area

	I_inject = {I_cmd when I_cmd_Rdy,
		    0 otherwise}

end

// have to do this because still don't know how GENESIS modifies B value in Ca_Concen
constant B_vol_soma = 3.439612723e10
//state I_axial_soma = 0

submodel GP_comp soma with {I_inject=I_inject,
			    V_cmd=soma_V_clamp,
            		    V_cmd_rdy=soma_V_clamp_isRdy,
			    length=soma_len,
			    diameter=soma_dia,
			    surf_area=soma_surf_area,
			    shell_vol=soma_shell_vol,
			    RA=RA_norm_soma,
			    RM=RM_norm_soma,
			    CM=CM_norm_soma,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_soma,
			    G_mult_Kdr=G_mult_Kdr_soma,
			    G_mult_KA=G_mult_KA_soma,
			    G_mult_KCNQ=G_mult_KCNQ_soma,
			    G_mult_SK=G_mult_SK_soma,
			    G_mult_Ca=G_mult_Ca_soma,
			    G_mult_HCN=G_mult_HCN_soma,
			    Ca_Buff_B=B_vol_soma}
/*
SYSTEM soma = new GP_comp 	[dt, t, integrate, 
				I_inject, 
				soma_V_clamp, 
				soma_V_clamp_isRdy, 
				soma_len, soma_dia, 
				soma_surf_area, 
				soma_shell_vol, 
				RA_norm_soma,
				RM_norm_soma,
				CM_norm_soma,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_soma,
				G_mult_Kdr_soma,
				G_mult_KA_soma,
				G_mult_KCNQ_soma,
				G_mult_SK_soma,
				G_mult_Ca_soma,
				G_mult_HCN_soma,
				B_vol_soma,
				I_axial_soma
				]*/				

// end spherical soma
///////////////******************************************************************////////////////


///////////////******************************************************************////////////////
// build proximal dendrites, only diff from distal dendrites is size and passive parameters

// taken from eric_temp.par
// note: all dendrites have the same conductance multipliers in the six compt model, so both prox and dend
// prototypes will use these multipliers
constant G_mult_Na_dend = 		0 
constant G_mult_Kdr_dend =		0
constant G_mult_KA_dend = 		1.6445
constant G_mult_KCNQ_dend = 		1.0601
constant G_mult_SK_dend =		1.4350
constant G_mult_Ca_dend = 		0.6852
constant G_mult_HCN_dend = 		0.8195

// taken from GP1_defaults.g
constant RA_prox = 0.9851 
constant RM_prox = 1.15028
constant CM_prox = 0.026

// dend1 and dend2 share the same physical properties
// taken from GP12.p and GPcomps.g
constant prox_len = 400e-6
constant prox_dia = 2.674e-6

// note: it appears that the shell thickness is the same across all compts
constant prox_shell_thick = 20e-9

// intermediates
equations
	prox_rad = prox_dia / 2
	prox_core_rad = prox_rad - prox_shell_thick

	// cylinder
	prox_surf_area = 2 * PI * prox_rad * prox_len
	prox_vol = PI * prox_rad^2 * prox_len
	prox_core_vol = PI * prox_core_rad^2 * prox_len

	prox_shell_vol = {prox_vol - prox_core_vol when prox_vol > prox_shell_thick,
         	          prox_vol otherwise}
/*	prox_shell_vol = IF (prox_vol > prox_shell_thick) THEN
				prox_vol - prox_core_vol
				ELSE
				prox_vol*/

	RM_norm_prox = RM_prox/prox_surf_area
	// taken from GPcomps.g
	RA_norm_prox = RA_prox*4*prox_len/(prox_dia^2*PI)
	CM_norm_prox = CM_prox*prox_surf_area
end
/*
I_inject = IF I_cmd_Rdy THEN
		I_cmd
		ELSE
		0
*/

// BAD HACKS
constant B_vol_prox = 2.893617357e10
constant FAKE_INPUT = 0
//state I_axial_dend1 = 0
//state I_axial_dend2 = 0

submodel GP_comp dend1 with {I_inject=FAKE_INPUT,
			    V_cmd=FAKE_INPUT,
            		    V_cmd_rdy=FAKE_INPUT,
			    length=prox_len,
			    diameter=prox_dia,
			    surf_area=prox_surf_area,
			    shell_vol=prox_shell_vol,
			    RA=RA_norm_prox,
			    RM=RM_norm_prox,
			    CM=CM_norm_prox,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_dend,
			    G_mult_Kdr=G_mult_Kdr_dend,
			    G_mult_KA=G_mult_KA_dend,
			    G_mult_KCNQ=G_mult_KCNQ_dend,
			    G_mult_SK=G_mult_SK_dend,
			    G_mult_Ca=G_mult_Ca_dend,
			    G_mult_HCN=G_mult_HCN_dend,
			    Ca_Buff_B=B_vol_prox}
/*
SYSTEM dend1 = new GP_comp 	[dt, t, integrate, 
				FAKE_INPUT,
				FAKE_INPUT,
				FAKE_INPUT,
				prox_len, 
				prox_dia, 
				prox_surf_area, 
				prox_shell_vol, 
				RA_norm_prox,
				RM_norm_prox,
				CM_norm_prox,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_dend,
				G_mult_Kdr_dend,
				G_mult_KA_dend,
				G_mult_KCNQ_dend,
				G_mult_SK_dend,
				G_mult_Ca_dend,
				G_mult_HCN_dend,
				B_vol_prox,
				I_axial_dend1]
*/
submodel GP_comp dend2 with {I_inject=FAKE_INPUT,
			    V_cmd=FAKE_INPUT,
            		    V_cmd_rdy=FAKE_INPUT,
			    length=prox_len,
			    diameter=prox_dia,
			    surf_area=prox_surf_area,
			    shell_vol=prox_shell_vol,
			    RA=RA_norm_prox,
			    RM=RM_norm_prox,
			    CM=CM_norm_prox,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_dend,
			    G_mult_Kdr=G_mult_Kdr_dend,
			    G_mult_KA=G_mult_KA_dend,
			    G_mult_KCNQ=G_mult_KCNQ_dend,
			    G_mult_SK=G_mult_SK_dend,
			    G_mult_Ca=G_mult_Ca_dend,
			    G_mult_HCN=G_mult_HCN_dend,
			    Ca_Buff_B=B_vol_prox}
/*
SYSTEM dend2 = new GP_comp 	[dt, t, integrate, 
				FAKE_INPUT,
				FAKE_INPUT,
				FAKE_INPUT,
				prox_len, 
				prox_dia, 
				prox_surf_area, 
				prox_shell_vol, 
				RA_norm_prox,
				RM_norm_prox,
				CM_norm_prox,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_dend,
				G_mult_Kdr_dend,
				G_mult_KA_dend,
				G_mult_KCNQ_dend,
				G_mult_SK_dend,
				G_mult_Ca_dend,
				G_mult_HCN_dend,
				B_vol_prox,
				I_axial_dend2]
*/
// end proximal dendrites
///////////////******************************************************************////////////////

///////////////******************************************************************////////////////
// build distal dendrites, only difference from proximal dendrites is size and passive parameters
// note: all dendrites have the same conductance multipliers in the six compt model, specified above

// taken from GP1_defaults.g
constant RA_dist = 1.3285 
constant RM_dist = 1.6711
constant CM_dist = 0.0234

// dend1a, dend2a, and dend2b all share the same physical properties
// taken from GP12.p and GPcomps.g
constant dist_len = 200e-6
constant dist_dia = 0.36e-6

// note: it appears that the shell thickness is the same across all compts
constant dist_shell_thick = 20e-9

// intermediates 
equations
	dist_rad = dist_dia / 2
	dist_core_rad = dist_rad - dist_shell_thick

	// cylinder
	dist_surf_area = 2 * PI * dist_rad * dist_len
	dist_vol = PI * dist_rad^2 * dist_len
	dist_core_vol = PI * dist_core_rad^2 * dist_len

	dist_shell_vol = {dist_vol - dist_core_vol when dist_vol > dist_shell_thick,
		       	  dist_vol otherwise}
/*
	dist_shell_vol = IF (dist_vol > dist_shell_thick) THEN
				dist_vol - dist_core_vol
				ELSE
				dist_vol
*/
	RM_norm_dist = RM_dist/dist_surf_area
	// taken from GPcomps.g
	RA_norm_dist = RA_dist*4*dist_len/(dist_dia^2*PI)
	CM_norm_dist = CM_dist*dist_surf_area
end

/*
I_inject = IF I_cmd_Rdy THEN
		I_cmd
		ELSE
		0
*/

// BAD HACKS
constant B_vol_dist = 3.192925716e12
constant FAKE_INPUT = 0
//state I_axial_dend1a = 0
//state I_axial_dend2a = 0
//state I_axial_dend2b = 0

submodel GP_comp dend1a with {I_inject=FAKE_INPUT,
			    V_cmd=FAKE_INPUT,
            		    V_cmd_rdy=FAKE_INPUT,
			    length=dist_len,
			    diameter=dist_dia,
			    surf_area=dist_surf_area,
			    shell_vol=dist_shell_vol,
			    RA=RA_norm_dist,
			    RM=RM_norm_dist,
			    CM=CM_norm_dist,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_dend,
			    G_mult_Kdr=G_mult_Kdr_dend,
			    G_mult_KA=G_mult_KA_dend,
			    G_mult_KCNQ=G_mult_KCNQ_dend,
			    G_mult_SK=G_mult_SK_dend,
			    G_mult_Ca=G_mult_Ca_dend,
			    G_mult_HCN=G_mult_HCN_dend,
			    Ca_Buff_B=B_vol_dist}

/*
SYSTEM dend1a = new GP_comp 	[dt, t, integrate, 
				FAKE_INPUT,
				FAKE_INPUT,
				FAKE_INPUT,
				dist_len, 
				dist_dia, 
				dist_surf_area, 
				dist_shell_vol, 
				RA_norm_dist,
				RM_norm_dist,
				CM_norm_dist,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_dend,
				G_mult_Kdr_dend,
				G_mult_KA_dend,
				G_mult_KCNQ_dend,
				G_mult_SK_dend,
				G_mult_Ca_dend,
				G_mult_HCN_dend,
				B_vol_dist,
				I_axial_dend1a]
*/
submodel GP_comp dend2a with {I_inject=FAKE_INPUT,
			    V_cmd=FAKE_INPUT,
            		    V_cmd_rdy=FAKE_INPUT,
			    length=dist_len,
			    diameter=dist_dia,
			    surf_area=dist_surf_area,
			    shell_vol=dist_shell_vol,
			    RA=RA_norm_dist,
			    RM=RM_norm_dist,
			    CM=CM_norm_dist,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_dend,
			    G_mult_Kdr=G_mult_Kdr_dend,
			    G_mult_KA=G_mult_KA_dend,
			    G_mult_KCNQ=G_mult_KCNQ_dend,
			    G_mult_SK=G_mult_SK_dend,
			    G_mult_Ca=G_mult_Ca_dend,
			    G_mult_HCN=G_mult_HCN_dend,
			    Ca_Buff_B=B_vol_dist}

/*
SYSTEM dend2a = new GP_comp 	[dt, t, integrate, 
				FAKE_INPUT,
				FAKE_INPUT,
				FAKE_INPUT,
				dist_len, 
				dist_dia, 
				dist_surf_area, 
				dist_shell_vol, 
				RA_norm_dist,
				RM_norm_dist,
				CM_norm_dist,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_dend,
				G_mult_Kdr_dend,
				G_mult_KA_dend,
				G_mult_KCNQ_dend,
				G_mult_SK_dend,
				G_mult_Ca_dend,
				G_mult_HCN_dend,
				B_vol_dist,
				I_axial_dend2a]
*/

submodel GP_comp dend2b with {I_inject=FAKE_INPUT,
			    V_cmd=FAKE_INPUT,
            		    V_cmd_rdy=FAKE_INPUT,
			    length=dist_len,
			    diameter=dist_dia,
			    surf_area=dist_surf_area,
			    shell_vol=dist_shell_vol,
			    RA=RA_norm_dist,
			    RM=RM_norm_dist,
			    CM=CM_norm_dist,
			    E_LEAK=E_LEAK_sd,
			    E_REST=E_REST_ACT,
			    E_Na=E_Na,
			    E_K=E_K,
			    E_H=E_H,
			    G_mult=G_mult,
			    G_mult_Na=G_mult_Na_dend,
			    G_mult_Kdr=G_mult_Kdr_dend,
			    G_mult_KA=G_mult_KA_dend,
			    G_mult_KCNQ=G_mult_KCNQ_dend,
			    G_mult_SK=G_mult_SK_dend,
			    G_mult_Ca=G_mult_Ca_dend,
			    G_mult_HCN=G_mult_HCN_dend,
			    Ca_Buff_B=B_vol_dist}
/*
SYSTEM dend2b = new GP_comp 	[dt, t, integrate, 
				FAKE_INPUT,
				FAKE_INPUT,
				FAKE_INPUT,
				dist_len, 
				dist_dia, 
				dist_surf_area, 
				dist_shell_vol, 
				RA_norm_dist,
				RM_norm_dist,
				CM_norm_dist,
				E_LEAK_sd,
				E_REST_ACT,
				E_Na,
				E_K,
				E_H,
				G_mult,			
				G_mult_Na_dend,
				G_mult_Kdr_dend,
				G_mult_KA_dend,
				G_mult_KCNQ_dend,
				G_mult_SK_dend,
				G_mult_Ca_dend,
				G_mult_HCN_dend,
				B_vol_dist,
				I_axial_dend2b]*/

// end distal dendrites
///////////////******************************************************************////////////////


///////////////******************************************************************////////////////
// RESOLVE AXIAL CURRENTS, FOR NOW THIS IS WHERE THE CONNECTIONS ARE SET UP

equations
// soma parent dend1 and dend2
I_axial_soma = ((dend1.Vm - soma.Vm)/RA_norm_prox + (dend2.Vm - soma.Vm)/RA_norm_prox)

// dend1 child to soma, parent to dend1a
I_axial_dend1 = (soma.Vm - dend1.Vm)/RA_norm_prox + (dend1a.Vm - dend1.Vm)/RA_norm_dist
I_axial_dend1a = (dend1.Vm - dend1a.Vm)/RA_norm_dist

// dend2 child to soma, parent to dend2a, parent to dend2b
I_axial_dend2 = (soma.Vm - dend2.Vm)/RA_norm_prox + (dend2a.Vm - dend2.Vm)/RA_norm_dist + (dend2b.Vm - dend2.Vm)/RA_norm_dist
I_axial_dend2a = (dend2.Vm - dend2a.Vm)/RA_norm_dist
I_axial_dend2b = (dend2.Vm - dend2b.Vm)/RA_norm_dist
end

soma.I_axial = I_axial_soma
dend1.I_axial = I_axial_dend1
dend1a.I_axial = I_axial_dend1a
dend2.I_axial = I_axial_dend2
dend2a.I_axial = I_axial_dend2a
dend2b.I_axial = I_axial_dend2b


///////////////******************************************************************////////////////

output Vm = (soma.Vm, dend1.Vm, dend2.Vm, dend1a.Vm, dend2a.Vm)

solver=forwardeuler{dt=1e-6}


end
/***********************************************************************************************/

/***********************************************************************************************/
/*MAIN
  FUN euler_integrate (dt, t, state, eq) =
    state + dt * eq(t)

  FUN integrate (dt, t, state, eq) = 
	euler_integrate (dt, t, state, eq)

  state t (0 TO 100 BY 1E-6) = 0
  constant dt (1E-6 TO 1E-5 BY 1E-6) = 5E-6 //s
  //constant dt = 1E-6
  t=t+dt

//  INPUT I_cmd (1E-9 TO 40E-9 BY 1E-10)
//  INPUT V_clamp (-0.100 TO 0.100 BY 0.0001) //V
  constant I_cmd (1E-9 TO 40E-9 BY 1E-10) = 1E-9
  constant V_clamp (-0.100 TO 0.100 BY 0.0001) = 0 //V

  SYSTEM test_neuron = new GP_neuron [dt, t, integrate, I_cmd, I_cmd.isReady, V_clamp, V_clamp.isReady]

  OUTPUT "t", "test_neuron.*.Vm"
  //OUTPUT "test_neuron.RA*"
  //OUTPUT "test_neuron.soma.Ca_buffer_GP.B_vol"

  OUTPUTRATE 10.0
  keep_running = t < 1

ENDMAIN*/
/***********************************************************************************************/
