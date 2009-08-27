import "GP_chans.dsl"
import "Ca_Concen.dsl"

/***********************************************************************************************/
model (Vm) = GP_comp (I_inject, V_cmd,  V_cmd_rdy, length, diameter, surf_area, shell_vol, RA, RM, CM,
      		 E_LEAK,
			 E_REST,
			 E_Na,
			 E_K,
			 E_H,
			 G_mult,
			 G_mult_Na,
			 G_mult_Kdr,
			 G_mult_KA,
			 G_mult_KCNQ,
			 G_mult_SK,
			 G_mult_Ca,
			 G_mult_HCN,
			 Ca_Buff_B,
			 I_axial)

state V = -0.06 //V
//state Vm = -0.06 //V

// Base Conductances - should be consistent across compartments, use multipliers to distinguish
// taken from eric_temp.par (otherwise defined in paspars.g differently)

constant G_Na_fast_GP = 		351.3056
constant G_Na_slow_GP = 		1.0413
constant G_Kv2_GP = 			1.4885
constant G_Kv3_GP =  			9.2162
constant G_Kv4_fast_GP =		21.3162
constant G_Kv4_slow_GP =		G_Kv4_fast_GP * 1.5 
constant G_KCNQ_GP  =			1.8188
constant G_K_ahp_GP  =			4.6366
constant G_Ca_HVA_GP  =			0.3871
constant G_h_HCN_GP  =			0.2430
constant G_h_HCN2_GP  =			G_h_HCN_GP * 2.5 

// initialize channels, connecting them to membrane voltage
submodel chan_NaF Na_Fast_GP with {G_NaF=G_mult*surf_area*G_Na_fast_GP*G_mult_Na, E_Na=E_Na}
submodel chan_NaP Na_Slow_GP with {G_NaP=G_mult*surf_area*G_Na_slow_GP*G_mult_Na, E_Na=E_Na}
submodel chan_Kv2 Kv2_GP with {G_Kv2=G_mult*surf_area*G_Kv2_GP*G_mult_Kdr, E_K=E_K}
submodel chan_Kv3 Kv3_GP with {G_Kv3=G_mult*surf_area*G_Kv3_GP*G_mult_Kdr, E_K=E_K}
submodel chan_Kv4_f Kv4f_GP with {G_Kv4_f=G_mult*surf_area*G_Kv4_fast_GP*G_mult_Kdr, E_K=E_K}
submodel chan_Kv4_s Kv4s_GP with {G_Kv4_s=G_mult*surf_area*G_Kv4_slow_GP*G_mult_Kdr, E_K=E_K}
submodel chan_KCNQ KCNQ_GP with {G_KCNQ=G_mult*surf_area*G_KCNQ_GP*G_mult_KCNQ, E_K=E_K}
submodel chan_h_HCN h_HCN_GP with {G_H=G_mult*surf_area*G_h_HCN_GP*G_mult_HCN, E_H=E_H}
submodel chan_h_HCN2 h_HCN2_GP with {G_H=G_mult*surf_area*G_h_HCN2_GP*G_mult_HCN, E_H=E_H}

/* have to do this because calcium channels are special..."special ed" special */
// setup a placeholder current
//state I_placeholder_Ca = 0

//equation I_placeholder_Ca = CaHVA_GP.I
// instantiate Ca2+ buffer object using placeholder current
submodel Ca_Concen Ca_buffer_GP with {shell_volume=shell_vol, B_vol=Ca_Buff_B}

submodel chan_CaHVA CaHVA_GP with {G_CaHVA=surf_area*G_Ca_HVA_GP*G_mult_Ca, E_Ca=Ca_buffer_GP.E}
Ca_buffer_GP.Ik = CaHVA_GP.I

// instantiate Ca2+ channels, set placeholder current equal to channel currents
submodel chan_SK K_ahp_GP with {G_SK=surf_area*G_K_ahp_GP*G_mult_SK, E_K=E_K, Ca_in=Ca_buffer_GP.Ca_in}

  equations
// FOR TESTING PURPOSES, SINGLE SEGMENT -> NO CONNECTIONS
    Im = I_inject +  Na_Fast_GP.I + Na_Slow_GP.I + Kv2_GP.I + Kv3_GP.I + Kv4f_GP.I + Kv4s_GP.I + KCNQ_GP.I + h_HCN_GP.I + h_HCN2_GP.I + CaHVA_GP.I + K_ahp_GP.I	+ I_axial

	// use Vm or V here? 
/*
	+ (parent_Vm - Vm)/RA
	+ (child_Vm - Vm)/child_RA
*/


    Vm = {V_cmd when V_cmd_rdy,
          V     otherwise}

    // remember, need to add in currents from other compartments
       V' = (1/(CM))*((E_LEAK-V)/(RM) + Im)
  end

  Na_Fast_GP.V = Vm
  Na_Slow_GP.V = Vm
  Kv2_GP.V = Vm
  Kv3_GP.V = Vm
  Kv4f_GP.V = Vm
  Kv4s_GP.V = Vm
  KCNQ_GP.V = Vm
  h_HCN_GP.V = Vm
  h_HCN2_GP.V = Vm
  CaHVA_GP.V = Vm
  K_ahp_GP.V = Vm

end
/***********************************************************************************************/
