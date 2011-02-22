/* Globus Pallidus Channel Kinetics - provided by J. Edgerton
Adapted for DYNAMO by S.S. Feng 10/2006, updated 12/2006

070223 - Removed NaP s-gate, causes too many precisions problems

070212 - Fixed h currents, tau_min needs to be smaller than 1 ms. 

070202 - Changed all parameters to constants. Only conductance multipliers (not seen at this level),
	 will be parameters. Looks like less RAM, not less multipliers.
	 Also, tried to initialize state variables to values seen in GENESIS after a "reset" command
	 Also, put S gate back into chan_NaP

070128 - Modified heavily to eliminate lots of parameters, turned into constants to aid in precision for FPGA

070122 - Added and validated chan_CaHVA, validated chan_h_HCN, chan_h_HCN2

070111 - Validated chan_KCNQ, chan_NaF, built chan_h_HCN, chan_h_HCN2 (not yet validated)

061227 - fixed units for chan_NaP, chan_Kv2, chan_Kv3, chan_Kv4f, chan_Kv4s (validated most of these), see S. Feng for matlab data figures

061222 - checked all units and ranges of chan_NaF, still not getting correct currents

061219 - Modified chan_NaF to use V instead of mV

FIXED chan_NaF FOR GP MODEL 11/13/2006
FIXED chan_NaP FOR GP MODEL 11/13/2006 - NO S GATE
ADDED chan_Kv2 FOR GP MODEL 11/13/2006
ADDED chan_Kv3 FOR GP MODEL 11/13/2006 

*/

import "chan_NaF.dsl"
import "chan_NaP.dsl"
import "chan_Kv2.dsl"
import "chan_Kv3.dsl"
import "chan_Kv4_f.dsl"
import "chan_Kv4_s.dsl"  
import "chan_KCNQ.dsl" 
import "chan_CaHVA.dsl"
import "chan_h_HCN.dsl"
import "chan_h_HCN2.dsl"
import "chan_SK.dsl"
  
  
  
  
  