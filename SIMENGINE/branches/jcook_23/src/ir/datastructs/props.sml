structure Property =
struct

type length = int
type dimlist = length list

datatype scopetype = LOCAL
		   | READSTATE of Symbol.symbol (* needs to be pulled out of input structure *)
		   | WRITESTATE of Symbol.symbol (* needs to be written back to output structure *)


datatype ep_index_type = STRUCT_OF_ARRAYS | ARRAY

type symbolproperty = 
     {dim: dimlist option,
      iterator: Iterator.iterator list option,
      derivative: (int * Symbol.symbol list) option,
      sourcepos: PosLog.pos option,
      realname: Symbol.symbol option,
      scope: scopetype,
      ep_index: ep_index_type option}

val default_symbolproperty = 
    {dim=NONE,
     iterator=NONE,
     derivative=NONE,
     sourcepos=NONE,
     realname=NONE,
     scope=LOCAL,
     ep_index=NONE}

fun getDim (props:symbolproperty) = #dim props

fun getIterator (props:symbolproperty) = #iterator props
	
fun getSpecificIterator props itersym = 
    case getIterator props of
	SOME iters => List.find (fn(sym,_)=>sym=itersym) iters
      | NONE => NONE

fun getDerivative (props:symbolproperty) = #derivative props

fun getSourcePos (props:symbolproperty) = #sourcepos props

fun getRealName (props:symbolproperty) = #realname props

fun getScope (props:symbolproperty) = #scope props

fun getEPIndex (props:symbolproperty) = #ep_index props

fun setDim props p = 
    {dim=SOME p,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     ep_index=getEPIndex props}
	
fun setIterator props p = 
    {dim=getDim props,
     iterator=SOME p,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     ep_index=getEPIndex props}
	
fun setDerivative props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=SOME p,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     ep_index=getEPIndex props}
	
fun setSourcePos props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=SOME p,
     realname=getRealName props,
     scope=getScope props,
     ep_index=getEPIndex props}
	
fun setRealName props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=SOME p,
     scope=getScope props,
     ep_index=getEPIndex props}	

fun setScope props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=p,
     ep_index=getEPIndex props}	

fun setEPIndex props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     ep_index=p}	

fun getCodeLocStr (props:symbolproperty) = 
    case (#sourcepos props)
     of SOME pos => SOME (PosLog.pos2str pos)
      | NONE => NONE

end
