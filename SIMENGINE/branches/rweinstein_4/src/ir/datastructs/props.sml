structure Property =
struct

type length = int
type dimlist = length list

type symbolproperty = 
     {dim: dimlist option,
      iterator: Iterator.iterator list option,
      derivative: (int * Symbol.symbol list) option,
      sourcepos: PosLog.pos option,
      realname: Symbol.symbol option}

val default_symbolproperty = 
    {dim=NONE,
     iterator=NONE,
     derivative=NONE,
     sourcepos=NONE,
     realname=NONE}

fun getDim (props:symbolproperty) = #dim props

fun getIterator (props:symbolproperty) = #iterator props
	
fun getSpecificIterator props itersym = 
    case getIterator props of
	SOME iters => List.find (fn(sym,_)=>sym=itersym) iters
      | NONE => NONE

fun getDerivative (props:symbolproperty) = #derivative props

fun getSourcePos (props:symbolproperty) = #sourcepos props

fun getRealName (props:symbolproperty) = #realname props

fun setDim props p = 
    {dim=SOME p,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props}
	
fun setIterator props p = 
    {dim=getDim props,
     iterator=SOME p,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props}
	
fun setDerivative props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=SOME p,
     sourcepos=getSourcePos props,
     realname=getRealName props}
	
fun setSourcePos props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=SOME p,
     realname=getRealName props}
	
fun setRealName props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=SOME p}	

fun getCodeLocStr (props:symbolproperty) = 
    case (#sourcepos props)
     of SOME pos => SOME (PosLog.pos2str pos)
      | NONE => NONE

end
