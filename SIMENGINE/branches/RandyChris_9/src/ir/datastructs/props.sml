structure Property =
struct

type length = int
type dimlist = length list

(* Several namespaces may be available for symbol lookup. *)
datatype scope_type 
  (* Local is the default scope. *)
  = LOCAL
  | READSTATE of Symbol.symbol (* needs to be pulled out of input structure *)
  | READSYSTEMSTATE of Symbol.symbol (* symbol here is the iterator to pull from  *)
  | WRITESTATE of Symbol.symbol (* needs to be written back to output structure *)
  | ITERATOR (* if it is an iterator, it needs to be prepended as such *)

datatype ep_index_type = STRUCT_OF_ARRAYS | ARRAY

type symbolproperty = {
     (* The extents of multidimensional data. *)
     dim: dimlist option,
     iterator: Iterator.iterator list option,
     derivative: (int * Symbol.symbol list) option,
     isevent: bool,
     (* The lexical position of the symbol. *)
     sourcepos: PosLog.pos option,
     (* Symbols may be renamed for compatibility with the C target languages. 
      * The original name is always retained for reports to the user. *)
     realname: Symbol.symbol option,
     scope: scope_type,
     outputbuffer: bool,
     ep_index: ep_index_type option}

val default_symbolproperty = 
    {dim=NONE,
     iterator=NONE,
     derivative=NONE,
     sourcepos=NONE,
     realname=NONE,
     scope=LOCAL,
     isevent=false,
     outputbuffer=false,
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

fun getIsEvent (props:symbolproperty) = #isevent props

fun isOutputBuffer (props:symbolproperty) = #outputbuffer props

fun getEPIndex (props:symbolproperty) = #ep_index props

fun setDim props p = 
    {dim=SOME p,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}

fun setIsEvent props flag = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=flag,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}
	
fun setIterator props p = 
    {dim=getDim props,
     iterator=SOME p,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}
	
fun setDerivative props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=SOME p,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}
	
fun setSourcePos props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=SOME p,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}
	
fun setRealName props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=SOME p,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}	

fun setScope props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=p,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=getEPIndex props}	

fun setOutputBuffer props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=p,
     ep_index=getEPIndex props}	

fun setEPIndex props p = 
    {dim=getDim props,
     iterator=getIterator props,
     derivative=getDerivative props,
     sourcepos=getSourcePos props,
     realname=getRealName props,
     scope=getScope props,
     isevent=getIsEvent props,
     outputbuffer=isOutputBuffer props,
     ep_index=p}	

fun getCodeLocStr (props:symbolproperty) = 
    case (#sourcepos props)
     of SOME pos => SOME (PosLog.pos2str pos)
      | NONE => NONE

end
