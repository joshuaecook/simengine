structure Inst =
struct

fun inst2props f : Fun.op_props = 
    let
	val classes = CurrentModel.classes()
    in
	case (List.find (fn({name,...}:DOF.class)=>name=f) classes)
	 of SOME {name,properties,inputs,outputs,exps,eqs} => {name=Symbol.name f,
							       operands=Fun.FIXED (length (!inputs)),
							       precedence=1,
							       commutative=false,
							       associative=false,
							       text=(Symbol.name f, Fun.PREFIX),
							       C=(Symbol.name f, Fun.PREFIX)}
	  | NONE => (Logger.log_internalerror (Printer.$("Can't handle operation '" ^ (Symbol.name f) ^ ". Doesn't exist in current classes: "
							 ^(String.concatWith ", " (map (fn{name,...}=>Symbol.name name) classes))^ "}"));
		     DynException.setErrored();
		     {name=Symbol.name f,
		      operands=Fun.FIXED 0,
		      precedence=1,
		      commutative=false,
		      associative=false,
		      text=("<?" ^ (Symbol.name f) ^ ">", Fun.PREFIX),
		      C=("<?" ^ (Symbol.name f) ^ ">", Fun.PREFIX)})
    end
    handle e => DynException.checkpoint "Inst.inst2props" e

(* handle instance properties *)
val emptyinstprops = {dim=NONE,
		      sourcepos=NONE,
		      realname=NONE}

fun getDim (props : Fun.instproperties) = #dim props
fun getSourcePos (props : Fun.instproperties)= #sourcepos props
fun getRealName (props : Fun.instproperties)= #realname props

fun setDim (props as {dim, sourcepos, realname} : Fun.instproperties) sym = 
    {dim=SOME sym,
    sourcepos=sourcepos,
    realname=realname}
															 
fun setSourcePos (props as {dim, sourcepos, realname} : Fun.instproperties) sym = 
    {dim=dim,
    sourcepos=SOME sym,
    realname=realname}
															 
fun setRealName (props as {dim, sourcepos, realname} : Fun.instproperties) sym = 
    {dim=dim,
    sourcepos=sourcepos,
    realname=SOME sym}
															 
end
