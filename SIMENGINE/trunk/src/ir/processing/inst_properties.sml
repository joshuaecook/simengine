structure InstProps = 
struct

type iteratorname = Symbol.symbol
type instname = Symbol.symbol

(*datatype instform = FUNCTIONAL
		  | FUNCTIONAL_BY_REF
		  | STATELY of {reads: (iteratorname * instname) list,
				writes: (iteratorname * instname) list}*)
type dimlist = int list

type instproperties =
     {dim: dimlist option,
      sourcepos: PosLog.pos option,
      realclassname: Symbol.symbol option,
      realinstname: Symbol.symbol option,
      iterators: Symbol.symbol list,
      inline: bool(*,
      form: instform option*)}

(* handle instance properties *)
val emptyinstprops = {dim=NONE,
		      sourcepos=NONE,
		      realclassname=NONE,
		      realinstname=NONE,
		      iterators=nil,
		      inline=false}

fun getDim (props : instproperties) = #dim props
fun getSourcePos (props : instproperties)= #sourcepos props
fun getRealClassName (props : instproperties)= #realclassname props
fun getRealInstName (props : instproperties)= #realinstname props
fun isInline (props : instproperties)= #inline props
fun getIterators (props: instproperties) = #iterators props

fun setDim (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=SOME sym,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setSourcePos (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=SOME sym,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setRealClassName (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=SOME sym,
     realinstname=realinstname,
     iterators=iterators,
     inline=inline}
															 
fun setRealInstName (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=SOME sym,
     iterators=iterators,
     inline=inline}

fun setInline (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) sym : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=iterators,
     inline=sym}

fun setIterators (props as {dim, sourcepos, realclassname, realinstname, inline, iterators} : instproperties) newiterators : instproperties = 
    {dim=dim,
     sourcepos=sourcepos,
     realclassname=realclassname,
     realinstname=realinstname,
     iterators=newiterators,
     inline=inline}


end
