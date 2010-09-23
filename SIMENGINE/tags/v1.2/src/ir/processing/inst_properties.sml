structure InstProps = 
struct


type instproperties =
     {sourcepos: PosLog.pos option,
      realclassname: Symbol.symbol option,
      iterators: Symbol.symbol list,
      inline: bool}

(* handle instance properties *)
val emptyinstprops = {sourcepos=NONE,
		      realclassname=NONE,
		      iterators=nil,
		      inline=false}

fun getSourcePos (props : instproperties)= #sourcepos props
fun getRealClassName (props : instproperties)= #realclassname props
fun isInline (props : instproperties)= #inline props
fun getIterators (props: instproperties) = #iterators props

fun setSourcePos (props as {sourcepos, realclassname, inline, iterators} : instproperties) sym : instproperties = 
    {sourcepos=SOME sym,
     realclassname=realclassname,
     iterators=iterators,
     inline=inline}
															 
fun setRealClassName (props as {sourcepos, realclassname, inline, iterators} : instproperties) sym : instproperties = 
    {sourcepos=sourcepos,
     realclassname=SOME sym,
     iterators=iterators,
     inline=inline}
															 
fun setInline (props as {sourcepos, realclassname, inline, iterators} : instproperties) sym : instproperties = 
    {sourcepos=sourcepos,
     realclassname=realclassname,
     iterators=iterators,
     inline=sym}

fun setIterators (props as {sourcepos, realclassname, inline, iterators} : instproperties) newiterators : instproperties = 
    {sourcepos=sourcepos,
     realclassname=realclassname,
     iterators=newiterators,
     inline=inline}


end
