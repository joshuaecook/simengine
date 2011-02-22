structure InstProps = 
struct


type instproperties =
     {sourcepos: PosLog.pos option,
      realclassname: Symbol.symbol option,
      iterators: Symbol.symbol list,
      inline: bool, 
      space: Space.space}

(* handle instance properties *)
val emptyinstprops = {sourcepos=NONE,
		      realclassname=NONE,
		      iterators=nil,
		      inline=false,
		      space=Space.emptyCollection}

fun getSourcePos (props : instproperties)= #sourcepos props
fun getRealClassName (props : instproperties)= #realclassname props
fun isInline (props : instproperties)= #inline props
fun getIterators (props: instproperties) = #iterators props
fun getSpace (props: instproperties) = #space props

fun setSourcePos (props as {sourcepos, realclassname, inline, iterators, space} : instproperties) sym : instproperties = 
    {sourcepos=SOME sym,
     realclassname=realclassname,
     iterators=iterators,
     inline=inline,
     space=space}
															 
fun setRealClassName (props as {sourcepos, realclassname, inline, iterators, space} : instproperties) sym : instproperties = 
    {sourcepos=sourcepos,
     realclassname=SOME sym,
     iterators=iterators,
     inline=inline,
     space=space}
															 
fun setInline (props as {sourcepos, realclassname, inline, iterators, space} : instproperties) sym : instproperties = 
    {sourcepos=sourcepos,
     realclassname=realclassname,
     iterators=iterators,
     inline=sym,
     space=space}

fun setIterators (props as {sourcepos, realclassname, inline, iterators, space} : instproperties) newiterators : instproperties = 
    {sourcepos=sourcepos,
     realclassname=realclassname,
     iterators=newiterators,
     inline=inline,
     space=space}

fun setSpace (props as {sourcepos, realclassname, inline, iterators, space} : instproperties) newspace : instproperties = 
    {sourcepos=sourcepos,
     realclassname=realclassname,
     iterators=iterators,
     inline=inline,
     space=newspace}


end
