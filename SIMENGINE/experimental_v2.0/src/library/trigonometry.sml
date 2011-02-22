structure TrigonometryLib =
struct

val realfun = LibraryUtil.realfun
and realsfun = LibraryUtil.realsfun

fun sin _ = realfun Math.sin
fun cos _ = realfun Math.cos
fun tan _ = realfun Math.tan

fun asin _ = realfun Math.asin
fun acos _ = realfun Math.acos
fun atan _ = realfun Math.atan
fun atan2 _ = realsfun Math.atan2

fun sinh _ = realfun Math.sinh
fun cosh _ = realfun Math.cosh
fun tanh _ = realfun Math.tanh

val library = [{name="sin", operation=sin},
	       {name="cos", operation=cos},
	       {name="tan", operation=tan},
	       {name="asin", operation=asin},
	       {name="acos", operation=acos},
	       {name="atan", operation=atan},
	       {name="atan2", operation=atan2},
	       {name="sinh", operation=sinh},
	       {name="cosh", operation=cosh},
	       {name="tanh", operation=tanh}]

end
