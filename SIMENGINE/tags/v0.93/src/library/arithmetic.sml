structure ArithmeticLib =
struct

val realfun = LibraryUtil.realfun
and realsfun = LibraryUtil.realsfun

fun add _ = realsfun op+
fun subtract _ = realsfun op-
fun multiply _ = realsfun op*
fun divide _ = realsfun op/

fun negation _ = realfun op~
fun ceiling _ = realfun (Real.fromInt o Real.ceil)
fun floor _ = realfun (Real.fromInt o Real.floor)

fun sqrt _ = realfun Math.sqrt

fun ln _ = realfun Math.ln
fun log10 _ = realfun Math.log10

fun realmod (r1, r2) =
    let
	val {frac, whole} = Real.split (r1 / r2)
    in
	r2 * frac
    end
    
fun modulus _ = (realsfun realmod)

fun negation _ = (realfun op~)
fun ceiling _ = (realfun (Real.fromInt o Real.ceil))
fun floor _ = (realfun (Real.fromInt o Real.floor))
fun exp _ = realfun Math.exp

fun pi_value _ args = KEC.LITERAL (KEC.CONSTREAL Math.pi)
fun e_value _ args = KEC.LITERAL (KEC.CONSTREAL Math.e)

val library = [{name="add", operation=add},
	       {name="subtract", operation=subtract},
	       {name="multiply", operation=multiply},
	       {name="divide", operation=divide},
	       {name="modulus", operation=modulus},
	       {name="neg", operation=negation},
	       {name="ceiling", operation=ceiling},
	       {name="floor", operation=floor},
	       {name="sqrt", operation=sqrt},
	       {name="ln", operation=ln},
	       {name="log10", operation=log10},
	       {name="exp", operation=exp},
	       {name="pi_value", operation=pi_value},
	       {name="e_value", operation=e_value}]

end
