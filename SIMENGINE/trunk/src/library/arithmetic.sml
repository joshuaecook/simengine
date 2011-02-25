(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

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
