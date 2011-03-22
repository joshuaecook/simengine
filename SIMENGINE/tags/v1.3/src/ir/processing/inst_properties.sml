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
