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

structure KEC =
struct

datatype typepattern = 
	 TYPE of Symbol.symbol
       | COMPOUNDTYPE of Symbol.symbol * typepattern
       | ARROW of typepattern * typepattern
       | TUPLETYPE of typepattern list
       | UNITTYPE
       | DONTCARE

datatype lit = CONSTREAL of real
             | CONSTBOOL of bool
             | CONSTSTR  of string
	     | CONSTBINARY of int * IntInf.int


type header = {name: Symbol.symbol, args: (Symbol.symbol * typepattern) list, return: typepattern option} 

datatype stream = INSTREAM of IOUtil.instream
		| OUTSTREAM of IOUtil.outstream

datatype visibility = HIDDEN | PUBLIC

datatype member = METHOD of (Symbol.symbol * visibility * runnable)
		| CONSTRUCTOR of {name: Symbol.symbol option,
				  init_methods:{args: (Symbol.symbol * typepattern) list, 
						return: typepattern, 
						stms: stm list,
						super: member,
						closure: exp Env.env} list,
				  new_object: member list}

		| VAR of {name: Symbol.symbol,
			  visibility: visibility,
			  typepattern: typepattern,
			  value: exp ref}

		| CONSTANT of (Symbol.symbol * visibility (** typepattern*) * exp)
		| PROPERTY of visibility * property
			      

and replacement = 
    OVERLOAD
  | REPLACE

and storage = REFERENCE of exp ref
	    | GETSET of (unit -> exp) * (exp -> unit)

and exp =
    LITERAL   of lit
  | SYMBOL    of Symbol.symbol 
  | LIBFUN    of Symbol.symbol * exp 
  | LAMBDA    of {args:Symbol.symbol list, body:exp, closure: env, undefined_args_allowed: bool}
  | STMS      of stm list
  | APPLY     of {func:exp, args:exp}
  | IFEXP     of {cond: exp, ift: exp, iff: exp}
  | VECTOR    of {array:exp Array.array ref, front_index: int ref, back_index: int ref, front_pad_size: int ref, back_pad_size: int ref}
  | VECTORLITERAL  of exp list
  | TUPLE     of exp list
  | UNIT      
  | UNDEFINED
  | OBJECT    of {members:(member list) ref, allPublic:bool}
  | SEND      of {message: Symbol.symbol, object: exp}
  | ERROR     of exp
  | POS       of exp * PosLog.pos
  | TYPEEXP   of typepattern
  (* added in kec *)
  | CELL      of typepattern * storage
  | MAKEREF   of typepattern * exp
  | DEREF     of exp
  | RUNNABLE  of runnable
  (* added in kec to support object creation, don't let user code create these directly *)
  | CLASSDEF  of {name: Symbol.symbol, members: (replacement * member) list, parent: exp}
  | NAMESPACEDEF of {name: Symbol.symbol, stms: (visibility * stm) list}
  | SATISFIES of {class:exp, interface: exp}
  | PROPERTYEXP of property
  | PROCESS of (TextIO.outstream, TextIO.instream, TextIO.instream) MLton.Process.t * string * string list
  | STREAM of stream * bool ref * string
 
and definition =
    DEFGLOBAL of replacement * Symbol.symbol * typepattern * exp
  | DEFLOCAL of replacement * Symbol.symbol * typepattern * exp
  | DEFCONST of replacement * Symbol.symbol * typepattern * exp

and action = 
    EXP of exp
  | IMPORT of string
  | OPEN of {obj:exp, excludes: Symbol.symbol list, include_privates: bool}
  | ASSIGN of exp * exp
(*  | COND of {cond: exp, ift: stm list, iff: stm list}
  | WHILE of {cond:exp, stms: stm list}
  | FOR of {var: string, collection: exp, stms: stm list}*) (*replace with EXP(bla(STMS(...))) *)
    
and stm = 
    DEFINITION of definition * PosLog.pos
  | ACTION of action * PosLog.pos

withtype env = exp Env.env	
and runnable = {name: Symbol.symbol,
		args: (Symbol.symbol * typepattern) list, 
		return: typepattern, 
		stms: stm list,
		closure: exp Env.env} list
(* read and write are both runnable option, but we cannot represent that in SML with type mutual references, so we paste it below*)
and property = {name: Symbol.symbol,
		expansionAllowed: bool,
		read: ({name: Symbol.symbol,
			args: (Symbol.symbol * typepattern) list, 
			return: typepattern, 
			stms: stm list,
			closure: exp Env.env} list) option,
		write: ({name: Symbol.symbol,
			 args: (Symbol.symbol * typepattern) list, 
			 return: typepattern, 
			 stms: stm list,
			 closure: exp Env.env} list) option}
 



val default_vector_size = 64			  

fun buildEmptyVector() =
    let
	(* This is parameterized to make changes to this code easier.  The compiler should optimize this all away *)
		    
	fun find_pad_size size =
	    (Int.div(size,2), Int.div(size,2), size)

	val (front_pad_size, back_pad_size, size) = 
	    find_pad_size default_vector_size

	val front_index = Int.div (size, 2)
	val back_index = Int.div (size, 2)

	val array = Array.array (size, UNDEFINED)
    in
	VECTOR {array=ref array,
		front_index=ref front_index,
		front_pad_size=ref front_pad_size,
		back_index=ref back_index,
		back_pad_size=ref back_pad_size}
    end
(*
    let
	val default_vector_size = 64

	fun find_pad_size size =
	    if length(list) > size then
		find_pad_size (size * 2)
	    else
		(Int.div(size,2), Int.div(size,2), size)

	val (front_pad_size, back_pad_size, size) = 
	    find_pad_size default_vector_size

	val front_index = Real.floor((Real.fromInt (size - (length list))) / 2.0)
	val back_index = size - (Real.ceil((Real.fromInt(size - (length list))) / 2.0))

	val array = Array.array (size, UNDEFINED)
	val values = Array.fromList(list)
	val _ = Array.copy{src=values, dst=array, di=front_index}
    in
	VECTOR {array=ref array,
		front_index=ref front_index,
		front_pad_size=ref front_pad_size,
		back_index=ref back_index,
		back_pad_size=ref back_pad_size}
    end
*)

fun buildVector nil = 
    (*LIBFUN(Symbol.symbol "deepclone", emptyvector) emptyvector*)
    buildEmptyVector()
  | buildVector list =
    let
	val newarray = Array.fromList(list)
		       
	fun max (x,y) = if x > y then x else y

	val array_size = max(Real.floor(Math.pow(2.0, Real.realCeil(Math.ln(Real.fromInt (length list) + 1.0) / (Math.ln(2.0))))),
			     default_vector_size)
			 
	val front_pad_size = Int.div (array_size, 2)
	val back_pad_size = Int.div (array_size, 2)
			    
	val finalarray = Array.array(array_size, UNDEFINED)
	val _ = Array.copy {src=newarray, dst=finalarray, di = 0}
    in
	VECTOR {array=ref finalarray, 
		front_index= ref 0,
		back_index= ref (length list),
		front_pad_size = ref front_pad_size,
		back_pad_size = ref back_pad_size}
    end

fun list2kecvector list = VECTORLITERAL list
(*
fun list2kecvector list =
    let
	fun pushVector items =
	    foldl (fn(i, v) => APPLY{func=SEND{message=Symbol.symbol "push_back", object=v},
					 args=TUPLE [i]}) 
		  (LIBFUN(Symbol.symbol "deepclone", emptyvector)) 
		  items 
    in
	(pushVector list) 
    end
*)

fun restOfVector {array, front_index, back_index, front_pad_size, back_pad_size} =
    let
	val restArray = Array.array(!front_pad_size + !back_pad_size, UNDEFINED)
	val _ = Array.copy {src= !array, dst=restArray, di = 0}
	
	val front_index' = if !back_index - !front_index > 0 then
			       !front_index + 1
			   else
			       !front_index
    in
	VECTOR {array=ref restArray,
		front_index = ref (front_index'),
		back_index = ref (!back_index),
		front_pad_size = ref (!front_pad_size),
		back_pad_size = ref (!back_pad_size)}
    end

fun kecvector2list {array, front_index, back_index, front_pad_size, back_pad_size} =
    GeneralUtil.listSlice (GeneralUtil.array2list (!array)) (!front_index, !back_index)
end
