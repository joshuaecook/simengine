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
 

		    
fun list2kecvector list =
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

fun kecvector2list {array, front_index, back_index, front_pad_size, back_pad_size} =
    GeneralUtil.listSlice (GeneralUtil.array2list (!array)) (!front_index, !back_index)
end
