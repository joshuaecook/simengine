structure Ast = 
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

(* An argument or return declaration consiting of an identifier and an optional type specification. *) 
type typedname = Symbol.symbol * typepattern option 

(* A function header comprising its name, a list of argument declarations, 
 * and an optional list of return declarations. *) 
type header = {name: Symbol.symbol, 
 	       args: typedname list,  
 	       returns: typedname list option (* NONE indicates that no return values were specified, the function will return the value of its last expression. *) 
 	      }  

type modelheader = {name: Symbol.symbol, 
 	       args: (Symbol.symbol * Symbol.symbol list option) list,  
 	       returns: Symbol.symbol list option (* NONE indicates that no return values were specified, the function will return the value of its last expression. *) 
 	      }  

datatype visibility = HIDDEN | PUBLIC

datatype exp =
	 (* basic features *)
	 LITERAL   of lit
       | STRINGEXP of exp list
       | SYMBOL    of Symbol.symbol 
       | LIBFUN    of Symbol.symbol * exp 
       | LAMBDA    of {args:Symbol.symbol list, body:exp}
       | APPLY     of {func:exp, args:exp}
       | IFEXP     of {cond: exp, ift: exp, iff: exp}
       | VECTOR    of exp list
       | TUPLE     of exp list
       | ASSERTION of exp
       | UNIT   
       | UNDEFINED
(*       | OBJECT    of method list*)
       | SEND      of {message: Symbol.symbol, object: exp}
       | SATISFIES of {class: exp, interface: exp}
       | ERROR     of exp
       | POS       of exp * PosLog.pos
       | TYPEEXP   of typepattern
       (* rich features *)
       | AND       of exp list
       | OR        of exp list
       | FORGENERATOR of {var: Symbol.symbol, collection: exp, condition: exp option} list * exp
       | FORALL    of {var:Symbol.symbol, collection: exp, test: exp}
       | EXISTS    of {var:Symbol.symbol, collection: exp, test: exp}
       | TABLE     of (Symbol.symbol * exp) list
       | LET       of (Symbol.symbol * exp * exp)
       | NAMEDPATTERN of Symbol.symbol * exp
       | WILDCARD
       | RULEMATCH of {find:exp, conds: exp, replace: exp}

(*withtype method = {name:string, exp: exp} (* implicitly takes self as first arg *)*)
and method = 
    METHODDEF of visibility option * definition
  | CONSTRUCTOR of {args: (Symbol.symbol * typepattern option) list, body: stm list}

and runnable_modifier = 
    OVERLOAD
  | REPLACE

and interfaceheader =
    FUNHEADER of header
  | CONSHEADER of (Symbol.symbol * typepattern option) list

and definition =
	 DEFFUN of runnable_modifier option * (header * stm list) list
       | DEFPROTOCOL of runnable_modifier option * header * stm list
       | DEFCLASS of {name: Symbol.symbol,
		      classheader: {inheritance: exp option, interfaces: Symbol.symbol list},
		      methods: method list}
       | DEFNAMESPACE of {name: Symbol.symbol,
			  stms: (visibility option * stm) list}
       | DEFINTERFACE of {name: Symbol.symbol,
			  headers: interfaceheader list}
       | DEFGLOBAL of Symbol.symbol * typepattern option * exp option
       | DEFLOCAL of Symbol.symbol * typepattern option * exp option
       | DEFENUM of {name: Symbol.symbol, parent: Symbol.symbol option, args: (Symbol.symbol * int option) list}
       | DEFCONST of Symbol.symbol * typepattern option * exp
       | DEFMODEL of {header: modelheader, 
		      parts: modelpart list}
       | INSTMODEL of {name: Symbol.symbol,
		       exp: exp}
       | DEFPROPERTY of {name: Symbol.symbol,
			 io:{read: stm list option,
			     write: (Symbol.symbol * (stm list)) option}}

and quantitytype = 
    GENERIC_QUANTITY
  | STATE_QUANTITY
  | RANDOM_QUANTITY
  | PARAMETER_QUANTITY

and modelpart =
    STM of stm
  | QUANTITYDEF of {modifiers: simq_modifier list, basetype: quantitytype, name: Symbol.symbol, precision: exp option, exp: exp option, settingstable: exp option, dimensions: Symbol.symbol list option}
  | OUTPUTDEF of {name: Symbol.symbol, quantity: exp, dimensions: Symbol.symbol list option, settings: exp option, condition: exp option}
  | INPUTDEF of {name: Symbol.symbol, settings: exp option}
  | ITERATORDEF of {name: Symbol.symbol, value: exp option, settings: exp option}
  | SUBMODELDEF of definition (* assumed to be DEFMODEL *)
  | SUBMODELINST of {class: Symbol.symbol, name: Symbol.symbol, opttable: exp option, optdimensions: Symbol.symbol list option}
(*    STATEDEF of {modifiers: simq_modifier list,
		 name: Symbol.symbol,
		 precision: exp option,
		 exp: exp}
  | PARAMDEF of {modifiers: simq_modifier list,
		 name: Symbol.symbol,
		 precision: exp option,
		 exp: exp}
  | EQUATIONSDEF of equation list
(*  | RUNNABLE of *)
  | SUBMODELDEF of definition (* assumed to be DEFMODEL *)
  | SUBMODELINST of Symbol.symbol * exp
  | TEMPORARY of Symbol.symbol * exp*)
(*  | MODELSTM of modelstm

and modelstm =
    MODELASSIGN of exp * exp
  | MODELEXP of exp
  | MODELCOND of {cond:exp, ift: modelpart list, iff: modelpart list}
  | MODELWHILE of {cond: exp, stms: modelpart list}
  | MODELFOR of {var: Symbol.symbol, collection: exp, stms: modelpart list}
*)
and simq_modifier =
    VISIBLE
  | TUNABLE
  | STATEFUL

and action = 
    EXP of exp
  | IMPORT of string
  | OPEN of exp
  | ASSIGN of exp * exp
  | COND of {cond: exp, ift: stm list, iff: stm list}
  | WHILE of {cond:exp, stms: stm list}
  | FOR of {var: Symbol.symbol, collection: exp, stms: stm list}
  | EQUATIONS of equation list
    
and stm = 
    DEFINITION of definition * PosLog.pos
  | ACTION of action * PosLog.pos

(* model/simulation related data structures *)
and equation =
    EQUATION of exp * exp * exp option
  | MATHFUNCTION of exp * exp
  | EVENT of Symbol.symbol * exp
	      

end
