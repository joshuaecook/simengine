structure HLEC = 
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


type header = {name: Symbol.symbol, args: (Symbol.symbol * typepattern option) list, return: typepattern option} 

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
       | ERROR    of exp
       | POS of exp * PosLog.pos
       (* rich features *)
       | AND       of exp list
       | OR        of exp list
       | LET       of {vals: (Symbol.symbol list * exp) list, body: exp}
       | LETREC    of {vals: (Symbol.symbol list * exp) list, body: exp}
       (* translation only features, do not generate user code to use these *)
       | STMS      of stm list
       | TYPEEXP   of typepattern
       | FORGENERATOR of {var: Symbol.symbol, collection: exp, condition: exp option} list * exp
       | FORALL    of {var:Symbol.symbol, collection: exp, test: exp}
       | EXISTS    of {var:Symbol.symbol, collection: exp, test: exp}
       | TABLE     of (Symbol.symbol * exp) list

and runnable_modifier = 
    OVERLOAD
  | REPLACE

(*withtype method = {name:string, exp: exp} (* implicitly takes self as first arg *)*)
and method = 
    METHODDEF of visibility * definition
  | CONSTRUCTOR of {args: (Symbol.symbol * typepattern option) list, body: stm list}


and interfaceheader =
    FUNHEADER of header
  | CONSHEADER of (Symbol.symbol * typepattern option) list

and definition =
	 DEFFUN of runnable_modifier * (header * stm list) list
       | DEFPROTOCOL of runnable_modifier * header * stm list
       | DEFCLASS of {name: Symbol.symbol,
		      classheader: {inheritance: exp option, interfaces: Symbol.symbol list},
		      methods: method list}
       | DEFNAMESPACE of {name: Symbol.symbol,
			  stms: (visibility * stm) list}
       | DEFINTERFACE of {name: Symbol.symbol,
			  headers: interfaceheader list}
       | DEFGLOBAL of Symbol.symbol * typepattern * exp
       | DEFLOCAL of Symbol.symbol * typepattern * exp
       | DEFCONST of Symbol.symbol * typepattern * exp
       | DEFPROPERTY of {name: Symbol.symbol,
			 read: stm list option,
			 write: (Symbol.symbol * (stm list)) option}

and action = 
    EXP of exp
  | IMPORT of string
  | OPEN of exp
  | ASSIGN of exp * exp
  | COND of {cond: exp, ift: stm list, iff: stm list}
  | WHILE of {cond:exp, stms: stm list}
  | FOR of {var: Symbol.symbol, collection: exp, stms: stm list}
    
and stm = 
    DEFINITION of definition * PosLog.pos
  | ACTION of action * PosLog.pos
	      

end
