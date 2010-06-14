structure Exp =
struct
(* Expressions are rooted trees representing a computable object.
 * Nb. There exist expressions which are not valid for our simulations. 
 * Expressions are commonly constructed by functions in the ExpBuild structure.
 * The ExpProcess structure contains functions for deconstructing and
 * inspecting expressions.
 * See src/ir/processing/exp_*.sml. *)
datatype exp = 
	 (* A function applied to an ordered list of argument expressions.
	  * Many functions support variadic arguments.
	  * See src/ir/datastructs/fun.sml. *)
	 FUN of (Fun.funtype * exp list)
       | TERM of term
       | META of meta	 
       | CONTAINER of container
		      
     and meta =
	 LAMBDA of {arg:Symbol.symbol, body:exp}
       | APPLY of {func:exp, arg:exp}
       | MAP of {func:exp, args: exp}
       | SEQUENCE of exp list
	 
     and container =
	 MATRIX of exp Matrix.matrix
       | ARRAY of exp Array.array
       | EXPLIST of exp list

     and term = 
	 RATIONAL of (int * int)
       | INT of int
       (* Infinity and NaN are represented separately 
	* and should not be allowed in the REAL constructor. *)
       | REAL of real
       | BOOL of bool
       | COMPLEX of (term * term)
       | TUPLE of (term list)
       | RANGE of {low: term, high: term, step: term}
       | RANDOM of randomtype
       (* Symbols are associated with a variety of metadata.
	* See props.sml. *)
       | SYMBOL of (Symbol.symbol * Property.symbolproperty)
       | DONTCARE
       | INFINITY
       | NAN
       | PATTERN of (Symbol.symbol * predicate * Pattern.patterncount)

     and randomtype = 
	 UNIFORM
       | NORMAL

withtype predicate = (string * (exp -> bool))

type pattern = (Symbol.symbol * predicate * Pattern.patterncount)

val null = FUN (Fun.BUILTIN Fun.NULL, [])
val exp2str : (exp -> string) ref = ref (fn(exp)=>"??")
val exp2JSON : (exp -> JSON.json) ref = ref (fn (exp) => JSON.null)

fun calculus () : exp Calculus.calculus = 
    {zero= TERM (INT 0),
     isZero= (fn(a)=> case a of
			  TERM (INT 0) => true
			| TERM (REAL r) => Real.== (0.0, r)
			| _ => false),
     one= TERM (INT 1),
     isOne= (fn(a)=> case a of
			 TERM (INT 1) => true
		       | TERM (REAL r) => Real.== (1.0, r)
		       | _ => false),
     addition= (fn(l)=>FUN (Fun.BUILTIN Fun.ADD, l)),
     multiplication= (fn(l)=>FUN (Fun.BUILTIN Fun.MUL, l)),
     toString= !exp2str,
     toJSON= !exp2JSON}


end
