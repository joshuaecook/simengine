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
	 MATRIX of exp Array2.array
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
       | LIST of (term list * Property.dimlist)
       | TUPLE of (term list)
       | RANGE of {low: term, high: term, step: term}
       | RANDOM
       (* Symbols are associated with a variety of metadata.
	* See props.sml. *)
       | SYMBOL of (Symbol.symbol * Property.symbolproperty)
       | DONTCARE
       | INFINITY
       | NAN
       | PATTERN of (Symbol.symbol * predicate * Pattern.patterncount)

withtype predicate = (string * (exp -> bool))
type pattern = (Symbol.symbol * predicate * Pattern.patterncount)

val null = FUN (Fun.BUILTIN Fun.NULL, [])


end
