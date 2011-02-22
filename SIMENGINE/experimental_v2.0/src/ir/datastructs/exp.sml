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
       | CONVERSION of conversion

     and conversion =
	 SUBREF of (exp * subspace)
       | RESHAPE of (exp * Space.space)
       | SUBSPACE of subspace
		      
     and meta =
	 LAMBDA of {args: Symbol.symbol list, body:exp}
       | APPLY of {func:exp, args: exp list}
       | MAP of {func:exp, args: exp}
       | SEQUENCE of exp list
	 
     and container =
	 MATRIX of exp Matrix.matrix
       | ARRAY of exp Array.array
       | ASSOC of exp SymbolTable.table
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
       | RANDOM of (randomtype * Space.space)
       (* Symbols are associated with a variety of metadata.
	* See props.sml. *)
       | SYMBOL of (Symbol.symbol * Property.symbolproperty)
       | DONTCARE
       | INFINITY
       | NAN
       | PATTERN of (Symbol.symbol * predicate * patterncount)
       | STRING of string
       | FILEREF of FileEntry.fileentry * Space.space

     and randomtype = 
	 UNIFORM
       | NORMAL


    (* An interval defines how to select across one dimension of data.  The top four types, Empty, Full,
     * Interval, and Indices can be used to iterate across one dimension of a Tensor or other geometric 
     * space.  The IntervalCollection defines how to iterator across a collection of spaces. *)
     and interval =
	 (* An Empty interval has no values included *)
	 Empty
       (* A Full interval includes every value - it's the same as an interval going from 0 to n-1, step 1. *)
       | Full
       (* An Interval is a range from start to stop by step.  It is valid if step < 0 but not if
	* step=0.  The usage follows the Matlab convention of start:step:stop, except that everything 
	* is zero indexed. *)
       | Interval of {start: int, stop: int, step: int} (* this is all zero indexed *)
       (* Indices are a lit of one or more values in any arbitray order.  Singluar indices are encoded as 
	* a one element list. *)
       | Indices of int list
       (* When you are subreferencing into a collection of multiple spaces (see space.sml), 
	* you need an IntervalCollection to index into it.  The first interval in the tuple 
	* refers to which elements of the collection you want to reference.  The second tupe
	* value, the subspace list, has the same length as the number of elements chosen out 
	* of the space collection.  Those intervals are then applied recursively to each space
	* in the collection.*)
       | IntervalCollection of (interval * interval list list)
       (* This is an inteval like x[i=_] where you are creating a generator function around 
	* x using the variable i as an index for each element. *)
       | NamedInterval of (Symbol.symbol * interval)
       (* This is an interval based on a calculated expression type, for example x[2*i+1] for odd indices *)
       | ExpInterval of exp
			      
     and patterncount =
	 ONE
       | ONE_OR_MORE
       | ZERO_OR_MORE
       | SPECIFIC_COUNT of int
       | SPECIFIC_RANGE of (int * int)	 

withtype predicate = (string * (exp -> bool))
     and subspace  = interval list

type pattern = (Symbol.symbol * predicate * patterncount)

val null = FUN (Fun.BUILTIN Fun.NULL, [])
val toString : (exp -> string) ref = ref (fn(exp)=>"??")
val toLayout : (exp -> Layout.t) ref = ref (fn(exp)=> Layout.str "??")
val toJSON : (exp -> JSON.json) ref = ref (fn (exp) => JSON.null)

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
     toString= !toString,
     toJSON= !toJSON}


end
