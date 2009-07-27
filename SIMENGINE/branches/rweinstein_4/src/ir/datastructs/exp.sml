structure Exp =
struct

datatype exp = FUN of (Symbol.symbol * exp list)
	     | TERM of term
     and term = RATIONAL of (int * int)
	       | INT of int
	       | REAL of real (* be careful to not allow inf and nan *)
	       | BOOL of bool
	       | COMPLEX of (term * term)
	       | LIST of (term list * Property.dimlist)
	       | TUPLE of (term list)
	       | SYMBOL of (Symbol.symbol * Property.symbolproperty)
	       | DONTCARE
	       | INFINITY
	       | NAN
	       | PATTERN of (Symbol.symbol * predicate * Pattern.patterncount)

withtype predicate = (string * (exp -> bool))



end
