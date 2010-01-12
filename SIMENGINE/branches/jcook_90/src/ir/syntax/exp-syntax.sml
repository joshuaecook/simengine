structure ExpSyntax: sig

val toJSON: Exp.exp -> JSON.json
val termToJSON: Exp.term -> JSON.json
(* val fromJSON: JSON.json -> Exp.exp *)

end = struct
open JSON
val int = int o IntInf.fromInt


fun toJSON (Exp.FUN (operator, operands)) = 
    JSONTypedObject ("Exp.FUN", 
		     object [("operator", FunSyntax.toJSON operator),
			     ("operands", array (map toJSON operands))])
  | toJSON (Exp.TERM term) = 
    JSONTypedObject ("Exp.TERM", termToJSON term)
  | toJSON (Exp.META meta) = 
    JSONTypedObject ("Exp.META", metaToJSON meta)
  | toJSON (Exp.CONTAINER container) = 
    JSONTypedObject ("Exp.CONTAINER", containerToJSON container)

and termToJSON (Exp.INT z) = JSONTypedObject ("Exp.INT", int z)
  | termToJSON (Exp.REAL r) = JSONTypedObject ("Exp.REAL", real r)
  | termToJSON (Exp.BOOL b) = JSONTypedObject ("Exp.BOOL", bool b)
  | termToJSON (Exp.DONTCARE) = JSONType ("Exp.DONTCARE")
  | termToJSON (Exp.INFINITY) = JSONType ("Exp.INFINITY")
  | termToJSON (Exp.NAN) = JSONType ("Exp.NAN")
  | termToJSON (Exp.RATIONAL (num, denom)) =
    JSONTypedObject ("Exp.RATIONAL", 
		     object [("numerator", int num), ("denominator", int num)])
  | termToJSON (Exp.RANDOM typ) =
    JSONTypedObject ("Exp.RANDOM",
		     case typ 
		      of Exp.UNIFORM => JSONType "Exp.RANDOM"
		       | Exp.NORMAL => JSONType "Exp.NORMAL")
  | termToJSON (Exp.COMPLEX (r, i)) =
    JSONTypedObject ("Exp.COMPLEX",
		     object [("real", termToJSON r), ("imaginary", termToJSON i)])
  | termToJSON (Exp.TUPLE terms) =
    JSONTypedObject ("Exp.TUPLE", array (map termToJSON terms))
  | termToJSON (Exp.RANGE {low, high, step}) =
    JSONTypedObject ("Exp.RANGE",
		     object [("low", termToJSON low),
			     ("step", termToJSON step),
			     ("high", termToJSON high)])
  | termToJSON (Exp.PATTERN (name, predicate, arity)) =
    let val js_arity = 
	    case arity
	     of Pattern.ONE => JSONType ("Pattern.ONE")
	      | Pattern.ONE_OR_MORE => JSONType ("Pattern.ONE_OR_MORE")
	      | Pattern.ZERO_OR_MORE => JSONType ("Pattern.ZERO_OR_MORE")
	      | Pattern.SPECIFIC_COUNT z => 
		JSONTypedObject ("Pattern.SPECIFIC_COUNT", int z)
	      | Pattern.SPECIFIC_RANGE (low, high) =>
		JSONTypedObject ("Pattern.SPECIFIC_COUNT", 
				 object [("low", int low), ("high", int high)])
    in
	JSONTypedObject ("Exp.PATTERN",
			 object [("predicate", string "FIXME"),
				 ("arity", js_arity)])
    end
  | termToJSON (Exp.SYMBOL (name, properties)) =
    JSONTypedObject ("Exp.SYMBOL",
		     object [(* ("name", JSONSymbol name), *)
			     ("properties", string "FIXME")])

and metaToJSON (Exp.LAMBDA {arg, body}) =
    JSONTypedObject ("Exp.LAMBDA",
		     object [(* ("argument", JSONSymbol arg), *)
			     ("body", toJSON body)])
  | metaToJSON (Exp.APPLY {func, arg}) =
    JSONTypedObject ("Exp.APPLY",
		     object [("function", toJSON func),
			     ("arg", toJSON arg)])
  | metaToJSON (Exp.MAP {func, args}) =
    JSONTypedObject ("Exp.MAP",
		     object [("function", toJSON func),
			     ("args", toJSON args)])
  | metaToJSON (Exp.SEQUENCE members) =
    JSONTypedObject ("Exp.SEQUENCE", array (map toJSON members))

and containerToJSON (Exp.EXPLIST exps) =
    JSONTypedObject ("Exp.EXPLIST", array (map toJSON exps))
  | containerToJSON (Exp.ARRAY arr) = 
    JSONTypedObject ("Exp.ARRAY", array (map toJSON (Container.arrayToList arr)))
  | containerToJSON (Exp.MATRIX mat) = 
    JSONTypedObject ("Exp.MATRIX", string "FIXME")

end
