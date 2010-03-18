(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure ExpSyntax: sig
(* Serialization and deserialization for DOF expression data. *)

val toJSON: Exp.exp -> JSON.json
val termToJSON: Exp.term -> JSON.json
(* TODO implemenent fromJSON *)
(* val fromJSON: JSON.json -> Exp.exp *)

end = struct

open JSON
open JSONExtensions

val int = int o IntInf.fromInt
fun symbol s = object [("$symbol", string (Symbol.name s))]


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
		     object [("name", symbol name),
			     ("properties", symbolPropertiesToJSON properties)])

and symbolPropertiesToJSON {iterator, derivative, isevent, isrewritesymbol, sourcepos, realname, scope, outputbuffer, ep_index} =
    object [("derivative", JSONOption (derivativeToJSON, derivative)),
	    ("epIndex", JSONOption (JSONType o (fn Property.STRUCT_OF_ARRAYS => "Property.STRUCT_OF_ARRAYS" | Property.ARRAY => "Property.ARRAY"), ep_index)),
	    ("isEvent", bool isevent),
	    ("isRewriteSymbol", bool isrewritesymbol),
	    ("iterators", JSONOption (fn its => array (map iteratorToJSON its), iterator)),
	    ("outputBuffer", bool outputbuffer),
	    ("scope", scopeToJSON scope),
	    ("sourcePosition", JSONOption (PosLog.toJSON, sourcepos))]

and derivativeToJSON (degree, iterators) =
    object [("degree", int degree),
	    ("iterators", array (map symbol iterators))]

and iteratorToJSON (name, index) =
    object [("name", symbol name), ("index", iteratorIndexToJSON index)]

and iteratorIndexToJSON (Iterator.RELATIVE z) =
    JSONTypedObject ("Iterator.RELATIVE", int z)
  | iteratorIndexToJSON (Iterator.ABSOLUTE z) = 
    JSONTypedObject ("Iterator.ABSOLUTE", int z)
  | iteratorIndexToJSON (Iterator.RANGE (low, high)) = 
    JSONTypedObject ("Iterator.RANGE", object [("low", int low), ("high", int high)])
  | iteratorIndexToJSON (Iterator.LIST zz) = 
    JSONTypedObject ("Iterator.LIST", array (map int zz))
  | iteratorIndexToJSON Iterator.ALL = 
    JSONType ("Iterator.ALL")

and scopeToJSON Property.LOCAL =
    JSONType ("Property.LOCAL")
  | scopeToJSON Property.ITERATOR =
    JSONType ("Property.ITERATOR")
  | scopeToJSON Property.SYSTEMITERATOR =
    JSONType ("Property.SYSTEMITERATOR")
  | scopeToJSON (Property.READSTATE name) = 
    JSONTypedObject ("Iterator.READSTATE", symbol name)
  | scopeToJSON (Property.READSYSTEMSTATE name) = 
    JSONTypedObject ("Iterator.READSYSTEMSTATE", symbol name)
  | scopeToJSON (Property.WRITESTATE name) = 
    JSONTypedObject ("Iterator.WRITESTATE", symbol name)

and metaToJSON (Exp.LAMBDA {arg, body}) =
    JSONTypedObject ("Exp.LAMBDA",
		     object [("argument", symbol arg),
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
    JSONTypedObject ("Exp.MATRIX", MatrixSyntax.toJSON mat)



val _ = Exp.exp2JSON := toJSON


end
