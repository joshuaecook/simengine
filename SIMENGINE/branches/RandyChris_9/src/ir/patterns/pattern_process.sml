structure PatternProcess =
struct

val i2s = Util.i2s

(*type predicate = (string * (exp -> bool))*)

(*
datatype patterntype = ANY
		     | ANY_FUN
		     | ANY_TERM
		     | ANY_NUMERIC
		     | ANY_SYMBOL
		     | ANY_INT
		     | ANY_REAL
		     | ANY_COMPLEX
		     | ANY_RATIONAL
		     | ANY_NAN
		     | ANY_INF
*)

				     

fun pattern2str ((symbol, (predicate_name, predicate_fun), patcount):
		 (Symbol.symbol * Exp.predicate * Pattern.patterncount)) =
    let
	val name = Symbol.name symbol
	val typestr = "{"^predicate_name^"}"

	val countstr = case patcount 
			of Pattern.ONE => "_"
			 | Pattern.ONE_OR_MORE => "__"
			 | Pattern.ZERO_OR_MORE  => "___"
			 | Pattern.SPECIFIC_COUNT i => "_{"^(i2s i)^"}"
			 | Pattern.SPECIFIC_RANGE (i1, i2) => "_{"^(i2s i1)^":"^(i2s i2)^"}"
    in
	name ^ typestr ^ countstr
    end

(* Define common predicates *)
val predicate_any = ("ANY", fn(x)=>true)
val predicate_anyfun = ("FUN", fn(x)=>case x of 
					  Exp.FUN _ => true 
					| _ => false)
val predicate_anyterm = ("TERM", fn(x)=>case x of 
					    Exp.TERM _ => true 
					  | _ => false)
val predicate_anynumeric = ("NUM", fn(x)=>case x of 
					      Exp.TERM t => Term.isNumeric t
					    | _ => false)
val predicate_anysymbol = ("NUM", fn(x)=>case x of 
					     Exp.TERM t => Term.isSymbol t
					   | _ => false)
val predicate_anydiffterm = ("DIFFTERM", 
			  fn(x)=>case x 
				  of Exp.TERM (Exp.SYMBOL (name, props)) => 
				     (case Property.getDerivative props 
				       of SOME (order, _) => order >= 1 
					| NONE => false)
				   | _ => false)



end
