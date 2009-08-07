structure PatternProcess =
struct

val i2s = Util.i2s

type predicate = (string * (Exp.exp -> bool))

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

fun patcount_compatible patcount count =
    case patcount 
     of Pattern.ONE => count = 1
      | Pattern.ONE_OR_MORE => count >= 1
      | Pattern.ZERO_OR_MORE => count >= 0
      | Pattern.SPECIFIC_COUNT i => i=count
      | Pattern.SPECIFIC_RANGE (i1, i2) => i1 <= count andalso count <= i2

fun min_patcount patcount =
    case patcount 
     of Pattern.ONE => 1
      | Pattern.ONE_OR_MORE => 1
      | Pattern.ZERO_OR_MORE => 0
      | Pattern.SPECIFIC_COUNT i => i
      | Pattern.SPECIFIC_RANGE (i1, i2) => i1

fun max_patcount patcount =
    case patcount 
     of Pattern.ONE => SOME 1
      | Pattern.ONE_OR_MORE => NONE
      | Pattern.ZERO_OR_MORE => NONE
      | Pattern.SPECIFIC_COUNT i => SOME i
      | Pattern.SPECIFIC_RANGE (i1, i2) => SOME i2

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

fun notpred ((id, pred):predicate):predicate = 
    ("!" ^ id, (fn(x) => not (pred x)))
    

fun gen_predicate_from_symbol sym = 
    ("ASYM", 
     fn(x)=>case x 
	     of Exp.TERM (Exp.SYMBOL (name, props)) => name = sym
	      | _ => false)

fun combine_preds nil = ("NIL", fn(x)=>true)
  | combine_preds [pred1] = pred1
  | combine_preds ((pred as (id,p))::rest) = 
    let
	val (id', combined_preds) = combine_preds rest
    in
	(id ^ ":" ^ id',
	 (fn(x) => ((p x) andalso (combined_preds x))))
    end

end
