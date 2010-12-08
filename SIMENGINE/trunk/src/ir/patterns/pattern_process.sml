signature PATTERNPROCESS =
sig
    
    (* Predicate type - includes a string name and a function that takes an expression and resolves to a boolean to indicate that it matched *)
    type predicate = (string * (Exp.exp -> bool))

    (* useful functions *)
    val pattern2str : Exp.pattern -> string
    val patcount_compatible : Exp.patterncount -> int -> bool
    val min_patcount : Exp.patterncount -> int
    val max_patcount : Exp.patterncount -> int option
    val combine_preds : predicate list -> predicate (* reduction predicate list *)
    val notpred : predicate -> predicate (* invert a predicate *)

    (* common predicates *)
    val predicate_any : predicate
    val predicate_anyfun : predicate
    val predicate_anybuiltin : predicate
    val predicate_anyterm : predicate
    val predicate_anynumeric : predicate
    val predicate_anyconstant : predicate
    val predicate_anysymbol : predicate
    val predicate_anylocal : predicate
    val predicate_anyscalar : predicate
    val predicate_anynonscalar : predicate
    val predicate_anyzeroscalar : predicate
    val predicate_anycomplex : predicate
    val predicate_anydiffterm : predicate
    val gen_predicate_from_symbol : Symbol.symbol -> predicate (* create a predicate that matches a symbol by name *)
    val predicate_anysymbol_with_iter : Symbol.symbol -> predicate
    val predicate_instance_with_classname : Symbol.symbol -> predicate
    val predicate_instance_with_classname_and_output : (Symbol.symbol * Symbol.symbol) -> predicate

end
structure PatternProcess : PATTERNPROCESS =
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
		 (Symbol.symbol * Exp.predicate * Exp.patterncount)) =
    let
	val name = Symbol.name symbol
	val typestr = "{"^predicate_name^"}"

	val countstr = case patcount 
			of Exp.ONE => "_"
			 | Exp.ONE_OR_MORE => "__"
			 | Exp.ZERO_OR_MORE  => "___"
			 | Exp.SPECIFIC_COUNT i => "_{"^(i2s i)^"}"
			 | Exp.SPECIFIC_RANGE (i1, i2) => "_{"^(i2s i1)^":"^(i2s i2)^"}"
    in
	name ^ typestr ^ countstr
    end

fun patcount_compatible patcount count =
    let
	val _ = print ("patcount_compatible (" ^ (Int.toString count) ^ ") = ")
val res =
    case patcount 
     of Exp.ONE => count = 1
      | Exp.ONE_OR_MORE => count >= 1
      | Exp.ZERO_OR_MORE => count >= 0
      | Exp.SPECIFIC_COUNT i => i=count
      | Exp.SPECIFIC_RANGE (i1, i2) => i1 <= count andalso count <= i2
val _ = print ((Bool.toString res) ^ "\n")
    in
    res						       
end

fun min_patcount patcount =
    case patcount 
     of Exp.ONE => 1
      | Exp.ONE_OR_MORE => 1
      | Exp.ZERO_OR_MORE => 0
      | Exp.SPECIFIC_COUNT i => i
      | Exp.SPECIFIC_RANGE (i1, i2) => i1

fun max_patcount patcount =
    case patcount 
     of Exp.ONE => SOME 1
      | Exp.ONE_OR_MORE => NONE
      | Exp.ZERO_OR_MORE => NONE
      | Exp.SPECIFIC_COUNT i => SOME i
      | Exp.SPECIFIC_RANGE (i1, i2) => SOME i2

(* Define common predicates *)
val predicate_any = ("ANY", fn(x)=>true)
val predicate_anyfun = ("FUN", fn(x)=>case x of 
					  Exp.FUN _ => true 
					| _ => false)
val predicate_anybuiltin = ("FUN", fn(x)=>case x of 
					      Exp.FUN (Fun.BUILTIN _, _) => true 
					    | _ => false)
val predicate_anyterm = ("TERM", fn(x)=>case x of 
					    Exp.TERM _ => true 
					  | _ => false)
val predicate_anynumeric = ("NUM", fn(x)=>case x of 
					      Exp.TERM t => Term.isNumeric t
					    | _ => false)

val predicate_anyconstant = ("CONST", fn(x)=>case x of 
						 Exp.TERM t => Term.isConstant t
					       | _ => false)
val predicate_anysymbol = ("SYM", fn(x)=>case x of 
					     Exp.TERM t => Term.isSymbol t
					   | _ => false)
val predicate_anylocal = ("SYM", fn(x)=>case x of 
					    Exp.TERM t => Term.isSymbol t andalso Term.isLocal t
					  | _ => false)

val predicate_anyscalar = 
    ("SCALAR", 
  fn Exp.TERM t => Term.isScalar t
   | _ => false)

val predicate_anynonscalar =
    ("NONSCALAR",
  fn Exp.TERM t => not (Term.isScalar t)
   | Exp.CONTAINER _ => true
   | Exp.CONVERSION _ => true
   | _ => false)

val predicate_anyzeroscalar =
    ("ZERO",
    fn Exp.TERM t => Term.isZero t
     | _ => false)

val predicate_anycomplex = 
    ("COMPLEX", 
  fn Exp.TERM (Exp.COMPLEX _) => true
   | _ => false)


val predicate_anydiffterm = ("DIFFTERM", 
			  fn(x)=>case x 
				  of Exp.TERM (Exp.SYMBOL (name, props)) => 
				     (case Property.getDerivative props 
				       of SOME (order, _) => order >= 1 
					| NONE => false)
				   | _ => false)

fun predicate_instance_with_classname sym = ("INST",
					     fn(x)=>case x 
						     of Exp.FUN (Fun.INST {classname,...}, _) => classname = sym
						      | _ => false)
fun predicate_instance_with_classname_and_output (name, output) = 
    ("INST", fn(x)=>case x 
		     of Exp.FUN (Fun.OUTPUT {classname, outname, ...}, _) => 
			classname = name andalso outname = output
		      | _ => false)

fun notpred ((id, pred):predicate):predicate = 
    ("!" ^ id, (fn(x) => not (pred x)))
    

fun gen_predicate_from_symbol sym = 
    ("ASYM", 
     fn(x)=>case x 
	     of Exp.TERM (Exp.SYMBOL (name, props)) => name = sym
	      | _ => false)

fun predicate_anysymbol_with_iter iter_sym =
    ("ASYM_WITH_ITER", 
     fn(x)=>case x
	     of Exp.TERM (Exp.SYMBOL (name, props)) => 
		(case Property.getIterator props of
		     SOME (iter_sym',_) => iter_sym = iter_sym'
		   | NONE => false)
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
