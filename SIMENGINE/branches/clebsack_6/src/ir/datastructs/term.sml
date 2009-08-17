structure Term =
struct

val i2s = Util.i2s
val same = Util.same

(*open DSLPATTERN*)

(*datatype term = RATIONAL of (int * int)
	       | INT of int
	       | REAL of real (* be careful to not allow inf and nan *)
	       | COMPLEX of (term * term)
	       | LIST of (term list * dimlist)
	       | TUPLE of (term list)
	       | SYMBOL of (string * property list)
	       | DONTCARE
	       | INFINITY
	       | NAN
	       | PATTERN of (string * predicate * patterncount)
*)

fun sym2curname (Exp.SYMBOL (s, props)) = s
  | sym2curname _ = DynException.stdException("Received an unexpected non symbol", "Term.sym2curname", Logger.INTERNAL)

fun sym2name (Exp.SYMBOL (s, props)) = 
    (case (Property.getRealName props)
     of SOME v => Symbol.name v
      | NONE => Symbol.name s)
  | sym2name _ = DynException.stdException("Received an unexpected non symbol", "Term.sym2name", Logger.INTERNAL)

fun sym2symname (Exp.SYMBOL (s, props)) = 
    (case (Property.getRealName props)
     of SOME v => v
      | NONE => s)
  | sym2symname _ = DynException.stdException("Received an unexpected non symbol", "Term.sym2symname", Logger.INTERNAL)



fun sym2str (s, props) =
    let
	val scope = Property.getScope props
	val prefix = case scope of
			 Property.LOCAL => ""
		       | Property.READSTATE v => Symbol.name v ^ "."
		       | Property.WRITESTATE v => Symbol.name v ^ "."

	val (order, vars) = case Property.getDerivative props
			      of SOME (order, iters) => (order, iters)
			       | NONE => (0, [])

	val iters = (case Property.getIterator props
		      of SOME iters => Iterator.iterators2str iters
		       | NONE => "")

	val n = prefix ^ (case (Property.getRealName props)
			   of SOME v => Symbol.name v
			    | NONE => Symbol.name s)

    in
	if order < 0 then (* integral *)
	    "Int(" ^ n ^ iters ^ ",["^(String.concatWith "," (map Symbol.name vars))^"])"
	else if order = 0 then
	    n ^ iters
	else if order > 3 andalso (same vars) then
	    n ^ "^("^(i2s order)^")" ^ iters
	else if order > 0 andalso (same vars) then
	    n ^ (foldl (op ^) "" (List.tabulate (order,(fn(x)=>"'")))) ^ iters
	else
	    "d^" ^ (i2s order)  ^ n ^ iters ^
	    "/" ^ (foldl (op ^) "" (map (fn(v)=>"d"^(Symbol.name v)) vars))
    end

fun sym2fullstr (s, props) =
    let
	val scope = Property.getScope props
	val prefix = case scope of
			 Property.LOCAL => ""
		       | Property.READSTATE v => Symbol.name v ^ "."
		       | Property.WRITESTATE v => Symbol.name v ^ "."

	val (order, vars) = case Property.getDerivative props
			      of SOME (order, iters) => (order, iters)
			       | NONE => (0, [])

	val iters = (case Property.getIterator props
		      of SOME iters => Iterator.iterators2str iters
		       | NONE => "")

	val n = prefix ^ (case (Property.getRealName props)
			   of SOME v => (Symbol.name s) ^ "[" ^ (Symbol.name v) ^ "]"
			    | NONE => Symbol.name s)

    in
	if order < 0 then (* integral *)
	    "Int(" ^ n ^ iters ^ ",["^(String.concatWith "," (map Symbol.name vars))^"])"
	else if order = 0 then
	    n ^ iters
	else if order > 3 andalso (same vars) then
	    n ^ "^("^(i2s order)^")" ^ iters
	else if order > 0 andalso (same vars) then
	    n ^ (foldl (op ^) "" (List.tabulate (order,(fn(x)=>"'")))) ^ iters
	else
	    "d^" ^ (i2s order)  ^ n ^ iters ^
	    "/" ^ (foldl (op ^) "" (map (fn(v)=>"d"^(Symbol.name v)) vars))
    end

fun sym2c_str (s, props) =
    let
	val ep_index = Property.getEPIndex props

	val scope = Property.getScope props

	val prefix = 
	    let val index = case ep_index
			     of SOME Property.STRUCT_OF_ARRAYS => "[STRUCT_IDX]."
			      | _ => "->"
	    in case scope
		of Property.LOCAL => ""
		 | Property.READSTATE v => Symbol.name v ^ index
		 | Property.WRITESTATE v => Symbol.name v ^ index
	    end

	val suffix = case ep_index 
		      of SOME _ => "[ARRAY_IDX]"
		       | NONE => ""

	val (order, vars) = case Property.getDerivative props
			      of SOME (order, iters) => (order, iters)
			       | NONE => (0, [])

	val iters = (case Property.getIterator props
		      of SOME iters => Iterator.iterators2str iters
		       | NONE => "")

	val n = Symbol.name s

    in
	if order < 0 then (* integral *)
	    (*"Int(" ^ n ^ iters ^  ",["^(String.concatWith "," (map Symbol.name vars))^"])"*)
	    DynException.stdException(("Can't support integrals ("^(sym2str (s, props))^")"), "DSL_TERMS.sym2c_str", Logger.INTERNAL)
	else if order = 0 then
	    (if iters = "[n+1]" then
		 prefix ^ n ^ suffix
		 (*"next_" ^ n*)
	     else
		 prefix ^ n ^ suffix(*^ iters*))
	else if order = 1 then
	    (*"d_" ^ n ^ "_dt"*) (* only support first order derivatives with respect to t *)
	    prefix ^ n ^ suffix
	else
	    DynException.stdException(("Can't support higher order derivative terms ("^(sym2str (s, props))^")"), "DSL_TERMS.sym2c_str", Logger.INTERNAL)
	(*else if order > 3 andalso (same vars) then
	    n ^ "^("^(i2s order)^")" ^ iters
	else if order > 0 andalso (same vars) then
	    n ^ (foldl (op ^) "" (List.tabulate (order,(fn(x)=>"'")))) ^ iters
	else
	    "d^" ^ (i2s order)  ^ n ^ iters ^
	    "/" ^ (foldl (op ^) "" (map (fn(v)=>"d"^(Symbol.name v)) vars))*)
    end



fun isZero term =
    case term of
	Exp.RATIONAL (v, _) => (v = 0) orelse (v = ~0)
      | Exp.INT v => (v = 0) orelse (v = ~0)
      | Exp.REAL v => (Real.==(v, 0.0)) orelse (Real.==(v, ~0.0))
      | Exp.COMPLEX (a,b) => (isZero a) andalso (isZero b)
      | Exp.LIST (l,_) => List.all isZero l
      | Exp.TUPLE l => List.all isZero l
      | _ => false

fun isNumeric term = 
    case term of
	Exp.RATIONAL _ => true
      | Exp.INT _ => true
      | Exp.COMPLEX (t1, t2) => (isNumeric t1) andalso (isNumeric t2)
      | Exp.LIST (l, _) => List.all isNumeric l
      | Exp.TUPLE l => List.all isNumeric l
      | Exp.INFINITY => true
      | Exp.NAN => true
      | _ => false

fun isSymbol term =
    case term of
	Exp.SYMBOL _ => true
      | Exp.LIST (l, _) => List.all isSymbol l
      | Exp.TUPLE l => List.all isSymbol l
      | Exp.COMPLEX (a,b) => (isSymbol a) andalso (isSymbol b)
      | _ => false

fun isScalar term =
    case term of
	Exp.LIST _ => false
      | Exp.TUPLE _ => false
      | _ => true

fun isReadState term =
    case term of
	Exp.SYMBOL (_, props) => (case (Property.getScope props) of
				      Property.READSTATE v => true
				    | _ => false)
      | _ => false
					  
fun isWriteState term =
    case term of
	Exp.SYMBOL (_, props) => (case (Property.getScope props) of
				      Property.WRITESTATE v => true
				    | _ => false)
      | _ => false
					  
fun termCount term =
    case term of
	Exp.SYMBOL _ => 1 (* this might have to change *)
      | Exp.LIST (l, _) => length l
      | Exp.TUPLE l => length l
      | _ => 1

fun isInitialValue term itersym = 
    case term of
	Exp.SYMBOL (sym, props) => 
	(case Property.getSpecificIterator props itersym of
	     SOME (_,Iterator.ABSOLUTE 0) => true
	   | _ => false)
      | _ => false

fun symbolTimeDerivativeOrder term =
    case term
     of Exp.SYMBOL (sym, props) =>
	(case Property.getDerivative props
	  of SOME (order, sym::rest)=> if sym = (Symbol.symbol "t") then 
					   order
				       else
					   0
	   | _ => 0)
      | _ => 0

fun symbolSampleDelay term =
    case term
     of Exp.SYMBOL (sym, props) =>
	(case Property.getIterator props
	      of SOME ((sym, Iterator.RELATIVE i)::rest) => if sym = (Symbol.symbol "n") then 
								i
							    else
								0
	       | _ => 0)
      | _ => 0

fun symbolSpatialSize term =
    case term
     of Exp.SYMBOL (sym, props) =>
	(case Property.getDim props
	  of SOME l => Util.prod l
	   | NONE => 1)
      | _ => 1

(* compute the memory requirements of a term *)
fun termMemorySize term =
    let
	val deriv_order = symbolTimeDerivativeOrder term
	val sample_delay = symbolSampleDelay term
    in
	(if deriv_order > 0 then deriv_order else 0) +
	(if sample_delay > 0 then sample_delay else 0)
    end

(* need to enhance with iterators and vectors/matrices when possible *)
fun termSize term =
    case term
     of Exp.SYMBOL (sym, props) =>
	let
	    (* for differential equations *)
	    val order = case Property.getDerivative props
			 of SOME (order, _) => order
			  | NONE => 0

	    (* for difference equations *)
	    val relative = case Property.getIterator props
			    of SOME ((sym, Iterator.RELATIVE i)::rest) => if i=1 then 1 else 0
			     | _ => 0
	in
	    order+relative
	end
      | _ => 0

fun termSizeByIterator iter term =
    case term
     of Exp.SYMBOL (sym, props) =>
	let
	    val absolute = case Property.getIterator props
			    of SOME ((iter', Iterator.ABSOLUTE i)::rest) => if i=0 andalso iter=iter' then 1 else 0
			     | _ => 0
	in
	    absolute
	end
      | _ => 0

end
