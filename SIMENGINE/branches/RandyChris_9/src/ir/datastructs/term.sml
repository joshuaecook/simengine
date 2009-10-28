signature TERM =
sig

(* Access properties of terminals *)
val isNumeric: Exp.term -> bool
val isZero: Exp.term -> bool
val isOne: Exp.term -> bool
val isSymbol: Exp.term -> bool
val isScalar: Exp.term -> bool
val isLocal: Exp.term -> bool (* is this a local symbol, as opposed to being stored in a state vector *)
val isReadState: Exp.term -> bool (* not a local symbol, but rather read in as a state *)
val isWriteState: Exp.term -> bool (* not a local symbol, but rather written to a state *)
val isInitialValue: Exp.term -> Symbol.symbol -> bool (* returns if it is an initial value for a given iterator *)
val termCount: Exp.term -> int (* count the elements of a symbol, list, or tuple *)
val symbolSpatialSize: Exp.term -> int (* assumes that the term is a symbol, otherwise returns 1 by default - also, uses dim property to determine size *)

(* When accessing symbols, these methods will return the name in varying different ways *)
val sym2str : (Symbol.symbol * Property.symbolproperty) -> string (* used for pretty printing *)
val sym2fullstr : (Symbol.symbol * Property.symbolproperty) -> string (* used for pretty printing when not using the default terse output *)
val sym2c_str : (Symbol.symbol * Property.symbolproperty) -> string (* this is the call to produce a valid c representation of a symbol (works for locals, read states, and write states) *)
(* these accessors require a term that can only be a symbol *)
val sym2curname : Exp.term -> Symbol.symbol (* returns the name as stored directly in symbol (this name can be changed as it is processed while the "realname" property is always the original) *)
val sym2symname : Exp.term -> Symbol.symbol (* returns the name after checking the "realname" property *)
val sym2name : Exp.term -> string (* This is equiv to (Symbol.name o sym2symname) *)
(* grab the properties *)
val sym2props : Exp.term -> Property.symbolproperty

end
structure Term : TERM =
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

fun sym2props (Exp.SYMBOL (s, props)) = props
  | sym2props _ = DynException.stdException("Received an unexpected non symbol", "Term.sym2props", Logger.INTERNAL)

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
		       | Property.READSYSTEMSTATE v => "sys_rd." ^ (Symbol.name v) ^ "."
		       | Property.WRITESTATE v => Symbol.name v ^ "."
		       | Property.ITERATOR => ""

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
		       | Property.READSYSTEMSTATE v => "sys_rd." ^ (Symbol.name v) ^ "."
		       | Property.WRITESTATE v => Symbol.name v ^ "."
		       | Property.ITERATOR => ""

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

	val useOutputBuffer = Property.isOutputBuffer props

	val prefix = 
	    if useOutputBuffer then
		"od[modelid]."
	    else
		let 
		    val index = case ep_index
				 of SOME Property.STRUCT_OF_ARRAYS => "[STRUCT_IDX]."
				  | _ => "->"
		in 
		    case scope
		     of Property.LOCAL => ""
		      | Property.READSTATE v => Symbol.name v ^ index
		      | Property.READSYSTEMSTATE v => "sys_rd" ^ index ^ "states_" ^ (Symbol.name v) ^ "->"
 		      | Property.WRITESTATE v => Symbol.name v ^ index
		      | Property.ITERATOR => ""
		end

	val suffix = if useOutputBuffer then
			 ""
		     else
			 case ep_index 
			  of SOME _ => "[ARRAY_IDX]"
			   | NONE => ""

	val (order, vars) = case Property.getDerivative props
			      of SOME (order, iters) => (order, iters)
			       | NONE => (0, [])

	val iters = (case Property.getIterator props
		      of SOME iters => Iterator.iterators2c_str iters
		       | NONE => "")

	val n = Symbol.name s

    in
	if order < 0 then (* integral *)
	    (*"Int(" ^ n ^ iters ^  ",["^(String.concatWith "," (map Symbol.name vars))^"])"*)
	    DynException.stdException(("Can't support integrals ("^(sym2str (s, props))^")"), "DSL_TERMS.sym2c_str", Logger.INTERNAL)
	else if order = 0 then
	    prefix ^ n ^ suffix
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

fun isOne term =
    case term of
	Exp.RATIONAL (n, d) => (n = d) andalso (d <> 0)
      | Exp.INT v => (v = 1)
      | Exp.REAL v => (Real.==(v, 1.0))
      | Exp.COMPLEX (a,b) => (isOne a) andalso (isZero b)
      | Exp.LIST (l,_) => List.all isOne l
      | Exp.TUPLE l => List.all isOne l
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
					  
					  
fun isLocal term =
    case term of
	Exp.SYMBOL (_, props) => (case (Property.getScope props) of
				      Property.LOCAL => true
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

fun makeInteger t = 
    case t of
	Exp.INT i => t
      | Exp.BOOL true => Exp.INT 1
      | Exp.BOOL false => Exp.INT 0
      | _ => DynException.stdException(("Invalid input"), "Term.makeInteger", Logger.INTERNAL)

and makeReal t =
    case t of
	Exp.REAL _ => t
      | Exp.INT i => Exp.REAL (Real.fromInt i)
      | Exp.BOOL _ => makeReal (makeInteger t)
      | Exp.INFINITY => Exp.REAL (Real.posInf)
      | Exp.NAN => Exp.REAL (0.0/0.0)
      | _ => DynException.stdException(("Invalid input"), "Term.makeReal", Logger.INTERNAL)

and makeList (t, dimlist) =
    let
	val size = Util.prod dimlist
    in
	Exp.LIST (List.tabulate (size, fn(x)=>t), dimlist)
    end
    
and makeRange t =
    case t of 
	Exp.RANGE _ => DynException.stdException(("Invalid range input"), "Term.makeRange", Logger.INTERNAL)
      | Exp.COMPLEX _ => DynException.stdException(("Invalid complex input"), "Term.makeRange", Logger.INTERNAL)
      | _ => Exp.RANGE {low=t, high=t, step=t}

and makeComplex t = 
    case t of 
	Exp.COMPLEX (t1, t2) => Exp.COMPLEX (t1, t2)
      | _ => Exp.COMPLEX (makeCommensurable (t, Exp.INT 0))

(* make commensurable will attempt to transform dissimilar terms into similar terms for the purposes of evaluation *)
and makeCommensurable (t1, t2) = 
    case (t1, t2) of 
	(Exp.BOOL _, Exp.BOOL _) => (t1, t2)
      | (Exp.INT _, Exp.INT _) => (t1, t2)
      | (Exp.REAL _, Exp.REAL _) => (t1, t2)
      | (Exp.COMPLEX (a1,b1), Exp.COMPLEX (a2, b2)) =>
	let
	    val (a1', b1') = makeCommensurable (a1, b1)
	    val (a2', b2') = makeCommensurable (a2, b2)
	    val (a1'', a2'') = makeCommensurable (a1', a2')
	    val (b1'', b2'') = makeCommensurable (b1', b2')
	in
	    (Exp.COMPLEX (a1'', b1''), Exp.COMPLEX (a2'', b2''))
	end
      | (Exp.LIST (l1, d1), Exp.LIST (l2, d2)) => 
	if (List.length l1) = (List.length l2) then
	    let
		val (l1', l2') = ListPair.unzip (map makeCommensurable (ListPair.zip (l1, l2)))
	    in
		(Exp.LIST (l1', d1), Exp.LIST (l2', d2))
	    end
 	else 
	    DynException.stdException(("Invalid lists"), "Term.makeCommensurable [List,List]", Logger.INTERNAL)	    
      | (Exp.TUPLE l1, Exp.TUPLE l2) => 
	if (List.length l1) = (List.length l2) then
	    let
		val (l1', l2') = ListPair.unzip (map makeCommensurable (ListPair.zip (l1, l2)))
	    in
		(Exp.TUPLE l1', Exp.TUPLE l2')
	    end
 	else 
	    DynException.stdException(("Invalid tuples"), "Term.makeCommensurable [Tuple,Tuple]", Logger.INTERNAL)
      | (Exp.RANGE _, Exp.RANGE _) => (t1, t2)
      | (Exp.NAN, Exp.NAN) => (t1, t2)
      | (Exp.INFINITY, Exp.INFINITY) => (t1, t2)
      | (Exp.DONTCARE, _) => (t2, t2)
      | (_, Exp.DONTCARE) => (t1, t1)
      | (Exp.INT _, Exp.BOOL _) => (t1, makeInteger t2)
      | (Exp.BOOL _, Exp.INT _) => (makeInteger t1, t2)
      | (Exp.REAL _, Exp.INT _) => (t1, makeReal t2)
      | (Exp.INT _, Exp.REAL _) => (makeReal t1, t2)
      | (Exp.REAL _, Exp.BOOL _) => (t1, makeReal t2)
      | (Exp.BOOL _, Exp.REAL _) => (makeReal t1, t2)
      | (Exp.COMPLEX _, Exp.BOOL _) => makeCommensurable (t1, makeComplex t2)
      | (Exp.BOOL _, Exp.COMPLEX _) => makeCommensurable (makeReal t1, t2)
      | (Exp.COMPLEX _, Exp.INT _) => makeCommensurable (t1, makeComplex t2)
      | (Exp.INT _, Exp.COMPLEX _) => makeCommensurable (makeReal t1, t2)
      | (Exp.COMPLEX _, Exp.REAL _) => makeCommensurable (t1, makeComplex t2)
      | (Exp.REAL _, Exp.COMPLEX _) => makeCommensurable (makeReal t1, t2)
      | (Exp.NAN, _) => makeCommensurable (makeReal t1, t2)
      | (_, Exp.NAN) => makeCommensurable (t1, makeReal t2)
      | (Exp.INFINITY, _) => makeCommensurable (makeReal t1, t2)
      | (_, Exp.INFINITY) => makeCommensurable (t1, makeReal t2)
      | (Exp.LIST (l1, d1), _) => makeCommensurable (t1, makeList (t2, d1))			     
      | (_, Exp.LIST (l2, d2)) => makeCommensurable (makeList (t1, d2), t2)
      | (Exp.TUPLE l1, _) => makeCommensurable (t1, Exp.TUPLE [t2])
      | (_, Exp.TUPLE l2) => makeCommensurable (Exp.TUPLE [t1], t2)
      | _ => (t1, t2)

fun isCommensurable (t1, t2) =
    case (t1, t2) of
	(Exp.BOOL _, Exp.BOOL _) => true
      | (Exp.INT _, Exp.INT _) => true
      | (Exp.REAL _, Exp.REAL _) => true
      | (Exp.COMPLEX (r1, i1), Exp.COMPLEX (r2, i2)) => 
	(isCommensurable (r1, i1) andalso (isCommensurable (r2, i2)) andalso
	 isCommensurable (r1, r2) andalso (isCommensurable (i1, i2)))
      | (Exp.LIST (l1,_), Exp.LIST (l2,_)) => 
	(List.length l1) = (List.length l2) andalso
	(List.all isCommensurable (ListPair.zip (l1, l2)))
      | (Exp.TUPLE l1, Exp.TUPLE l2) => 
	(List.length l1) = (List.length l2) andalso
	(List.all isCommensurable (ListPair.zip (l1, l2)))
      | (Exp.INFINITY, Exp.INFINITY) => true
      | (Exp.NAN, Exp.NAN) => true
      | (Exp.DONTCARE, Exp.DONTCARE) => true
      | _ => false

end
