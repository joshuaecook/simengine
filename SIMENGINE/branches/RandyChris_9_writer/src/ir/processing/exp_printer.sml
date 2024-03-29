signature EXPPRINTER =
sig
 
(* will print in a full form or a terse form depending on the setting of "usefullform" in the options *)
val exp2str : Exp.exp -> string

end
structure ExpPrinter =
struct

val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val log = Util.log

fun exp2tersestr (Exp.FUN (f, exps)) = 
    let
	fun useParen (Exp.FUN (f', _)) = 
	    let
		val sym = FunProcess.fun2name f
		val sym' = FunProcess.fun2name f'
		val (prec, assoc) = 
		    case f of
			Fun.BUILTIN _ => 
			let
			    val {precedence, associative, ...} = FunProcess.fun2props f
			in
			    (precedence, associative)
			end
		      | Fun.INST _ => (Inst.instancePrecedence, false)
		val prec' = case f' of
				Fun.BUILTIN _ => #precedence (FunProcess.fun2props f')
			      | Fun.INST _ => Inst.instancePrecedence
	    in
		(prec = prec' andalso (sym <> sym' orelse (not assoc))) orelse prec < prec'
	    end
	  | useParen (Exp.TERM _) = false

	fun addParen (str, exp) = 
	    if hd (String.explode str) = #"-" then
		"(" ^ str ^ ")"
	    else if useParen exp then
		"(" ^ str ^")"
	    else
		str
    in
	case (FunProps.fun2textstrnotation f) of
	    (v, FunProps.INFIX) => 
	    if FunProps.hasVariableArguments f andalso length exps = 1 then
		(FunProps.op2name f) ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2tersestr e,e))) exps)) ^ ")"
	    else
		String.concatWith v (map (fn(e)=>addParen ((exp2tersestr e),e)) exps)
	  | (v, FunProps.PREFIX) => v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2tersestr e,e))) exps)) ^ ")"
	  | (v, FunProps.POSTFIX) => (String.concatWith " " (map (fn(e)=> addParen ((exp2tersestr e),e)) exps)) ^ " " ^ v
	  | (v, FunProps.MATCH) => 
	    let
		fun replaceIndex str (i,e) = 
		    Util.repStr(str, "$"^(i2s i), addParen (exp2tersestr e, e))
	    in
		foldl (fn((exp, index),str')=>replaceIndex str' (index+1,exp)) v (Util.addCount exps)
	    end
			    
    end
  | exp2tersestr (Exp.TERM term) =
    case term of 
	Exp.RATIONAL (n,d) => (i2s n) ^ "/" ^ (i2s d)
      | Exp.INT v => i2s v
      | Exp.REAL v => r2s v
      | Exp.BOOL v => b2s v
      | Exp.COMPLEX (t1,t2) => if Term.isZero t1 andalso Term.isZero t2 then (exp2tersestr (Exp.TERM (Exp.INT 0)))
			   else if Term.isZero t1 then (exp2tersestr (Exp.TERM t2) ^ " i")
			   else if Term.isZero t2 then exp2tersestr (Exp.TERM t1)
			   else exp2tersestr (ExpBuild.plus [Exp.TERM t1, ExpBuild.times [Exp.TERM t2, Exp.TERM (Exp.SYMBOL (Symbol.symbol "i",Property.default_symbolproperty))]])
      | Exp.LIST (l,_) => "[" ^ (String.concatWith ", " (map (fn(t)=>exp2tersestr (Exp.TERM t)) l)) ^ "]"
      | Exp.TUPLE l => "("^(String.concatWith ", " (map (fn(t)=>exp2tersestr (Exp.TERM t)) l))^")"
      | Exp.RANGE {low, high, step} => 
	if Term.isOne step then
	    (exp2tersestr (Exp.TERM low)) ^ ":" ^ (exp2tersestr (Exp.TERM high))
	else
	    (exp2tersestr (Exp.TERM low)) ^ ":" ^ (exp2tersestr (Exp.TERM step)) ^ ":" ^ (exp2tersestr (Exp.TERM high))
      | Exp.SYMBOL (s, props) => Term.sym2str (s, props)
      | Exp.DONTCARE => "?"
      | Exp.INFINITY => "Inf"
      | Exp.NAN => "NaN"
      | Exp.PATTERN p => PatternProcess.pattern2str p



fun exp2fullstr (Exp.FUN (f, exps)) = 
    let
	fun useParen (Exp.FUN (f', _)) = 
	    let
		val sym = FunProcess.fun2name f
		val sym' = FunProcess.fun2name f'
		val (prec, assoc) = 
		    case f of
			Fun.BUILTIN _ => 
			let
			    val {precedence, associative, ...} = FunProcess.fun2props f
			in
			    (precedence, associative)
			end
		      | Fun.INST _ => (Inst.instancePrecedence, false)
		val prec' = case f' of
				Fun.BUILTIN _ => #precedence (FunProcess.fun2props f')
			      | Fun.INST _ => Inst.instancePrecedence
	    in
		(prec = prec' andalso (sym <> sym' orelse (not assoc))) orelse prec < prec'
	    end
	  | useParen (Exp.TERM _) = false

	fun addParen (str, exp) = 
	    if hd (String.explode str) = #"-" then
		"(" ^ str ^ ")"
	    else if useParen exp then
		"(" ^ str ^")"
	    else
		str
    in
	Symbol.name (FunProcess.fun2name f) ^ "(" ^ (String.concatWith "," (map exp2fullstr exps)) ^")"
    (*
	case (FunProps.fun2textstrnotation str) of
	    (v, FunProps.INFIX) => String.concatWith v (map (fn(e)=>addParen ((exp2str e),e)) exps)
	  | (v, FunProps.PREFIX) => v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2str e,e))) exps)) ^ ")"
	  | (v, FunProps.POSTFIX) => (String.concatWith " " (map (fn(e)=> addParen ((exp2fullstr e),e)) exps)) ^ " " ^ v
	  | (v, FunProps.MATCH) => 
	    let
		fun replaceIndex str (i,e) = 
		    Util.repStr(str, "$"^(i2s i), addParen (exp2str e, e))
	    in
		foldl (fn((exp, index),str')=>replaceIndex str' (index,exp)) v (Util.addCount exps)
	    end*)
			    
    end
  | exp2fullstr (Exp.TERM term) =
    case term of 
	Exp.RATIONAL (n,d) => "Rational(" ^ (i2s n) ^ "," ^ (i2s d) ^ ")"
      | Exp.INT v => i2s v
      | Exp.REAL v => r2s v
      | Exp.BOOL v => b2s v
      | Exp.COMPLEX (t1,t2) => "Complex("^(exp2fullstr (Exp.TERM t1))^","^(exp2fullstr (Exp.TERM t2))^")"
	(*if Term.isZero t1 andalso Term.isZero t2 then (exp2fullstr (Exp.TERM (Exp.INT 0)))
			   else if Term.isZero t1 then (exp2fullstr (Exp.TERM t2) ^ " i")
			   else if Term.isZero t2 then exp2fullstr (Exp.TERM t1)
			   else exp2fullstr (Exp.FUN (Symbol.symbol "PLUS", [Exp.TERM t1, Exp.FUN (Symbol.symbol "TIMES", [Exp.TERM t2, Exp.TERM (Exp.SYMBOL (Symbol.symbol "i",Property.default_symbolproperty))])]))	*)
      | Exp.LIST (l,_) => "List(" ^ (String.concatWith "," (map (fn(t)=>exp2fullstr (Exp.TERM t)) l)) ^ ")"
      | Exp.TUPLE l => "Tuple("^(String.concatWith ", " (map (fn(t)=>exp2fullstr (Exp.TERM t)) l))^")"
      | Exp.RANGE {low, high, step} => "Range("^(exp2fullstr (Exp.TERM low))^":"^(exp2fullstr (Exp.TERM step))^":"^(exp2fullstr (Exp.TERM high))^")"
      | Exp.SYMBOL (s, props) => Term.sym2fullstr (s, props)
      | Exp.DONTCARE => "?"
      | Exp.INFINITY => "Inf"
      | Exp.NAN => "NaN"
      | Exp.PATTERN p => "Pattern(" ^ (PatternProcess.pattern2str p) ^ ")"

fun exp2str e = 
    (if DynamoOptions.isFlagSet("usefullform") then
	 exp2fullstr e
     else
	 exp2tersestr e)
    handle e => DynException.checkpoint "ExpProcess.exp2str" e



end
