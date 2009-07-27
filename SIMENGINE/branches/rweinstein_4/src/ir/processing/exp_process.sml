structure ExpProcess =
struct

val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val log = Util.log

fun exp2symbol_names (Exp.FUN (_, exps)) = 
    List.concat (map exp2symbol_names exps)
  | exp2symbol_names (Exp.TERM (Exp.SYMBOL (var, _))) = [Symbol.name var]
  | exp2symbol_names (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2symbol_names (Exp.TERM t)) terms)
  | exp2symbol_names (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2symbol_names (Exp.TERM t)) terms)
  | exp2symbol_names (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2symbol_names (Exp.TERM t1)) @ (exp2symbol_names (Exp.TERM t2))
  | exp2symbol_names _ = []
    
fun exp2symbols (Exp.FUN (_, exps)) = 
    List.concat (map exp2symbols exps)
  | exp2symbols (Exp.TERM (Exp.SYMBOL (var, _))) = [var]
  | exp2symbols (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2symbols (Exp.TERM t)) terms)
  | exp2symbols (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2symbols (Exp.TERM t)) terms)
  | exp2symbols (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2symbols (Exp.TERM t1)) @ (exp2symbols (Exp.TERM t2))
  | exp2symbols _ = []
    

fun exp2fun_names (Exp.FUN (funname, exps)) = (Symbol.name funname)::(List.concat (map exp2fun_names exps))
  | exp2fun_names _ = []

val uniqueid = ref 0

fun uniq(sym) =
    (uniqueid := (!uniqueid)+1;
     (Symbol.symbol ((Symbol.name sym) ^ "_U" ^ (i2s (!uniqueid)))))

fun exp2term (Exp.TERM t) = t
  | exp2term _ = Exp.NAN



(*fun sort_explist *)

fun exp2tersestr (Exp.FUN (str, exps)) = 
    let
	fun useParen (Exp.FUN (str', _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = Fun.fun2props str
		val {precedence=prec',...} = Fun.fun2props str'
	    in
		(prec = prec' andalso (str <> str' orelse (not assoc))) orelse prec < prec'
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
	case (Fun.fun2textstrnotation str) of
	    (v, Fun.INFIX) => String.concatWith v (map (fn(e)=>addParen ((exp2tersestr e),e)) exps)
	  | (v, Fun.PREFIX) => v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2tersestr e,e))) exps)) ^ ")"
	  | (v, Fun.POSTFIX) => (String.concatWith " " (map (fn(e)=> addParen ((exp2tersestr e),e)) exps)) ^ " " ^ v
	  | (v, Fun.MATCH) => 
	    let
		fun replaceIndex str (i,e) = 
		    Util.repStr(str, "$"^(i2s i), addParen (exp2tersestr e, e))
	    in
		foldl (fn((exp, index),str')=>replaceIndex str' (index,exp)) v (Util.addCount exps)
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
			   else exp2tersestr (Exp.FUN (Symbol.symbol "PLUS", [Exp.TERM t1, Exp.FUN (Symbol.symbol "TIMES", [Exp.TERM t2, Exp.TERM (Exp.SYMBOL (Symbol.symbol "i",Property.default_symbolproperty))])]))	
      | Exp.LIST (l,_) => "[" ^ (String.concatWith ", " (map (fn(t)=>exp2tersestr (Exp.TERM t)) l)) ^ "]"
      | Exp.TUPLE l => "("^(String.concatWith ", " (map (fn(t)=>exp2tersestr (Exp.TERM t)) l))^")"
      | Exp.SYMBOL (s, props) => Term.sym2str (s, props)
      | Exp.DONTCARE => "?"
      | Exp.INFINITY => "Inf"
      | Exp.NAN => "NaN"
      | Exp.PATTERN p => PatternProcess.pattern2str p



fun exp2fullstr (Exp.FUN (str, exps)) = 
    let
	fun useParen (Exp.FUN (str', _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = Fun.fun2props str
		val {precedence=prec',...} = Fun.fun2props str'
	    in
		(prec = prec' andalso (str <> str' orelse (not assoc))) orelse prec < prec'
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
	Symbol.name str ^ "(" ^ (String.concatWith "," (map exp2fullstr exps)) ^")"
    (*
	case (Fun.fun2textstrnotation str) of
	    (v, Fun.INFIX) => String.concatWith v (map (fn(e)=>addParen ((exp2str e),e)) exps)
	  | (v, Fun.PREFIX) => v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2str e,e))) exps)) ^ ")"
	  | (v, Fun.POSTFIX) => (String.concatWith " " (map (fn(e)=> addParen ((exp2fullstr e),e)) exps)) ^ " " ^ v
	  | (v, Fun.MATCH) => 
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
      | Exp.SYMBOL (s, props) => Term.sym2fullstr (s, props)
      | Exp.DONTCARE => "?"
      | Exp.INFINITY => "Inf"
      | Exp.NAN => "NaN"
      | Exp.PATTERN p => "Pattern(" ^ (PatternProcess.pattern2str p) ^ ")"

fun exp2str e = 
    if DynamoOptions.isFlagSet("usefullform") then
	exp2fullstr e
    else
	exp2tersestr e


fun eq2str (lhs, rhs) =
    ExpBuild.equals (lhs, rhs)



fun renameSym (orig_sym, new_sym) exp =
    case exp of
	Exp.FUN (f, args) => Exp.FUN (f, map (renameSym (orig_sym, new_sym)) args)
      | Exp.TERM (Exp.SYMBOL (sym, props)) => if sym = orig_sym then
						  Exp.TERM (Exp.SYMBOL (new_sym, case Property.getRealName props
										  of SOME sym => props
										   | NONE  => Property.setRealName props orig_sym))
					      else
						  exp
      | Exp.TERM (Exp.TUPLE terms) => 
	let
	    val new_terms = map (fn(t)=> exp2term ((renameSym (orig_sym, new_sym)) (Exp.TERM t))) terms
	in
	    Exp.TERM (Exp.TUPLE (new_terms))
	end
      | _ => exp

fun log_exps (header, exps) = 
    (log "";
     log header;
     log ("--------------------------------------");
     (app (fn(e)=>log (exp2str e)) exps);
     log ("--------------------------------------"))


end
(*
open DSLEXP;


val exps = [initvar "u" equals (int 1),
	    initvar "w" equals (int 1),
	    diff "u" equals (plus [tvar "u",neg (power (tvar "u", int 3)), neg (tvar "w"), var "I"]),
	    diff "w" equals (times [var "e", plus [var "b0", times [var "b1", tvar "u"], neg (tvar "w")]])];

val _ = log ""
val _ = log ""
val _ = log "Test Output: "
val _ = log ("--------------------------------------")
val _ = log (exp2str (plus [int 1, int 2, var "a"]))
(*val _ = log (exp2str (Exp.FUN ("PLUS", [Exp.TERM (INT 1), Exp.TERM (INT 2), Exp.TERM (Exp.SYMBOL ("a",[]))])))*)
val _ = log ((exp2str (times [plus [var "c", var "d"], var "a"])))
val _ = log ((exp2str (times [plus [var "d", var "e"], plus[var "a",times[var "x", var "y"], var "c"]])))
val _ = log ((exp2str (var "y" equals (exp (plus [var "x", int 1])))))
val _ = log ((exp2str (var "y" equals (exp (var "x")))))
val _ = log ""
val _ = log_exps ("FN Model", exps)
*)

