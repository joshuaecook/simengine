structure CWriterUtil =
struct

open Printer

val i2s = Util.i2s
val r2s = Util.r2s
val log = Util.log

fun exp2c_str (Exp.FUN (str, exps)) =
    let
	fun useParen (Exp.FUN (str', _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = FunProcess.fun2props str
		val {precedence=prec',...} = FunProcess.fun2props str'
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
	case (Fun.fun2cstrnotation str) of
	    (v, Fun.INFIX) => String.concatWith v (map (fn(e)=>addParen ((exp2c_str e),e)) exps)
	  | (v, Fun.PREFIX) => v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2c_str e,e))) exps)) ^ ")"
	  | (v, Fun.POSTFIX) => (String.concatWith " " (map (fn(e)=> addParen ((exp2c_str e),e)) exps)) ^ " " ^ v
	  | (v, Fun.MATCH) => 
	    let
		fun replaceIndex str (i,e) = 
		    Util.repStr(str, "$"^(i2s i), addParen (exp2c_str e, e))
	    in
		foldl (fn((exp, index),str')=>replaceIndex str' (index+1,exp)) v (Util.addCount exps)
	    end
			    
    end
  | exp2c_str (Exp.TERM term) =
    case term of 
	Exp.RATIONAL (n,d) => (i2s n) ^ ".0/" ^ (i2s d) ^ ".0" (* must make it float for proper eval *)
      | Exp.INT v => i2s v
      | Exp.REAL v => if Real.isNan v then "(0.0/0.0)" else r2s v
      | Exp.BOOL v => if v then "1" else "0"
      (*| COMPLEX (t1,t2) => if isZero t1 andalso isZero t2 then (exp2c_str (Exp.TERM (INT 0)))
			   else if isZero t1 then (exp2c_str (Exp.TERM t2) ^ " i")
			   else if isZero t2 then exp2c_str (Exp.TERM t1)
			   else exp2c_str (FUN (Symbol.symbol "PLUS", [Exp.TERM t1, FUN (Symbol.symbol "TIMES", [Exp.TERM t2, Exp.TERM (SYMBOL (Symbol.symbol "i",[]))])]))	*)
      (*| LIST (l,_) => "[" ^ (String.concatWith ", " (map (fn(t)=>exp2c_str (Exp.TERM t)) l)) ^ "]"*)
      | Exp.TUPLE l => "("^(String.concatWith ", " (map (fn(t)=>exp2c_str (Exp.TERM t)) l))^")"
      | Exp.SYMBOL (s, props) => Term.sym2c_str (s, props)
      | Exp.DONTCARE => "0"
      (*| INFINITY => "Inf"
      | NAN => "NaN"
      | PATTERN p => pattern2str p*)
      | _ => DynException.stdException (("Can't write out term '"^(ExpProcess.exp2str (Exp.TERM term))^"'"),"CWriter.exp2c_str", Logger.INTERNAL)


fun log_c_exps (header, exps) = 
    (log "";
     log header;
     log ("--------------------------------------");
     (app (fn(e)=>log (exp2c_str e)) exps);
     log ("--------------------------------------"))
(*
fun log_c_eqs (header, eqs) = 
    (log "";
     log header;
     log ("-----------------------------------------------------------------");
     printtexts (TextIO.stdOut, List.concat (map (fn(e)=>(eq2c_progs e)) eqs), 0);
     log ("-----------------------------------------------------------------"))
*)
fun class2uniqueoutputsymbols (class:DOF.class) = 
    let
	val outputs = !(#outputs class)
	val all_exps = (Util.flatmap (fn{contents,...}=>contents) outputs) @
		       (map (fn{condition,...}=>condition) outputs)
	val all_symbols = Util.flatmap ExpProcess.exp2termsymbols all_exps
	val sym_mapping = map (fn(term)=>(term, Term.sym2curname term)) all_symbols
	fun cmp_fun ((_,s1),(_,s2))= s1 = s2
	val unique_symbols = Util.uniquify_by_fun cmp_fun sym_mapping
    in
	unique_symbols
    end

end
