signature MATHEMATICA =
sig

datatype MathematicaExp = 
	 MASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MDELAYASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MEXP of Exp.exp
       | MFUN of {name:Symbol.symbol, args:(Symbol.symbol * MathematicaExp option) list}
       | MODULE of {locals:Symbol.symbol list, exps:MathematicaExp list, return:MathematicaExp}
       | MBUILTIN of {funname:Symbol.symbol, args:MathematicaExp list}
       | MREWRITE of {mexp:MathematicaExp, rewrites:(MathematicaExp * MathematicaExp) list}

val mexp2prog : MathematicaExp -> Printer.text
val exp2mathematica_str : Exp.exp -> string
val term2mathematica_str : Exp.term -> string

end
structure Mathematica : MATHEMATICA =
struct

open Printer

datatype MathematicaExp = 
	 MASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MDELAYASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MEXP of Exp.exp
       | MFUN of {name:Symbol.symbol, args:(Symbol.symbol * (MathematicaExp option)) list}
       | MODULE of {locals:Symbol.symbol list, exps:MathematicaExp list, return:MathematicaExp}
       | MREWRITE of {mexp:MathematicaExp, rewrites:(MathematicaExp * MathematicaExp) list}
       | MBUILTIN of {funname:Symbol.symbol, args:MathematicaExp list}

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str

val fixname = Util.mathematica_fixname
fun sym2str sym = 
    fixname (Symbol.name sym)
fun addPrefix pre sym = Symbol.symbol (pre ^ (Symbol.name sym))

fun r2ms r =
    let
	val {exp,man} = Real.toManExp r
    in
	"(" ^ (r2s man) ^ " 2^"^(i2s exp) ^ ")"
    end

fun exp2mathematica_str (exp as (Exp.FUN (Fun.BUILTIN Fun.ASSIGN,[_,Exp.FUN (Fun.INST _, _)])))=
    let
	val {classname,instname,props,inpargs,outargs} =
	    ExpProcess.deconstructInst exp
	val expsvar = addPrefix "exps" instname
	val outputs = map Term.sym2symname outargs
	val lhs = Exp.TERM (Exp.TUPLE 
				[Term.sym2term expsvar, 
					     Exp.TUPLE 
						 (map Term.sym2term outputs)])
	val lhs' = exp2mathematica_str lhs
	val rhsargs = map exp2mathematica_str inpargs
	val rhs' = "SubModel["^(fixname (Symbol.name classname))^"["^String.concatWith "," rhsargs^"]]"		   
    in
	lhs' ^ " = " ^ rhs'
    end			     
  | exp2mathematica_str (Exp.FUN (str, exps)) =
    let
	fun useParen (Exp.FUN (str', _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = FunProcess.fun2props str
		val {precedence=prec',...} = FunProcess.fun2props str'
	    in
		(prec = prec' andalso (str <> str' orelse (not assoc))) orelse prec < prec'
	    end
	  | useParen (Exp.TERM _) = false
	  | useParen (Exp.META _) = false
	  | useParen (Exp.CONTAINER _) = false

	fun addParen (str, exp) = 
	    if String.isPrefix "-" str then
		"(" ^ str ^ ")"
	    else if useParen exp then
		"(" ^ str ^")"
	    else
		str

	fun replaceIndex str (i,e) = 
	    Util.repStr(str, "$"^(i2s i), addParen (exp2mathematica_str e, e))

	fun notation2mathematica_str (v, FunProps.INFIX) = 
	    String.concatWith (" "^v^" ") (map (fn(e)=>addParen ((exp2mathematica_str e),e)) exps)
	  | notation2mathematica_str (v, FunProps.PREFIX) = 
	    v ^ "[" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2mathematica_str e,e))) exps)) ^ "]"
	  | notation2mathematica_str (v, FunProps.POSTFIX) = 
	    (String.concatWith " " (map (fn(e)=> addParen ((exp2mathematica_str e),e)) exps)) ^ " " ^ v
	  | notation2mathematica_str (v, FunProps.MATCH) = 
	    foldl (fn((exp, index),str')=>replaceIndex str' (index+1,exp)) v (Util.addCount exps)

    in
	notation2mathematica_str (FunProps.fun2mathematicastrnotation str)
    end
  | exp2mathematica_str (Exp.TERM term) = term2mathematica_str term
  | exp2mathematica_str (Exp.CONTAINER c) = 
    let
	fun list2str l = "{" ^ (String.concatWith "," l) ^ "}"
	fun explist2str l = list2str (map exp2mathematica_str l)
    in
	case c of 
	    Exp.EXPLIST l => explist2str l
	  | Exp.ARRAY a => explist2str (Container.arrayToList a)
	  | Exp.MATRIX m => list2str (map 
					  (explist2str o Container.arrayToList)
					  (Matrix.toRows m))
    end    
  | exp2mathematica_str (Exp.META _) = 
    DynException.stdException ("Cannot write META expressions.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
    

and term2mathematica_str (Exp.RATIONAL (n,d)) = "("^(i2s n) ^ "/" ^ (i2s d) ^ ")" (* must make it float for proper eval *)
  | term2mathematica_str (Exp.INT v) = i2s v
  | term2mathematica_str (Exp.REAL v) = if Real.isFinite v then "("^(r2ms v)^")"
					else if Real.isNan v then "Indeterminate" 
					else if v < 0.0 then "-Infinity" 
					else "Infinity"
  | term2mathematica_str (Exp.BOOL v) = if v then "true" else "false"
  | term2mathematica_str (Exp.TUPLE l) = "{"^(String.concatWith ", " (map term2mathematica_str l))^"}"
  | term2mathematica_str (term as (Exp.SYMBOL (s, props))) = (*Term.sym2mathematica_str (s, props)*)
	let
	    val base_str = Term.sym2mathematica_str (s, props)
	    val spatial_iterators = TermProcess.symbol2spatialiterators term
	    val iter_strs = List.filter (fn(str)=> str <> "") (map Iterator.iterator2mathematica_str spatial_iterators)
	in
	    base_str ^ (if List.length iter_strs > 0 then "["^(String.concatWith ", " iter_strs)^"]" else "")
	end
  | term2mathematica_str (Exp.COMPLEX (r,i)) = "Complex["^(term2mathematica_str r)^","^(term2mathematica_str i)^"]"
  | term2mathematica_str Exp.DONTCARE = "_"
  | term2mathematica_str Exp.INFINITY = "Infinity"
  | term2mathematica_str Exp.NAN = "Indeterminate"
  | term2mathematica_str term =
    DynException.stdException (("Can't write out term '"^(e2s (Exp.TERM term))^"'"),"Mathematica.exp2mathematica_str", Logger.INTERNAL)


fun mexp2prog mexp = 
    case mexp of
	MASSIGN {lhs,rhs} => $((text2str (mexp2prog lhs)) ^ " = " ^ (text2str (mexp2prog rhs)))
      | MDELAYASSIGN {lhs,rhs} => $((text2str (mexp2prog lhs)) ^ " := " ^ (text2str (mexp2prog rhs)))
      | MEXP exp => $(exp2mathematica_str exp)
      | MBUILTIN {funname, args} => $(Symbol.name funname ^ "["^(String.concatWith ",\n" (map (text2str o mexp2prog) args))^"]")
      | MFUN {name,args} => 
	let
	    fun sym2pat sym = 
		(sym2str sym) ^ "_"
	    fun arg2str (asym, SOME default) = (sym2pat asym ^ ":" ^ (text2str (mexp2prog default)))
	      | arg2str (asym, NONE) = sym2pat asym		
	in
	    $(sym2str name ^ "["^(String.concatWith "," (map arg2str args))^"]")
	end
      | MREWRITE {mexp,rewrites} =>
	let
	    fun exps2rewrite (from,to) =
		(text2str (mexp2prog from)) ^ "->" ^
		(text2str (mexp2prog to))
	in
	    $(text2str (mexp2prog mexp) ^ " /.\n{" ^ (String.concatWith "," (map exps2rewrite rewrites)) ^ "}")
	end
      | MODULE {locals,exps,return} =>
	SUB[$("Module["),
	    (
	     let 
		 val local_prog : text = $("{"^(String.concatWith "," (map sym2str locals))^"},")
		 val exp_progs : text list = map 
						 (fn(e)=> case mexp2prog e of 
							      $ str => $(str ^ ";")
							    | SUB progs => SUB(progs @ [$(";")]))
				     exps
		 val return_prog : text = mexp2prog return
	     in
		 SUB([local_prog] @ exp_progs @ [return_prog])
	     end
	    ),
	    $("]")]

end
