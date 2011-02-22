signature MATHEMATICA =
sig

datatype MathematicaExp = 
	 MASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MDELAYASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MEXP of Exp.exp
       | MSYMBOL of Symbol.symbol
       | MLIST of MathematicaExp list
       | MRULE of (MathematicaExp * MathematicaExp)
       | MFUN of {name:Symbol.symbol, args:(Symbol.symbol * MathematicaExp option) list}
       | MINFIX of string * (MathematicaExp * MathematicaExp)
       | MODULE of {locals:Symbol.symbol list, exps:MathematicaExp list, return:MathematicaExp}
       | MWITH of {locals:(Symbol.symbol * MathematicaExp) list, exps:MathematicaExp list, return:MathematicaExp}
       | MBUILTIN of {funname:Symbol.symbol, args:MathematicaExp list}
       | MREWRITE of {mexp:MathematicaExp, rewrites:MathematicaExp}

val mexp2prog : MathematicaExp -> Printer.text
val fixname : Symbol.symbol -> Symbol.symbol

end
structure Mathematica : MATHEMATICA =
struct

open Printer

datatype MathematicaExp = 
	 MASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MDELAYASSIGN of {lhs:MathematicaExp, rhs:MathematicaExp}
       | MSYMBOL of Symbol.symbol
       | MEXP of Exp.exp
       | MLIST of MathematicaExp list
       | MRULE of (MathematicaExp * MathematicaExp)
       | MFUN of {name:Symbol.symbol, args:(Symbol.symbol * (MathematicaExp option)) list}
       | MINFIX of string * (MathematicaExp * MathematicaExp)
       | MODULE of {locals:Symbol.symbol list, exps:MathematicaExp list, return:MathematicaExp}
       | MWITH of {locals:(Symbol.symbol * MathematicaExp) list, exps:MathematicaExp list, return:MathematicaExp}
       | MREWRITE of {mexp:MathematicaExp, rewrites:MathematicaExp}
       | MBUILTIN of {funname:Symbol.symbol, args:MathematicaExp list}

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str

val fixname = Symbol.symbol o Util.mathematica_fixname o Symbol.name
fun sym2str sym = 
    Symbol.name (fixname sym)
fun addPrefix pre sym = Symbol.symbol (pre ^ (Symbol.name sym))

fun r2ms r =
    let
	val {exp,man} = Real.toManExp r
    in
	"(" ^ (r2s man) ^ " 2^"^(i2s exp) ^ ")"
    end

(*
fun exp2mathematica_str (exp as (Exp.FUN (Fun.BUILTIN Fun.ASSIGN,[_,Exp.FUN (Fun.INST _, _)])))=
    let
	val {classname,instname,props,inpargs,outargs} =
	    ExpProcess.deconstructInst exp
	val expsvar = addPrefix "exps" instname
	val outputs = map Term.sym2symname (SymbolTable.listItems outargs)
	val lhs = Exp.TERM (Exp.TUPLE 
				[Term.sym2term expsvar, 
					     Exp.TUPLE 
						 (map Term.sym2term outputs)])
	val lhs' = exp2mathematica_str lhs
	val rhsargs = map exp2mathematica_str (SymbolTable.listItems inpargs)
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
		(prec = prec' andalso (not (FunProcess.equal (str, str')) orelse (not assoc))) orelse prec < prec'
	    end
	  | useParen (Exp.TERM _) = false
	  | useParen (Exp.META _) = false
	  | useParen (Exp.CONVERSION _) = false
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

	fun notation2mathematica_str (v, MathFunctionProperties.INFIX) = 
	    String.concatWith (" "^v^" ") (map (fn(e)=>addParen ((exp2mathematica_str e),e)) exps)
	  | notation2mathematica_str (v, MathFunctionProperties.PREFIX) = 
	    v ^ "[" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2mathematica_str e,e))) exps)) ^ "]"
	  | notation2mathematica_str (v, MathFunctionProperties.POSTFIX) = 
	    (String.concatWith " " (map (fn(e)=> addParen ((exp2mathematica_str e),e)) exps)) ^ " " ^ v
	  | notation2mathematica_str (v, MathFunctionProperties.MATCH) = 
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
	  | Exp.ASSOC t => 
	    DynException.stdException ("Cannot write ASSOC container expressions.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
	  | Exp.MATRIX m => list2str (map 
					  (explist2str o Container.arrayToList)
					  (Matrix.toRows m))
    end    
  | exp2mathematica_str (Exp.CONVERSION _) = 
    DynException.stdException ("Cannot write SUBREF expressions just yet.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
  | exp2mathematica_str (Exp.META _) = 
    DynException.stdException ("Cannot write META expressions.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
    

and term2mathematica_str (Exp.RATIONAL (n,d)) = "("^(i2s n) ^ "/" ^ (i2s d) ^ ")" (* must make it float for proper eval *)
  | term2mathematica_str (Exp.INT v) = i2s v
  | term2mathematica_str (Exp.REAL v) = if Real.isFinite v then "("^ (r2ms v)^")"
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
*)

local
open Layout

fun nbFun name args =
    seq [str name, 
	 bracketList args]
fun nbAsgn (lhs, rhs) =
    seq [lhs, str " = ", rhs]
val i2l = str o i2s
val r2l = str o r2s
val r2ml = str o r2ms
in
fun exp2mathematica_layout (exp as (Exp.FUN (Fun.BUILTIN Fun.ASSIGN,[_,Exp.FUN (Fun.INST _, _)])))=
    let
	val {classname,instname,props,inpargs,outargs} =
	    ExpProcess.deconstructInst exp
	val expsvar = addPrefix "exps" instname
	val outputs = map Term.sym2symname (SymbolTable.listItems outargs)
	val lhs = Exp.TERM (Exp.TUPLE 
				[Term.sym2term expsvar, 
					     Exp.TUPLE 
						 (map Term.sym2term outputs)])
	val lhs' = exp2mathematica_layout lhs
	val rhsargs = map exp2mathematica_layout (SymbolTable.listItems inpargs)
	val rhs' = nbFun "SubModel" [nbFun (Symbol.name (fixname classname)) rhsargs]
(*"SubModel["^(fixname (Symbol.name classname))^"["^String.concatWith "," rhsargs^"]]"		   *)
    in
	nbAsgn (lhs', rhs')
	(*lhs' ^ " = " ^ rhs'*)
    end			     
  | exp2mathematica_layout (Exp.FUN (funname, exps)) =
    let
	fun useParen (Exp.FUN (str', _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = FunProcess.fun2props funname
		val {precedence=prec',...} = FunProcess.fun2props str'
	    in
		(prec = prec' andalso (not (FunProcess.equal (funname, str')) orelse (not assoc))) orelse prec < prec'
	    end
	  | useParen (Exp.TERM _) = false
	  | useParen (Exp.META _) = false
	  | useParen (Exp.CONVERSION _) = false
	  | useParen (Exp.CONTAINER _) = false

	fun addParen (layout, exp) = 
	    if String.isPrefix "-" (toString layout) then
		paren (layout)
	    else if useParen exp then
		paren (layout)
	    else
		layout

	fun replaceIndex layout (i,e) = 
	    repStr(layout, "$"^(i2s i), toString (addParen (exp2mathematica_layout e, e)))

	fun notation2mathematica_layout (v, MathFunctionProperties.INFIX) = 
	    (*String.concatWith (" "^v^" ") (map (fn(e)=>addParen ((exp2mathematica_str e),e)) exps)*)
	    series ("",""," "^v^" ") (map (fn(e)=>addParen ((exp2mathematica_layout e),e)) exps)
	  | notation2mathematica_layout (v, MathFunctionProperties.PREFIX) = 
	    (*v ^ "[" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2mathematica_str e,e))) exps)) ^ "]"*)
	    nbFun v (map (fn(e)=>addParen((exp2mathematica_layout e,e))) exps)
	  | notation2mathematica_layout (v, MathFunctionProperties.POSTFIX) = 
	    (*(String.concatWith " " (map (fn(e)=> addParen ((exp2mathematica_str e),e)) exps)) ^ " " ^ v*)	   
	    series ("", " " ^ v, " ") (map (fn(e)=> addParen ((exp2mathematica_layout e),e)) exps)
	  | notation2mathematica_layout (v, MathFunctionProperties.MATCH) = 
	    foldl (fn((exp, index),layout')=>replaceIndex layout' (index+1,exp)) (str v) (Util.addCount exps)

    in
	notation2mathematica_layout (FunProps.fun2mathematicastrnotation funname)
    end
  | exp2mathematica_layout (Exp.TERM term) = term2mathematica_layout term
  | exp2mathematica_layout (Exp.CONTAINER c) = 
    let
	fun list2str l = "{" ^ (String.concatWith "," l) ^ "}"
	(*fun explist2str l = list2str (map exp2mathematica_str l)*)
	fun explist2layout l = curlyList (map exp2mathematica_layout l)
	fun rule (sym,value) = seq [str (sym2str sym), str "->", value]
    in
	case c of 
	    Exp.EXPLIST l => explist2layout l
	  | Exp.ARRAY a => explist2layout (Container.arrayToList a)
	  | Exp.ASSOC t => curlyList (map (fn(sym, exp)=> rule (sym, exp2mathematica_layout exp)) (SymbolTable.listItemsi t))
	   (* DynException.stdException ("Cannot write ASSOC container expressions.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)*)
	  | Exp.MATRIX m => curlyList (map 
					   (explist2layout o Container.arrayToList)
					   (Matrix.toRows m))
    end    
  | exp2mathematica_layout (Exp.CONVERSION _) = 
    DynException.stdException ("Cannot write SUBREF expressions just yet.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
  | exp2mathematica_layout (Exp.META _) = 
    DynException.stdException ("Cannot write META expressions.", "Mathematica.exp2mathematica_str", Logger.INTERNAL)
    

and term2mathematica_layout (Exp.RATIONAL (n,d)) = paren (seq [i2l n, str "/", i2l d]) (*"("^(i2s n) ^ "/" ^ (i2s d) ^ ")"*) (* must make it float for proper eval *)
  | term2mathematica_layout (Exp.INT v) = i2l v
  | term2mathematica_layout (Exp.REAL v) = if Real.isFinite v then paren (r2ml v)
					else if Real.isNan v then str "Indeterminate" 
					else if v < 0.0 then str "-Infinity" 
					else str "Infinity"
  | term2mathematica_layout (Exp.BOOL v) = if v then str "True" else str "False"
  | term2mathematica_layout (Exp.TUPLE l) = curlyList (map term2mathematica_layout l)
  | term2mathematica_layout (term as (Exp.SYMBOL (s, props))) = (*Term.sym2mathematica_str (s, props)*)
	let
	    val base_str = Term.sym2mathematica_str (s, props)
	    val spatial_iterators = TermProcess.symbol2spatialiterators term
	    val iter_strs = List.filter (fn(str)=> str <> "") (map Iterator.iterator2mathematica_str spatial_iterators)
	in
	    seq [str base_str, (if List.length iter_strs > 0 then bracketList (map str iter_strs) else empty)]
	end
  | term2mathematica_layout (Exp.COMPLEX (r,i)) = nbFun "Complex" [(term2mathematica_layout r), (term2mathematica_layout i)]
  | term2mathematica_layout Exp.DONTCARE = str "_"
  | term2mathematica_layout Exp.INFINITY = str "Infinity"
  | term2mathematica_layout Exp.NAN = str "Indeterminate"
  | term2mathematica_layout term =
    DynException.stdException (("Can't write out term '"^(e2s (Exp.TERM term))^"'"),"Mathematica.exp2mathematica_str", Logger.INTERNAL)

fun mexp2prog mexp = 
    case mexp of
	MINFIX (oper, (left, right)) => seq [mexp2prog left, str oper, mexp2prog right]
      | MASSIGN {lhs,rhs} => mexp2prog (MINFIX ("=", (lhs, rhs)))
				 (*$((text2str (mexp2prog lhs)) ^ " = " ^ (text2str (mexp2prog rhs)))*)
      | MDELAYASSIGN {lhs,rhs} => mexp2prog (MINFIX (":=", (lhs, rhs)))
      (*$((text2str (mexp2prog lhs)) ^ " := " ^ (text2str (mexp2prog rhs)))*)
      | MSYMBOL sym => str (sym2str sym)
      | MEXP exp => exp2mathematica_layout exp
      (*$(exp2mathematica_str exp)*)
      | MLIST explist => curlyList (map mexp2prog explist)
      | MRULE (lhs, rhs) => mexp2prog (MINFIX ("->", (lhs, rhs)))
      | MBUILTIN {funname, args} => 
	seq [str (Symbol.name funname), bracketList (map mexp2prog args)]
	(*$(Symbol.name funname ^ "["^(String.concatWith ",\n" (map (text2str o mexp2prog) args))^"]")*)
      | MFUN {name,args} => 
	let
	    fun sym2pat sym = 
		str ((sym2str sym) ^ "_")
	    fun arg2layout (asym, SOME default) = seq [sym2pat asym, 
						       str ":", 
						       mexp2prog default]
	      | arg2layout (asym, NONE) = sym2pat asym		
	in
	    seq [str (sym2str name), bracketList (map arg2layout args)]
	    (*$(sym2str name ^ "["^(String.concatWith "," (map arg2str args))^"]")*)
	end
      | MREWRITE {mexp,rewrites} =>
	    (case rewrites of 
		 MLIST [] => mexp2prog mexp
	       | _ => 
		 mayAlign [mexp2prog mexp,
			   str "/.",
			   mexp2prog rewrites])
      | MODULE {locals,exps,return} =>
	nbFun "Module"
	      let
		 val localvars =
		     MLIST (map MSYMBOL locals)
	      in
		  [mexp2prog localvars, series ("","",";") (map mexp2prog (exps @ [return]))]
	      end
      | MWITH {locals,exps,return} =>
	nbFun "With"
	      let
		  val assigns = MLIST (map (fn(lhs, rhs)=> MASSIGN {lhs=MSYMBOL lhs,
								    rhs=rhs}) locals)
	      in
		  [mexp2prog assigns, series ("","",";") (map mexp2prog (exps @ [return]))]
	      end
(*	SUB[$("Module["),
	    (
	     let 
		 val local_prog : text =
		     curlyList (map (str o sym2str) locals)
			       (*$("{"^(String.concatWith "," (map sym2str locals))^"},")*)
		 val exp_progs : text list = 
		     map (fn(e)=>seq [mexp2prog e, str ";"]) exps
			 (*map 
			       (fn(e)=> case mexp2prog e of 
					    $ str => $(str ^ ";")
					  | SUB progs => SUB(progs @ [$(";")]))
			       exps*)
		 val return_prog : text = mexp2prog return
	     in
		 SUB([local_prog] @ exp_progs @ [return_prog])
	     end
	    ),
	    $("]")]*)
end

end
