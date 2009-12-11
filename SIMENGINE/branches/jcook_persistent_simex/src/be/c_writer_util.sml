structure CWriterUtil =
struct

open Printer

val e2s = ExpPrinter.exp2str
val i2s = Util.i2s
val r2s = Util.real2exact_str
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
	    Util.repStr(str, "$"^(i2s i), addParen (exp2c_str e, e))

	fun notation2c_str (v, FunProps.INFIX) = 
	    String.concatWith (" "^v^" ") (map (fn(e)=>addParen ((exp2c_str e),e)) exps)
	  | notation2c_str (v, FunProps.PREFIX) = 
	    v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2c_str e,e))) exps)) ^ ")"
	  | notation2c_str (v, FunProps.POSTFIX) = 
	    (String.concatWith " " (map (fn(e)=> addParen ((exp2c_str e),e)) exps)) ^ " " ^ v
	  | notation2c_str (v, FunProps.MATCH) = 
	    foldl (fn((exp, index),str')=>replaceIndex str' (index+1,exp)) v (Util.addCount exps)

    in
	notation2c_str (FunProps.fun2cstrnotation str)
    end
  | exp2c_str (Exp.TERM term) = term2c_str term
  | exp2c_str (Exp.CONTAINER c) =
    let
	fun array2str a = "{" ^ (String.concatWith ", " (map exp2c_str (Container.array2list a))) ^ "}"
	fun matrix2str m = 
	    let
		val arrays = Container.matrix2rows m
	    in
		"{" ^ "\n" ^
		(String.concatWith ",\n" (map array2str arrays)) ^ "\n" ^
		"}"
	    end
    in
	case c of
	    Exp.EXPLIST l => DynException.stdException ("Cannot write EXPLIST expressions", "CWriter.exp2c_str", Logger.INTERNAL)
	  | Exp.ARRAY a => array2str a
	  | Exp.MATRIX m => matrix2str m
    end
  | exp2c_str (Exp.META _) = 
    DynException.stdException ("Cannot write META expressions.", "CWriter.exp2c_str", Logger.INTERNAL)
    

and term2c_str (Exp.RATIONAL (n,d)) = "(FLITERAL("^(i2s n) ^ ".0)/FLITERAL(" ^ (i2s d) ^ ".0))" (* must make it float for proper eval *)
  | term2c_str (Exp.INT v) = i2s v
  | term2c_str (Exp.REAL v) = if Real.isFinite v then "FLITERAL("^(r2s v)^")"
			      else if Real.isNan v then "NAN" 
			      else if v < 0.0 then "-INFINITY" 
			      else "INFINITY"
  | term2c_str (Exp.NAN) = "NAN"
  | term2c_str (Exp.INFINITY) = "INFINITY"
  | term2c_str (Exp.BOOL v) = if v then "1" else "0"
  | term2c_str (Exp.TUPLE l) = "("^(String.concatWith ", " (map (fn(t)=>exp2c_str (Exp.TERM t)) l))^")"
  | term2c_str (term as (Exp.SYMBOL (s, props))) = (*Term.sym2c_str (s, props)*)
	let
	    val base_str = Term.sym2c_str (s, props)
	    val spatial_iterators = TermProcess.symbol2spatialiterators term
	    val iter_strs = List.filter (fn(str)=> str <> "") (map Iterator.iterator2c_str spatial_iterators)
	in
	    base_str ^ (if List.length iter_strs > 0 then "["^(String.concatWith ", " iter_strs)^"]" else "")
	end
  | term2c_str Exp.DONTCARE = "_"
  | term2c_str term =
    DynException.stdException (("Can't write out term '"^(e2s (Exp.TERM term))^"'"),"CWriter.exp2c_str", Logger.INTERNAL)

fun iter2range class (itersym, itertype) = 
    case List.find (fn{name,...}=> name=itersym) (#iterators class) of
	SOME v => v
      | NONE => DynException.stdException(("Iterator '"^(Symbol.name itersym)^
					   "' not found in class '"^
					   (Symbol.name (#name class))^"'"), 
					  "CWriterUtil.iter2range",
					  Logger.INTERNAL)
						

fun expandprogs2parallelfor (class: DOF.class) (exp, progs) = 
    let
	val size = ExpProcess.exp2size (#iterators class) exp
	val spatial_iterators = ExpProcess.exp2spatialiterators exp
    in
	[$("{"),
	 SUB(Util.flatmap (fn(sym, _)=> [$("int iterator_" ^ (Symbol.name sym) ^ ";"),
					 $("CDATAFORMAT "^(Symbol.name (Util.sym2codegensym sym))^";")]) spatial_iterators),
	 SUB(foldl
		 (fn(iter as (sym, _) , progs)=>
		    let 
			val i = Symbol.name sym
			val {name,high,low,step} = iter2range class iter
			val size = Real.ceil ((high-low)/step) + 1
		    in
			[$("for (iterator_"^i^" = 0; iterator_"^i^" < "^(i2s size)^"; iterator_"^i^"++) {"),
			 SUB($(Symbol.name (Util.sym2codegensym sym) ^ " = " ^
			       "((CDATAFORMAT)iterator_"^i^")*((CDATAFORMAT)"^(r2s step)^")+"^(r2s low)^";")
			     ::progs),
			 $("}")]
		    end)
		 progs
		 spatial_iterators),
	 $("}")]
    end

fun exp2parallelfor (class:DOF.class) exp = 
    let
	val base_stm = $(exp2c_str exp ^ ";")
    in
	expandprogs2parallelfor class (exp, [base_stm])
    end

fun expsym2parts class exp = 
    case exp of 
	Exp.TERM (s as (Exp.SYMBOL (sym, props))) => 
	let
	    val scope = Property.getScope props
	    val prefix = case scope of
			 Property.LOCAL => ""
		       | Property.READSTATE v => "rd_" ^ (Symbol.name v)
		       | Property.READSYSTEMSTATE v => "sys_rd->" ^ (Symbol.name v)
		       | Property.WRITESTATE v => "wr_" ^ (Symbol.name v)
		       | Property.ITERATOR => "iter"

	    (*val (order, vars) = case Property.getDerivative props
				 of SOME (order, iters) => (order, iters)
				  | NONE => (0, [])*)
					    
	    val spatial_iterators = ExpProcess.exp2spatialiterators exp
	    val n = Symbol.name sym

	in
	    {prefix=prefix,
	     identifier=n,
	     iterators=Iterator.iterators2c_str spatial_iterators}
	end
      | _ => DynException.stdException(("Can't extract parts from non symbol expression '"^(e2s exp)^"'"),
				       "CWriterUtil.expsym2parts",
				       Logger.INTERNAL)

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
	val exps = Util.flatmap (fn {contents, condition, ...} => condition :: contents) outputs
	val all_symbols = Util.flatmap ExpProcess.exp2termsymbols exps
	val sym_mapping = map (fn(term)=>(term, Term.sym2curname term)) all_symbols
	fun cmp_fun ((_,s1),(_,s2))= s1 = s2
	val unique_symbols = Util.uniquify_by_fun cmp_fun sym_mapping
    in
	unique_symbols
    end

end
