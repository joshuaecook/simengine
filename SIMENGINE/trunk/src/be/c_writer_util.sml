structure CWriterUtil =
struct

open Printer

val e2s = ExpPrinter.exp2str
val i2s = Util.i2s
val r2s = Util.real2exact_str
val log = Util.log

local
    open Spil 
    structure T = Type
    structure X = Expression
    structure A = Atom
    structure Op = Operator

    val v = Vector.fromList

    structure MF = MathFunctions

    val STRUCT_IDX = A.CompileVar (fn () => A.Literal (Const "STRUCT_IDX"), T.C"int")
    val ARRAY_IDX = A.CompileVar (fn () => A.Literal (Const "ARRAY_IDX"), T.C"int")

    fun symbol_to_expr (sym, props) =
	let
	    fun rename sym =
		Symbol.symbol (
		let val name = Symbol.name sym in
		    if String.isPrefix "#" name then
			"INTERNAL_"^(String.extract (name,1,NONE))
		    else name
		end)
	    val sym = rename sym
	in
	    if Property.isOutputBuffer props then
		X.Apply {oper= Op.Record_extract,
			 args= v[X.Apply {oper= Op.Array_extract,
					  args= v[X.Value (A.Variable "od"), X.Value (A.Variable "modelid")]},
				 X.Value (A.Symbol (Symbol.name sym))]}
	    else case Property.getScope props
		  of Property.LOCAL => local_to_expr (sym, props)
		   | Property.ITERATOR => iterator_to_expr (sym, props)
		   | Property.READSTATE scope => read_to_expr scope (sym, props)
		   | Property.READSYSTEMSTATE scope => sysread_to_expr scope (sym, props)
		   | Property.WRITESTATE scope => write_to_expr scope (sym, props)
		   | Property.SYSTEMITERATOR => sysiterator_to_expr (sym, props)
	end
    and local_to_expr (sym, props) =
	X.Value (A.Variable (Symbol.name sym))
    and iterator_to_expr (sym, props) =
	X.Value (A.Variable (Symbol.name sym))
    and read_to_expr scope (sym, props) =
	let
	    val record = 
		X.Apply {oper= Op.Array_extract,
			 args= v[X.Value (A.Variable ("rd_"^(Symbol.name scope))),
				 case Property.getEPIndex props
				  of SOME Property.STRUCT_OF_ARRAYS => X.Value STRUCT_IDX
				   | _ => X.Value (A.Literal (Int 0))]}
	    val field =
		X.Apply {oper= Op.Record_extract,
			 args= v[record, X.Value (A.Symbol (Symbol.name sym))]}
	in
	    if Option.isSome (Property.getEPIndex props) then
		X.Apply {oper= Op.Array_extract,
			 args= v[field, X.Value ARRAY_IDX]}
	    else field
	end
    and write_to_expr scope (sym, props) =
	let
	    val record = 
		X.Apply {oper= Op.Array_extract,
			 args= v[X.Value (A.Variable ("wr_"^(Symbol.name scope))),
				 case Property.getEPIndex props
				  of SOME Property.STRUCT_OF_ARRAYS => X.Value STRUCT_IDX
				   | _ => X.Value (A.Literal (Int 0))]}
	    val field =
		X.Apply {oper= Op.Record_extract,
			 args= v[record, X.Value (A.Symbol (Symbol.name sym))]}
	in
	    if Option.isSome (Property.getEPIndex props) then
		X.Apply {oper= Op.Array_extract,
			 args= v[field, X.Value ARRAY_IDX]}
	    else field
	end
    and sysread_to_expr scope (sym, props) =
	let
	    val record = 
		X.Apply {oper= Op.Record_extract,
			 args= v[X.Value (A.Variable "sys_rd"),
				 X.Value (A.Symbol ("states_"^(Symbol.name scope)))]}
	    val record = 
		X.Apply {oper= Op.Array_extract,
			 args= v[record,
				 case Property.getEPIndex props
				  of SOME Property.STRUCT_OF_ARRAYS => X.Value STRUCT_IDX
				   | _ => X.Value (A.Literal (Int 0))]}
	    val field =
		X.Apply {oper= Op.Record_extract,
			 args= v[record, X.Value (A.Symbol (Symbol.name sym))]}
	in
	    if Option.isSome (Property.getEPIndex props) then
		X.Apply {oper= Op.Array_extract,
			 args= v[field, X.Value ARRAY_IDX]}
	    else field
	end
    and sysiterator_to_expr (sym, props) =
	let
	    val record = X.Value (A.Variable "sys_rd")
	    val field =
		X.Apply {oper= Op.Record_extract,
			 args= v[record, X.Value (A.Symbol (Symbol.name sym))]}
	in
	    if Option.isSome (Property.getEPIndex props) then
		X.Apply {oper= Op.Array_extract,
			 args= v[field, X.Value (A.Variable "modelid")]}
	    else field
	end

in
fun fun_to_spil to_spil (funtype, exps) =
    case funtype
     of Fun.BUILTIN oper =>
	let 
	    val spil_oper =
		case oper
		 of MF.ADD => Op.Float_add
		  | MF.SUB => Op.Float_sub
		  | MF.NEG => Op.Float_neg
		  | MF.MUL => Op.Float_mul
		  | MF.DIVIDE => Op.Float_div
		  | MF.GT => Op.Float_gt
		  | MF.LT => Op.Float_lt
		  | MF.GE => Op.Float_ge
		  | MF.LE => Op.Float_le
		  | MF.EQ => Op.Float_eq
		  | MF.NEQ => Op.Float_ne
		  | MF.EXP => Op.Math_exp
		  | MF.POW => Op.Math_pow
		  | MF.SIN => Op.Math_sin
		  | MF.COS => Op.Math_cos
		  | MF.TAN => Op.Math_tan
		  | MF.CSC => Op.Math_csc
		  | MF.SEC => Op.Math_sec
		  | MF.COT => Op.Math_cot
		  | MF.ASIN => Op.Math_asin
		  | MF.ACOS => Op.Math_acos
		  | MF.ATAN => Op.Math_atan
		  | MF.ATAN2 => Op.Math_atan2
		  | MF.ACSC => Op.Math_acsc
		  | MF.ASEC => Op.Math_asec
		  | MF.ACOT => Op.Math_acot
		  | MF.SINH => Op.Math_sinh
		  | MF.COSH => Op.Math_cosh
		  | MF.TANH => Op.Math_tanh
		  | MF.CSCH => Op.Math_csch
		  | MF.SECH => Op.Math_sech
		  | MF.COTH => Op.Math_coth
		  | MF.ASINH => Op.Math_asinh
		  | MF.ACOSH => Op.Math_acosh
		  | MF.ATANH => Op.Math_atanh
		  | MF.ACSCH => Op.Math_acsch
		  | MF.ASECH => Op.Math_asech
		  | MF.ACOTH => Op.Math_acoth
		  | MF.IF => Op.Sim_if
		  | _ => Op.Sim_bug
	in
	    X.Apply {oper= spil_oper, args= v(map to_spil exps)}
	end
      | Fun.INST _ =>
	X.Value (A.Literal (Const "inst"))
      | Fun.OUTPUT _ =>
	X.Value (A.Literal (Const "output"))

val rec term_to_spil =
 fn Exp.INT z => X.Value (A.Literal (Int z))
  | Exp.REAL r => X.Value (A.Literal (Real r))
  | Exp.NAN => X.Value (A.Literal Nan)
  | Exp.INFINITY => X.Value (A.Literal Infinity)
  | Exp.BOOL b => X.Value (A.Literal (Bool b))
  | Exp.STRING s => X.Value (A.Literal (String s))
  | Exp.RATIONAL (num, den) => X.Apply {oper= Op.Rational_rational, args= v[X.Value (A.Literal (Int num)), X.Value (A.Literal (Int den))]}
  | Exp.COMPLEX (real, imag) => X.Apply {oper= Op.Complex_complex, args= v[term_to_spil real, term_to_spil imag]}
  | Exp.TUPLE terms => 
    let
	val (fields, elems, _) =
	    List.foldl
		(fn (term, (fs, es, n)) =>
		    ((X.Value (A.Symbol ("t"^(Int.toString n)))) :: fs,
		     (term_to_spil term) :: es,
		     1+n)
		)
		(nil,nil,0)
		terms
    in
	X.Apply {oper= Op.Record_record, 
		 args= v[X.Apply {oper= Op.Array_array, args= v(List.rev fields)}, 
			 X.Apply {oper= Op.Array_array, args= v(List.rev elems)}]}
    end
  | Exp.RANGE {low, high, step} => X.Apply {oper= Op.Range_range, args= v(map term_to_spil [low,high,step])}
  | Exp.RANDOM Exp.UNIFORM => X.Apply {oper= Op.Random_uniform, args= v[]}
  | Exp.RANDOM Exp.NORMAL => X.Apply {oper= Op.Random_normal, args= v[]}
  | Exp.SYMBOL (sym, props) => symbol_to_expr (sym, props)
  | term =>
    DynException.stdException (("Can't write out term '"^(e2s (Exp.TERM term))^"'"),"CWriterUtil.term_to_spil", Logger.INTERNAL)

fun container_to_spil to_spil =
 fn Exp.ARRAY array => 
    X.Apply {oper= Op.Array_array, args= v(map to_spil (Container.arrayToList array))}
  | Exp.MATRIX matrix =>
    let
	val (rows,cols) = Matrix.size matrix
    in
	case ! matrix
	 of Matrix.DENSE _ => 
	    X.Apply {oper= Op.Matrix_dense, 
		     args= v[X.Value (A.Literal (Int rows)), X.Value (A.Literal (Int cols)), 
			     X.Apply {oper= Op.Array_array, args= v(map to_spil (Matrix.getElements matrix))}]}
	  | Matrix.BANDED {upperbw, lowerbw, ...} =>
	    let
		val m' = Matrix.fromRows (Exp.calculus())  (Matrix.toPaddedBands matrix)
		val _ = Matrix.transpose m'
	    in
		X.Apply {oper= Op.Matrix_banded,
			 args= v[X.Value (A.Literal (Int rows)), X.Value (A.Literal (Int cols)), 
				 X.Value (A.Literal (Int upperbw)), X.Value (A.Literal (Int lowerbw)), 
				 X.Apply {oper= Op.Array_array, args= v(map to_spil (Matrix.getElements m'))}]}
	    end
    end
  | Exp.EXPLIST list =>
    DynException.stdException ("Cannot write EXPLIST expressions", "CWriterUtil.container_to_spil", Logger.INTERNAL)
  | Exp.ASSOC assoc =>
    DynException.stdException ("Cannot write ASSOC expressions", "CWriterUtil.container_to_spil", Logger.INTERNAL)

val rec exp_to_spil =
 fn Exp.FUN (str, exps) => fun_to_spil exp_to_spil (str, exps)
  | Exp.TERM term => term_to_spil term
  | Exp.CONTAINER con => container_to_spil exp_to_spil con
  | exp as Exp.META _ => 
    DynException.stdException (("Cannot write META expressions. ["^(ExpPrinter.exp2str exp)^"]"), "CWriterUtil.exp_to_spil", Logger.INTERNAL)

end

local
fun exp2c_str (Exp.FUN (str : Fun.funtype, exps)) =
    let
	fun useParen (Exp.FUN (str' : Fun.funtype, _)) = 
	    let
		val {precedence=prec,associative=assoc,...} = FunProcess.fun2props str
		val {precedence=prec',...} = FunProcess.fun2props str'
	    in
		(prec = prec' andalso (not (FunProcess.equal (str, str')) orelse (not assoc))) orelse prec < prec'
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

	fun notation2c_str (v, MathFunctionProperties.INFIX) = 
	    String.concatWith (" "^v^" ") (map (fn(e)=>addParen ((exp2c_str e),e)) exps)
	  | notation2c_str (v, MathFunctionProperties.PREFIX) = 
	    v ^ "(" ^ (String.concatWith ", " (map (fn(e)=>addParen((exp2c_str e,e))) exps)) ^ ")"
	  | notation2c_str (v, MathFunctionProperties.POSTFIX) = 
	    (String.concatWith " " (map (fn(e)=> addParen ((exp2c_str e),e)) exps)) ^ " " ^ v
	  | notation2c_str (v, MathFunctionProperties.MATCH) = 
	    foldl (fn((exp, index),str')=>replaceIndex str' (index+1,exp)) v (Util.addCount exps)

    in
	notation2c_str (FunProps.fun2cstrnotation str)
    end
  | exp2c_str (Exp.TERM term) = term2c_str term
  | exp2c_str (Exp.CONTAINER c) =
    let
	fun array2str a = "{" ^ (String.concatWith ", " (map exp2c_str (Container.arrayToList a))) ^ "}"
	fun matrix2str m = 
	    case !m of
		Matrix.DENSE _ =>
		let
		    (*val _ = print ("dense matrix -> ")
		    val _ = Matrix.print m*)
		    val arrays = Matrix.toRows m
		in
		    "{" ^ "\n" ^
		    (String.concatWith ",\n" (map array2str arrays)) ^ "\n" ^
		    "}"
		end
	      | Matrix.BANDED _ =>
		let
		    val bands = Matrix.toPaddedBands m
		    val m' = Matrix.fromRows (Exp.calculus()) bands
		   (* val _ = print ("matrix bands -> ")
		    val _ = Matrix.print m'*)
		    val _ = Matrix.transpose m'
		(*val _ = print ("matrix bands (transposed) -> ")
		    val _ = Matrix.print m'*)
		in
		    matrix2str m'
		end		
    in
	case c of
	    Exp.EXPLIST l => DynException.stdException ("Cannot write EXPLIST expressions", "CWriter.exp2c_str", Logger.INTERNAL)
	  | Exp.ARRAY a => array2str a
	  | Exp.ASSOC t => 
	    DynException.stdException ("Cannot write ASSOC containers.", "CWriter.exp2c_str", Logger.INTERNAL)
	  | Exp.MATRIX m => matrix2str m
    end
  | exp2c_str (e as Exp.META _) = 
    DynException.stdException (("Cannot write META expressions. ["^(ExpPrinter.exp2str e)^"]"), "CWriter.exp2c_str", Logger.INTERNAL)
    

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
  | term2c_str (Exp.RANDOM Exp.UNIFORM) = "UNIFORM_RANDOM(PARALLEL_MODELS, modelid)"
  | term2c_str (Exp.RANDOM Exp.NORMAL) = "NORMAL_RANDOM(PARALLEL_MODELS, modelid)"
  | term2c_str Exp.DONTCARE = "_"
  | term2c_str term =
    DynException.stdException (("Can't write out term '"^(e2s (Exp.TERM term))^"'"),"CWriter.exp2c_str", Logger.INTERNAL)
in
fun exp2c_str_helper exp = exp2c_str exp
    handle e => DynException.checkpoint ("CWriterUtil.exp2c_str ["^(e2s exp)^"]") e
end
val exp2c_str = exp2c_str_helper

fun iter2range class (itersym, itertype) = 
    (*case List.find (fn{name,...}=> name=itersym) (#iterators class) of
	SOME v => v
      | NONE => *)DynException.stdException(("Iterator '"^(Symbol.name itersym)^
					   "' not found in class '"^
					   (Symbol.name (#name class))^"'"), 
					  "CWriterUtil.iter2range",
					  Logger.INTERNAL)
						

fun expandprogs2parallelfor (class: DOF.class) (exp, progs) = 
    let
	val size = ExpProcess.exp2size exp
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
fun outputs2uniqueoutputsymbols (outputs:DOF.Output.output list) = 
    let
	val exps = Util.flatmap (fn output => (DOF.Output.condition output) :: (DOF.Output.contents output)) outputs
	val all_symbols = Util.flatmap ExpProcess.exp2termsymbols exps
	val sym_mapping = map (fn(term)=>(term, (Term.processInternalName o Term.sym2curname) term)) all_symbols
	fun cmp_fun ((_,s1),(_,s2))= s1 = s2
	val unique_symbols = Util.uniquify_by_fun cmp_fun sym_mapping
    in
	unique_symbols
    end
fun class2uniqueoutputsymbols (class:DOF.class) = 
    let
	val outputs = !(#outputs class)
	val exps = Util.flatmap (fn output => (DOF.Output.condition output) :: (DOF.Output.contents output)) outputs
	val all_symbols = Util.flatmap ExpProcess.exp2termsymbols exps
	val sym_mapping = map (fn(term)=>(term, Term.sym2curname term)) all_symbols
	fun cmp_fun ((_,s1),(_,s2))= s1 = s2
	val unique_symbols = Util.uniquify_by_fun cmp_fun sym_mapping
    in
	unique_symbols
    end

end
