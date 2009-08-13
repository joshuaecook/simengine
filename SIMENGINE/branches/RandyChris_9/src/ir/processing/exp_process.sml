structure ExpProcess =
struct

val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val log = Util.log
val exp2str = ExpPrinter.exp2str
val e2s = ExpPrinter.exp2str
open Printer

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
    
fun exp2termsymbols (Exp.FUN (_, exps)) = 
    List.concat (map exp2termsymbols exps)
  | exp2termsymbols (Exp.TERM (s as Exp.SYMBOL _)) = [s]
  | exp2termsymbols (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2termsymbols (Exp.TERM t)) terms)
  | exp2termsymbols (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2termsymbols (Exp.TERM t)) terms)
  | exp2termsymbols (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2termsymbols (Exp.TERM t1)) @ (exp2termsymbols (Exp.TERM t2))
  | exp2termsymbols _ = []
    

fun exp2fun_names (Exp.FUN (funtype, exps)) = (FunProcess.fun2name funtype)::(List.concat (map exp2fun_names exps))
  | exp2fun_names _ = []

val uniqueid = ref 0

fun uniq(sym) =
    (uniqueid := (!uniqueid)+1;
     (Symbol.symbol ((Symbol.name sym) ^ "_U" ^ (i2s (!uniqueid)))))

fun exp2term (Exp.TERM t) = t
  | exp2term _ = Exp.NAN

(*fun sort_explist *)

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
     (app (fn(e)=>log (e2s e)) exps);
     log ("--------------------------------------"))

fun eq2str (lhs, rhs) =
    ExpBuild.equals (lhs, rhs)



(* general processing of expression *)

fun error_no_return exp text = 
    (Logger.log_internalerror (Printer.$("Error when processing '"^(e2s exp)^"': "^(text)));
     DynException.setErrored())

fun error exp text = (error_no_return exp text; Exp.null)

fun isFun exp = 
    case exp of
	Exp.FUN _ => true
      | Exp.TERM _ => false

fun isTerm exp = 
    case exp of
	Exp.FUN _ => false
      | Exp.TERM _ => true

fun isSymbol exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL _) => true
      | _ => false

(*fun isEquation exp =
    case exp of
	Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, rhs]) => true
      | _ => false*)

fun isEquation exp = 
    Match.equiv (exp,
		 ExpBuild.equals (Match.any "a",
				  Match.any "b"))

fun isInstanceEq exp = 
    (case exp of 
	 Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, Exp.FUN (Fun.INST {props,...}, _)]) => 
	 not (Fun.isInline props)
       | _ => false)
    handle e => DynException.checkpoint "ExpProcess.isInstanceEq" e
	     
fun lhs exp = 
    case exp of 
	Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [l, r]) => l
      | _ => error exp "No left hand side found"

fun rhs exp = 
    case exp of 
	Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [l, r]) => r
      | _ => error exp "No right hand side found"

fun deconstructInst exp = 
    let
	val empty_return = {classname=Symbol.symbol "NULL", 
			    instname=Symbol.symbol "NULL", 
			    props=Fun.emptyinstprops, 
			    inpargs=[], 
			    outargs=[]}
    in
	if isInstanceEq exp then
	    case exp of 
		Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE outargs), Exp.FUN (Fun.INST {classname, instname, props}, inpargs)]) => 
		{classname=classname, instname=instname, props=props, inpargs=inpargs, outargs=outargs}
	      | _ => (error_no_return exp "Malformed instance equation"; empty_return)
	else
	    (error_no_return exp "Not an instance equation"; empty_return)
    end

fun instSpatialSize inst =
    if isInstanceEq inst then
	let
	    val {props,...} = deconstructInst inst
	in
	    case Fun.getDim props  
	     of SOME l => Util.prod l
	      | NONE => 1
	end	    
    else
	DynException.stdException(("Passed exp '"^(e2s inst)^"' that is not an instance"), "Inst.instSpatialSize", Logger.INTERNAL)

fun instOrigClassName inst = 
    let
	val {classname, instname, props, inpargs, outargs} = deconstructInst inst
    in
	case Fun.getRealClassName props 
	 of SOME v => v
	  | NONE => classname
    end

fun instOrigInstName inst = 
    let
	val {classname, instname, props, inpargs, outargs} = deconstructInst inst
    in
	case Fun.getRealInstName props 
	 of SOME v => v
	  | NONE => instname
    end

(* the lhs is something of the form of x'[t] *)
fun isFirstOrderDifferentialTerm exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val derivative = Property.getDerivative props
	    val continuous_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									   DOF.DISCRETE _ => false
									 | DOF.CONTINUOUS _ => true) (CurrentModel.iterators())
	in
	    case derivative of
		SOME (order, [iterator]) =>  order = 1 andalso List.exists (fn(sym, _)=> iterator = sym) continuous_iterators
	      | _ => false
	end
      | _ => false

fun isFirstOrderDifferentialEq exp =
    isEquation exp andalso
    isFirstOrderDifferentialTerm (lhs exp)

(* now difference equations of the form x[n+1] *)
fun isNextVarDifferenceTerm exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.DISCRETE _ => true
								       | DOF.CONTINUOUS _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 1)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | _ => false
	end
      | _ => false

(* difference equations must have x[n+1] on the left hand side *)
fun isDifferenceEq exp =
    isEquation exp andalso
    isNextVarDifferenceTerm (lhs exp)

(* difference terms of the form x[n] *)
fun isCurVarDifferenceTerm exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.DISCRETE _ => true
								       | DOF.CONTINUOUS _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 0)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | _ => false
	end
      | _ => false

(* anything of the form x[0] *)
fun isInitialConditionTerm exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val all_iterators = CurrentModel.iterators()
	in
	    case iterators of
		SOME ((itersym, Iterator.ABSOLUTE 0)::rest) => List.exists (fn(sym,_)=>sym=itersym) all_iterators
	      | _ => false
	end
      | _ => false

fun isInitialConditionEq exp =
    isEquation exp andalso
    isInitialConditionTerm (lhs exp)

(* look for state equations of a particular iterator type *)
fun isStateTermOfIter (iter as (name, DOF.CONTINUOUS _)) exp =
    (case exp of
	 Exp.TERM (Exp.SYMBOL (_, props)) =>
	 let
	     val derivative = Property.getDerivative props
	 in
	     case derivative of
		 SOME (order, [iterator]) => order = 1 andalso iterator = name
	       | _ => false
	 end
       | _ => false)
  | isStateTermOfIter (iter as (name, DOF.DISCRETE _)) exp =
    (case exp of
	 Exp.TERM (Exp.SYMBOL (_, props)) =>
	 let
	     val iterators = Property.getIterator props
	 in
	     case iterators of
		 SOME ((iterator, Iterator.RELATIVE 1)::rest) => iterator = name
	       | _ => false
	 end
       | _ => false)

fun isStateEqOfIter iter exp =
    isEquation exp andalso
    isStateTermOfIter iter (lhs exp)

fun isStateTerm exp = 
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(iter)=>isStateTermOfIter iter exp) iterators
    end
fun isStateEq exp =
    isEquation exp andalso
    isStateTerm (lhs exp)

(* intermediate equations *)
fun isIntermediateTerm exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val derivative = Property.getDerivative props
	    val iterators = Property.getIterator props
	    val all_iterators = CurrentModel.iterators()
	in
	    case (derivative, iterators) of
		(SOME _, _) => false
	      | (_, SOME ((itersym, Iterator.ABSOLUTE _)::rest)) => not (List.exists (fn(sym,_)=>sym=itersym) all_iterators)
	      | (_, SOME ((itersym, Iterator.RELATIVE 1)::rest)) => not (List.exists (fn(sym,_)=>sym=itersym) all_iterators)
	      | (_, _) => true
	end
      | _ => false

fun isIntermediateEq exp =
    isEquation exp andalso
    isIntermediateTerm (lhs exp)

fun isNonSupportedEq exp =
    if isEquation exp then
	if isInitialConditionEq exp orelse
	   isFirstOrderDifferentialEq exp orelse
	   isIntermediateEq exp orelse
	   isInstanceEq exp then
	    true (* all supported *)
	else
	    (error_no_return exp "Not a supported equation type"; false)
    else
	(error_no_return exp "Not an equation"; true)

fun doesTermHaveIterator iter exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	(case (Property.getIterator props) of
	     SOME iters => List.exists (fn(sym,_)=>sym=iter) iters
	   | NONE => false)
      | _ => false

fun doesEqHaveIterator iter exp =
    let 
	val result =
	    isEquation exp andalso
	    doesTermHaveIterator iter (lhs exp)
    in
	result
    end

fun exp2size exp = 
    let
	fun combineSizes (size1, size2) = 
	    if (size1 = size2) then size1
	    else if (size1 = 1) then size2
	    else if (size2 = 1) then size1
	    else
		(error_no_return Exp.null ("Arguments have mismatched sizes ("^(i2s size1)^","^(i2s size2)^")"); 1)
	
	val size = case exp of
		       Exp.TERM t => 
		       if Term.isNumeric t then
			   Term.termCount t
		       else if Term.isScalar t andalso Term.isSymbol t then
			   Term.symbolSpatialSize t
		       else 
			   1 (* out of default - need to do something better here *)
		     | Exp.FUN (f, args) => foldl combineSizes 1 (map exp2size args)
    in
	size
    end

fun getLHSSymbol exp = 
    case exp2term (lhs exp) of
	Exp.SYMBOL s => Exp.SYMBOL s
      | _ => (error_no_return exp ("No valid symbol found on LHS");
	      Exp.SYMBOL (Symbol.symbol "???", Property.default_symbolproperty))



fun countTerms exp =
    length (Match.findRecursive (Match.anyterm "a", exp))

fun countFuns exp =
    length (Match.findRecursive (Match.anyfun "a", exp))

fun assignIteratorToSymbol (sym, itertype) exp =
    let
	val iter = TermProcess.symbol2temporaliterator (exp2term exp)
    in
	case iter of
	    SOME (sym', _) => if sym = sym' then
				 (* already assigned *)
				 exp
			     else
				 DynException.stdException(
				 ("Can't assign iterator '"^(Symbol.name sym)^"' over previously defined iterator on expression '"^(e2s exp)^"'"),
				 "ExpProcess.assignIteratorToSymbol",
				 Logger.INTERNAL)
	  | NONE => 
	    let
		val (exp_sym, props, iterators) = case (exp2term exp) of
						      Exp.SYMBOL (exp_sym, props) => 
						      (exp_sym, props, Property.getIterator props)
						    | _ => DynException.stdException(("Unexpected non symbol '"^(e2s exp)^"'"),
										     "ExpProcess.assignIteratorToSymbol", 
										     Logger.INTERNAL)

		val iterators' = case iterators of
				     SOME iters => (sym, Iterator.RELATIVE 0)::iters (* prepend the iterator in this case *)
				   | NONE => [(sym, Iterator.RELATIVE 0)] (* create a new list *)

	    in
		Exp.TERM (Exp.SYMBOL (exp_sym, Property.setIterator props iterators'))
	    end
    end

fun assignCorrectScopeOnSymbol exp =
    case exp of
	Exp.TERM (s as (Exp.SYMBOL (sym, props))) => 
	let
	    val iter = TermProcess.symbol2temporaliterator s
	in
	    case iter of
		SOME (iter_sym, iter_type) => 
		if isFirstOrderDifferentialTerm exp then
		    case exp of 
			Exp.TERM (s as (Exp.SYMBOL (sym, props))) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ("wr_" ^ (Symbol.name iter_sym))))))
		      | _ => DynException.stdException("Unexpected expression", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		else if isNextVarDifferenceTerm exp then
		    case exp of 
			Exp.TERM (s as (Exp.SYMBOL (sym, props))) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ("wr_" ^ (Symbol.name iter_sym))))))
		      | _ => DynException.stdException("Unexpected expression", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		else if isCurVarDifferenceTerm exp then
		    case exp of 
			Exp.TERM (s as (Exp.SYMBOL (sym, props))) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSTATE (Symbol.symbol ("rd_" ^ (Symbol.name iter_sym))))))
		      | _ => DynException.stdException("Unexpected expression", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		else if isIntermediateTerm exp then
		    case exp of 
			Exp.TERM (s as (Exp.SYMBOL (sym, props))) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSTATE (Symbol.symbol ("rd_" ^ (Symbol.name iter_sym))))))
		      | _ => DynException.stdException("Unexpected expression", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		else if isInitialConditionTerm exp then
		    exp (* this doesn't really apply here ... *)
		else
		    (Logger.log_error($("Unexpected expression '"^(e2s exp)^"' found when assigning correct scope - unknown expression type"));
		     DynException.setErrored();
		     exp)
	      | NONE => (*(Logger.log_error($("Unexpected expression '"^(e2s exp)^"' found when assigning correct scope - no temporal iterator"));
			 DynException.setErrored();*)
		exp
	end
      | _ => (Logger.log_error($("Unexpected expression '"^(e2s exp)^"' found when assigning correct scope - not a symbol"));
	      DynException.setErrored();
	      exp)

fun assignCorrectScope states exp =
    if isSymbol exp then
	let
	    val sym = Util.hd (exp2symbols exp)
	in
	    if List.exists (fn(sym')=>(sym=sym')) states then
		assignCorrectScopeOnSymbol exp
	    else
		exp
	end
    else
	(Match.head exp) (map (assignCorrectScope states) (Match.level exp))
	
end
