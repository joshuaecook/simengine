signature EXPPROCESS =
sig

(* Commonly used functions *)
val exp2term : Exp.exp -> Exp.term (* must be of term expression, otherwise this fails *)
val term2exp : Exp.term -> Exp.exp

(* Basic extraction functions *)
val lhs : Exp.exp -> Exp.exp
val rhs : Exp.exp -> Exp.exp
val deconstructInst : Exp.exp -> {classname: Symbol.symbol,
				  instname: Symbol.symbol,
				  props: InstProps.instproperties,
				  inpargs: Exp.exp list,
				  outargs: Exp.term list}

(* Classification functions *)
val isTerm : Exp.exp -> bool
val isStateEq : Exp.exp -> bool
val isInitialConditionEq : Exp.exp -> bool
val isInstanceEq : Exp.exp -> bool
val isFirstOrderDifferentialEq : Exp.exp -> bool
val isDifferenceEq : Exp.exp -> bool
val isIntermediateEq : Exp.exp -> bool
val isPPEq : Exp.exp -> bool
val isUpdateEq : Exp.exp -> bool

(* Iterator related functions *)
val doesTermHaveIterator : Symbol.symbol -> Exp.exp -> bool (* Looks at the symbol in the expression, returns if the iterator is assigned to that symbol *)
val doesEqHaveIterator : Symbol.symbol -> Exp.exp -> bool (* Looks at the symbol on the lhs, returns if the iterator is assigned to that symbol *)
val isStateEqOfIter : DOF.systemiterator -> Exp.exp -> bool (* like doesEqHaveIterator, put also ensures that eq is a state equation *)
val prependIteratorToSymbol : Iterator.iterator -> Exp.exp -> Exp.exp
val appendIteratorToSymbol : Iterator.iterator -> Exp.exp -> Exp.exp
val exp2spatialiterators : Exp.exp -> Iterator.iterator list

(* Rewriter related functions *)
val equation2rewrite : Exp.exp -> Rewrite.rewrite (* if a == b, then make a -> b *)

(* Expression manipulation functions - get/set differing properties *)
val renameSym : (Symbol.symbol * Symbol.symbol) -> Exp.exp -> Exp.exp (* Traverse through the expression, changing symbol names from the first name to the second name *)
val renameInst : (Symbol.symbol * Symbol.symbol) -> Exp.exp -> Exp.exp (* Traverse through the expression, updating instance names *)
val exp2size : DOF.classiterator list -> Exp.exp -> int
val enableEPIndex : bool -> (Symbol.symbol list) -> Exp.exp -> Exp.exp (* Add an EP index property when running an embarrassingly parallel simulation *)
val assignCorrectScopeOnSymbol : Exp.exp -> Exp.exp (* addes the read and write state attributes based on found derivatives or differential terms *)
val assignToOutputBuffer : Exp.exp -> Exp.exp (* sets an output buffer flag - only run in the code generator which reformulates the expression for generating the output logger *)
val instOrigClassName : Exp.exp -> Symbol.symbol (* returns original class name, assumes that the expression is an instance equation *)
val instOrigInstName : Exp.exp -> Symbol.symbol (* returns original instance name, assumes that the expression is an instance equation *)
val instSpatialSize : Exp.exp -> int (* returns the assigned dimension of an instance *)

(* Extract symbols from expressions in different forms *)
val exp2symbol_names : Exp.exp -> string list
val exp2symbols : Exp.exp -> Symbol.symbol list
val exp2termsymbols : Exp.exp -> Exp.term list
val getLHSTerm : Exp.exp -> Exp.term (* assuming exp is an equation, pulls out the symbol as a term from the lhs *)
val getLHSTerms : Exp.exp -> Exp.term list (* assuming exp is an equation, pulls out the symbol as a term from the lhs *)
val getLHSSymbol : Exp.exp -> Symbol.symbol (* assuming exp is an equation, pulls out the symbol as just a symbol name *)
val getLHSSymbols : Exp.exp -> Symbol.symbol list (* Grab them as a list, since the lhs can be a tuple *)

(* pull out all the function names that are present *)
val exp2fun_names : Exp.exp -> Symbol.symbol list

(* useful helper functions *)
val uniq : Symbol.symbol -> Symbol.symbol (* given a sym, create a unique sym name *)

end
structure ExpProcess : EXPPROCESS =
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
fun term2exp t = Exp.TERM t

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

fun renameInst (orig_sym, new_sym) exp =
    case exp of
	Exp.FUN (Fun.INST (inst as {classname,instname,props}), args) =>
	if classname=orig_sym then
	    Exp.FUN (Fun.INST {classname=new_sym,instname=instname,props=props}, 
		     map (renameInst (orig_sym, new_sym)) args)
	else
	    Exp.FUN (Fun.INST inst, map (renameInst (orig_sym, new_sym)) args)
      | Exp.FUN (f, args) => Exp.FUN (f, map (renameInst (orig_sym, new_sym)) args)
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

val raiseExceptions = true

fun error_no_return exp text = 
    (Logger.log_internalerror (Printer.$("Error when processing '"^(e2s exp)^"': "^(text)));
     DynException.setErrored();
     if raiseExceptions then
	 DynException.stdException("Expression error", "ExpProcess.error_no_return", Logger.INTERNAL)
     else
	 ())

fun error exp text = (error_no_return exp text; 
		      if raiseExceptions then
			  DynException.stdException("Expression error", "ExpProcess.error", Logger.INTERNAL)
		      else
			  Exp.null)

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
    ExpEquality.equiv (exp,
		       ExpBuild.equals (Match.any "a",
					Match.any "b"))

fun isInstanceEq exp = 
    (case exp of 
	 Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, Exp.FUN (Fun.INST {props,...}, _)]) => 
	 not (InstProps.isInline props)
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
			    props=InstProps.emptyinstprops, 
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
	    case InstProps.getDim props  
	     of SOME l => Util.prod l
	      | NONE => 1
	end	    
    else
	DynException.stdException(("Passed exp '"^(e2s inst)^"' that is not an instance"), "Inst.instSpatialSize", Logger.INTERNAL)

fun instOrigClassName inst = 
    let
	val {classname, instname, props, inpargs, outargs} = deconstructInst inst
    in
	case InstProps.getRealClassName props 
	 of SOME v => v
	  | NONE => classname
    end

fun instOrigInstName inst = 
    let
	val {classname, instname, props, inpargs, outargs} = deconstructInst inst
    in
	case InstProps.getRealInstName props 
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
									   DOF.CONTINUOUS _ => true
									 | _ => false) (CurrentModel.iterators())
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
								       | _ => false) (CurrentModel.iterators())
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

fun isNextPPTerm exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.POSTPROCESS _ => true
								       | _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 1)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | _ => false
	end
      | _ => false

fun isPPEq exp =
    isEquation exp andalso
    isNextPPTerm (lhs exp)

fun isNextUpdateTerm exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.UPDATE _ => true
								       | _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 1)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | _ => false
	end
      | _ => false

fun isUpdateEq exp =
    isEquation exp andalso
    isNextUpdateTerm (lhs exp)
    
(* difference terms of the form x[n] *)
fun isCurVarDifferenceTerm exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.DISCRETE _ => true
								       | _ => false) (CurrentModel.iterators())
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
		 SOME (order, [iterator]) => order >= 1 andalso iterator = name
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
  | isStateTermOfIter (iter as (name, DOF.POSTPROCESS _)) exp =
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
  | isStateTermOfIter (iter as (name, DOF.UPDATE _)) exp =
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
	    not (isNextPPTerm exp) andalso not (isNextUpdateTerm exp) andalso
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

fun equation2rewrite exp = 
    if isEquation exp then
	let
	    val lhs = lhs exp
	    val rhs = rhs exp
	in
	    {find=lhs,
	     test=NONE,
	     replace=Rewrite.RULE rhs}
	end
    else
	DynException.stdException(("Can't write a rewrite around an expression '"^(e2s exp)^"' that is not already an equation"),
				  "ExpProcess.equation2rewrite",
				  Logger.INTERNAL)

fun exp2size iterator_list exp : int = 
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
			   let
			       val spatial_iterators = TermProcess.symbol2spatialiterators t
			       (*val _ = Util.log ("Returned spatial iterators: " ^ (Util.symlist2s (map #1 spatial_iterators)))*)
			       fun lookupsize itersym = 
				   case List.find (fn{name,...}=>name=itersym) iterator_list of
					    SOME {name, low, high, step} => Real.ceil((high-low)/step + 1.0)
					  | _ => DynException.stdException(
						 ("Iterator '"^(Symbol.name itersym)^"' not defined for exp '"^(e2s exp)^"'"), 
						 "ExpProcess.exp2size.lookupsize", 
						 Logger.INTERNAL)

			       fun iterator2size (itersym, itertype) = 
				   case itertype of
				       Iterator.ABSOLUTE _ => 1
				     | Iterator.RELATIVE _ => lookupsize itersym
				     | Iterator.RANGE (a, b) => b-a + 1 (* inclusive *)
				     | Iterator.LIST l => List.length l
				     | Iterator.ALL => lookupsize itersym				       
				       
			   in
			       (*Term.symbolSpatialSize t*)
			       Util.prod (map iterator2size spatial_iterators)
			   end
		       else 
			       1 (* out of default - need to do something better here *)
		     | Exp.FUN (Fun.BUILTIN f, args) => 
		       let
			   val codomain = #codomain (FunProps.op2props f)
		       in
			   Util.prod(codomain (map (fn(a) => [exp2size iterator_list a]) args))
(*		       foldl combineSizes 1 (map (exp2size iterator_list) args)*)
		       end
		     | Exp.FUN (Fun.INST _, args) => 1 (*TODO: ???? *)

    in
	size
    end


fun exp2spatialiterators exp = 
    if isEquation exp then
	exp2spatialiterators (lhs exp)
    else
	case exp of 
	    Exp.TERM (term as (Exp.SYMBOL (sym, props))) => TermProcess.symbol2spatialiterators term
	  | Exp.TERM (term as (Exp.TUPLE (termlist))) => Util.uniquify_by_fun (fn((a,_),(a',_))=>a=a') (StdFun.flatmap (exp2spatialiterators o Exp.TERM) termlist)
	  | _ => []

fun symterm2symterm term = 
    case term of 
	Exp.SYMBOL s => Exp.SYMBOL s
      | _ => (error_no_return (term2exp term) ("No valid symbol found on term");
	      Exp.SYMBOL (Symbol.symbol "???", Property.default_symbolproperty))

fun getLHSTerm exp = 
    symterm2symterm (exp2term (lhs exp))

fun getLHSTerms exp = 
    case exp2term (lhs exp) of
	Exp.SYMBOL s => [Exp.SYMBOL s]
      | Exp.TUPLE terms => map symterm2symterm terms
      | _ => (error_no_return exp ("No valid symbols found on LHS");
	      [Exp.SYMBOL (Symbol.symbol "???", Property.default_symbolproperty)])


fun term2sym term = 
    case term of 
	Exp.SYMBOL (sym,_) => sym
      | _ => (error_no_return (term2exp term) ("No valid symbol found on term");
	      Symbol.symbol "???")

fun getLHSSymbol exp = 
    term2sym(exp2term (lhs exp))
	
fun getLHSSymbols exp = 
    case exp2term (lhs exp) of
	Exp.SYMBOL (sym,_) => [sym]
      | Exp.TUPLE terms => map term2sym terms
      | _ => (error_no_return exp ("No valid symbols found on LHS");
	      [Symbol.symbol "???"])

fun countTerms exp =
    length (Match.findRecursive (Match.anyterm "a", exp))

fun countFuns exp =
    length (Match.findRecursive (Match.anyfun "a", exp))

datatype p = PREPEND | APPEND

fun assignIteratorToSymbol (sym, itertype, p) exp =
    let
	val iter = TermProcess.symbol2temporaliterator (exp2term exp)
	val spatial_iterators = TermProcess.symbol2spatialiterators (exp2term exp)
	val temporal_iterators = CurrentModel.iterators()
	val isTemporal = List.exists (fn(sym',_)=>sym=sym') temporal_iterators

	val isAssigned = if isTemporal then
			     case iter of
				 SOME (sym', _) => if sym = sym' then
					      (* already assigned *)
						       true
						   else
						       (*DynException.stdException(
						       ("Can't assign iterator '"^(Symbol.name sym)^"' over previously defined iterator on expression '"^(e2s exp)^"'"),
						       "ExpProcess.assignIteratorToSymbol",
						       Logger.INTERNAL)*)true
			       | NONE => false
			 else
			     List.exists (fn(sym',_)=>sym=sym') (spatial_iterators)

    in
	if isAssigned then
	    exp
	else
	    let
		val (exp_sym, props, iterators) = case (exp2term exp) of
						      Exp.SYMBOL (exp_sym, props) => 
						      (exp_sym, props, Property.getIterator props)
						    | _ => DynException.stdException(("Unexpected non symbol '"^(e2s exp)^"'"),
										     "ExpProcess.assignIteratorToSymbol", 
										     Logger.INTERNAL)

		val iterators' = 
		    case p of
			PREPEND => (case iterators of
					SOME iters => (sym, Iterator.RELATIVE 0)::iters (* prepend the iterator in this case *)
				      | NONE => [(sym, Iterator.RELATIVE 0)]) (* create a new list *)
		      | APPEND => (case iterators of
				       SOME iters => iters @ [(sym, Iterator.RELATIVE 0)] (* prepend the iterator in this case *)
				     | NONE => [(sym, Iterator.RELATIVE 0)]) (* create a new list *)

	    in
		Exp.TERM (Exp.SYMBOL (exp_sym, Property.setIterator props iterators'))
	    end
    end
    handle e => DynException.checkpoint "ExpProcess.assignIteratorToSymbol" e
 
fun prependIteratorToSymbol (sym, itertype) exp = assignIteratorToSymbol (sym, itertype, PREPEND) exp
fun appendIteratorToSymbol (sym, itertype) exp = assignIteratorToSymbol (sym, itertype, APPEND) exp

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
		else if isNextPPTerm exp then
		    case exp of 
			Exp.TERM (s as (Exp.SYMBOL (sym, props))) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ("wr_" ^ (Symbol.name iter_sym))))))
		      | _ => DynException.stdException("Unexpected expression", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		else if isNextUpdateTerm exp then
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
		    error exp "Unexpected expression found when assign correct scope - unknown expression type"
	      | NONE => (*(Logger.log_error($("Unexpected expression '"^(e2s exp)^"' found when assigning correct scope - no temporal iterator"));
			 DynException.setErrored();*)
		exp
	end
      | _ => error exp "Unexpected expression found when assign correct scope - not a symbol"

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
	
(* function to add EP_index property to all state symbols *)
fun enableEPIndex is_top states exp = 
    case exp of
	Exp.FUN (funtype, args) => Exp.FUN (funtype, map (enableEPIndex is_top states) args)
      | Exp.TERM (Exp.SYMBOL (sym, props)) => 
	if List.exists (fn(sym')=>sym=sym') states then
	    Exp.TERM (Exp.SYMBOL (sym, Property.setEPIndex props (SOME (if is_top then Property.STRUCT_OF_ARRAYS else Property.ARRAY))))
	else
	    exp
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => Exp.TERM (Exp.LIST (map (exp2term o (enableEPIndex is_top states) o Exp.TERM) termlist, dimlist))
      | Exp.TERM (Exp.TUPLE termlist) => Exp.TERM (Exp.TUPLE (map (exp2term o (enableEPIndex is_top states) o Exp.TERM) termlist))
      | _ => exp


(* find symbols and assign to output buffer *)
fun assignToOutputBuffer exp =
    case exp of
	Exp.FUN (funtype, args) => Exp.FUN (funtype, map assignToOutputBuffer args)
      | Exp.TERM (Exp.SYMBOL (sym, props)) => 
	Exp.TERM (Exp.SYMBOL (sym, Property.setOutputBuffer props true))
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => Exp.TERM (Exp.LIST (map (exp2term o assignToOutputBuffer o Exp.TERM) termlist, dimlist))
      | Exp.TERM (Exp.TUPLE termlist) => Exp.TERM (Exp.TUPLE (map (exp2term o assignToOutputBuffer o Exp.TERM) termlist))
      | _ => exp


end
