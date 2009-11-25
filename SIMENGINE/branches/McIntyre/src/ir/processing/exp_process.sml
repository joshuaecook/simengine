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
val isEquation : Exp.exp -> bool
val isExpList : Exp.exp -> bool
val isArray : Exp.exp -> bool
val isMatrix : Exp.exp -> bool
val isStateEq : Exp.exp -> bool
val isInitialConditionEq : Exp.exp -> bool
val isInstanceEq : Exp.exp -> bool
val isFirstOrderDifferentialEq : Exp.exp -> bool
val isDifferenceEq : Exp.exp -> bool
val isIntermediateEq : Exp.exp -> bool
val isPPEq : Exp.exp -> bool
val isUpdateEq : Exp.exp -> bool
val isPattern : Exp.exp -> bool

val isIntermediateTerm : Exp.exp -> bool
val isDelayedVarDifferenceTerm : Exp.exp -> bool

(* Iterator related functions *)
val doesTermHaveIterator : Symbol.symbol -> Exp.exp -> bool (* Looks at the symbol in the expression, returns if the iterator is assigned to that symbol *)
(* Looks at the symbol on the lhs, returns if the iterator is assigned to that symbol *)
val doesEqHaveIterator : Symbol.symbol -> Exp.exp -> bool 
(* like doesEqHaveIterator, put also ensures that eq is a state equation *)
val isStateEqOfIter : DOF.systemiterator -> Exp.exp -> bool 
val prependIteratorToSymbol : Symbol.symbol -> Exp.exp -> Exp.exp
val appendIteratorToSymbol : Symbol.symbol -> Exp.exp -> Exp.exp
val updateTemporalIteratorToSymbol : (Symbol.symbol * (Symbol.symbol -> Symbol.symbol)) -> Exp.exp -> Exp.exp (* update temporal iterators, requires a new iterator name, and a change function that can create a name (just used for update iterators to change scopes).  This requires that an Exp.TERM (Exp.SYMBOL) is passed in. *)
val updateTemporalIterator : Iterator.iterator -> Exp.exp -> Exp.exp (* changes the temporal iterator of a symbol to a new temporal iterator *)
val exp2spatialiterators : Exp.exp -> Iterator.iterator list
val exp2temporaliterator : Exp.exp -> Iterator.iterator option

val hasTemporalIterator : Exp.exp -> bool

(* Returns the set of names of all iterators appearing within an expression. *)
val iterators_of_expression : Exp.exp -> SymbolSet.set

(* Rewriter related functions *)
(* Constructs a rule that will match the lhs of an equation and replace it with the rhs. *)
val equation2rewrite : Exp.exp -> Rewrite.rewrite

val collect : Symbol.symbol * Exp.exp -> Exp.exp
val multicollect : Symbol.symbol list * Exp.exp -> Exp.exp


(* Expression manipulation functions - get/set differing properties *)
val renameSym : (Symbol.symbol * Symbol.symbol) -> Exp.exp -> Exp.exp (* Traverse through the expression, changing symbol names from the first name to the second name *)
type sym = Symbol.symbol
val renameInst : ((sym * sym) * (sym * sym)) -> Exp.exp -> Exp.exp (* Traverse through the expression, updating instance names *)
val exp2size : DOF.classiterator list -> Exp.exp -> int
val enableEPIndex : bool -> (Symbol.symbol list) -> Exp.exp -> Exp.exp (* Add an EP index property when running an embarrassingly parallel simulation *)
val assignCorrectScopeOnSymbol : Exp.exp -> Exp.exp (* addes the read and write state attributes based on found derivatives or differential terms *)
val assignToOutputBuffer : Exp.exp -> Exp.exp (* sets an output buffer flag - only run in the code generator which reformulates the expression for generating the output logger *)
val instOrigClassName : Exp.exp -> Symbol.symbol (* returns original class name, assumes that the expression is an instance equation *)
val instOrigInstName : Exp.exp -> Symbol.symbol (* returns original instance name, assumes that the expression is an instance equation *)
val instSpatialSize : Exp.exp -> int (* returns the assigned dimension of an instance *)

(* Extract symbols from expressions in different forms *)
val exp2symbol_names : Exp.exp -> string list
val exp2symbol : Exp.exp -> Symbol.symbol
val exp2symbols : Exp.exp -> Symbol.symbol list
val exp2symbolset: Exp.exp -> SymbolSet.set
val exp2termsymbols : Exp.exp -> Exp.term list
val getLHSTerm : Exp.exp -> Exp.term (* assuming exp is an equation, pulls out the symbol as a term from the lhs *)
val getLHSTerms : Exp.exp -> Exp.term list (* assuming exp is an equation, pulls out the symbol as a term from the lhs *)
val getLHSSymbol : Exp.exp -> Symbol.symbol (* assuming exp is an equation, pulls out the symbol as just a symbol name *)
val getLHSSymbols : Exp.exp -> Symbol.symbol list (* Grab them as a list, since the lhs can be a tuple *)

(* pull out all the function names that are present *)
val exp2fun_names : Exp.exp -> Symbol.symbol list

(* sort states by dependencies *)
val sortStatesByDependencies : (Symbol.symbol * Exp.exp * SymbolSet.set) list -> (Symbol.symbol * Exp.exp * SymbolSet.set) list

(* useful helper functions *)
val uniq : Symbol.symbol -> Symbol.symbol (* given a sym, create a unique sym name *)

val to_json : Exp.exp -> mlJS.json_value
val term_to_json : Exp.term -> mlJS.json_value

end
structure ExpProcess : EXPPROCESS =
struct

type sym = Symbol.symbol
val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val log = Util.log
val exp2str = ExpPrinter.exp2str
val e2s = ExpPrinter.exp2str
open Printer

(* TODO: Refactor*)
fun exp2termsymbols (Exp.FUN (_, exps)) = 
    List.concat (map exp2termsymbols exps)
  | exp2termsymbols (Exp.TERM (s as Exp.SYMBOL _)) = [s]
  | exp2termsymbols (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2termsymbols (Exp.TERM t)) terms)
  | exp2termsymbols (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2termsymbols (Exp.TERM t)) terms)
  | exp2termsymbols (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2termsymbols (Exp.TERM t1)) @ (exp2termsymbols (Exp.TERM t2))
  | exp2termsymbols (Exp.CONTAINER c) =
    List.concat (map exp2termsymbols (Container.container2elements c))
  | exp2termsymbols _ = []
    
fun exp2symbols exp =
    let
	val terms = exp2termsymbols exp
    in
	map Term.sym2curname terms
    end

fun exp2symbol_names exp =
    let
	val symbols = exp2symbols exp
    in
	map Symbol.name symbols
    end

(*
fun exp2symbols (Exp.FUN (_, exps)) = 
    List.concat (map exp2symbols exps)
  | exp2symbols (Exp.TERM (Exp.SYMBOL (var, _))) = [var]
  | exp2symbols (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2symbols (Exp.TERM t)) terms)
  | exp2symbols (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2symbols (Exp.TERM t)) terms)
  | exp2symbols (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2symbols (Exp.TERM t1)) @ (exp2symbols (Exp.TERM t2))
  | exp2symbols (Exp.CONTAINER c) =
    List.concat (map exp2symbols (Container.container2elements c))
  | exp2symbols _ = []

fun exp2symbol_names (Exp.FUN (_, exps)) = 
    List.concat (map exp2symbol_names exps)
  | exp2symbol_names (Exp.TERM (Exp.SYMBOL (var, _))) = [Symbol.name var]
  | exp2symbol_names (Exp.TERM (Exp.LIST (terms, _))) = 
    List.concat (map (fn(t)=> exp2symbol_names (Exp.TERM t)) terms)
  | exp2symbol_names (Exp.TERM (Exp.TUPLE terms)) = 
    List.concat (map (fn(t)=> exp2symbol_names (Exp.TERM t)) terms)
  | exp2symbol_names (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2symbol_names (Exp.TERM t1)) @ (exp2symbol_names (Exp.TERM t2))
  | exp2symbol_names (Exp.CONTAINER c) =
    List.concat (map exp2symbol_names (Container.container2elements c))
  | exp2symbol_names _ = []
    *)

fun exp2symbolset exp = SymbolSet.fromList (exp2symbols exp)
    

fun exp2fun_names (Exp.FUN (funtype, exps)) = (FunProcess.fun2name funtype)::(List.concat (map exp2fun_names exps))
  | exp2fun_names (Exp.CONTAINER c) = List.concat (map exp2fun_names (Container.container2elements c))
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
      | Exp.CONTAINER c => 
	let
	    fun renameList l = map (renameSym (orig_sym, new_sym)) l
	    fun renameArray a = (Container.list2array o 
				 renameList o
				 Container.array2list) a
	    fun renameMatrix m = (Container.rows2matrix o
				  (map renameArray) o
				  Container.matrix2rows) m
				 
	    val c' = case c of
			 Exp.EXPLIST l => Exp.EXPLIST (renameList l)
		       | Exp.ARRAY a => Exp.ARRAY (renameArray a)
		       | Exp.MATRIX m => Exp.MATRIX (renameMatrix m)
	in
	    Exp.CONTAINER c'
	end
      | _ => exp

fun renameInst (syms as ((sym, new_sym),(orig_sym,new_orig_sym))) exp =
    case exp of
	Exp.FUN (Fun.INST (inst as {classname,instname,props}), args) =>
	if classname=sym then
	    ((*Util.log("Renaming '"^(Symbol.name sym)^"' to '"^(Symbol.name new_sym)^"' and '"^(Symbol.name orig_sym)^"' to '"^(Symbol.name new_orig_sym)^"'");*)
	     Exp.FUN (Fun.INST {classname=new_sym,instname=instname,props=InstProps.setRealClassName props new_orig_sym}, 
		     map (renameInst syms) args))
	else
	    Exp.FUN (Fun.INST inst, map (renameInst syms) args)
      | Exp.FUN (f, args) => Exp.FUN (f, map (renameInst syms) args)
      | Exp.CONTAINER c => 
	let
	    fun renameList l = map (renameInst syms) l
	    fun renameArray a = (Container.list2array o 
				 renameList o
				 Container.array2list) a
	    fun renameMatrix m = (Container.rows2matrix o
				  (map renameArray) o
				  Container.matrix2rows) m
				 
	    val c' = case c of
			 Exp.EXPLIST l => Exp.EXPLIST (renameList l)
		       | Exp.ARRAY a => Exp.ARRAY (renameArray a)
		       | Exp.MATRIX m => Exp.MATRIX (renameMatrix m)
	in
	    Exp.CONTAINER c'
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
      | _ => false


fun isTerm exp = 
    case exp of
	Exp.FUN _ => false
      | _ => true

fun isSymbol exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL _) => true
      | _ => false

fun isExpList exp =
    case exp of
	Exp.CONTAINER (Exp.EXPLIST _) => true
      | _ => false

fun isArray exp =
    case exp of
	Exp.CONTAINER (Exp.ARRAY _) => true
      | _ => false

fun isMatrix exp =
    case exp of
	Exp.CONTAINER (Exp.MATRIX _) => true
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
    
fun isPattern (Exp.TERM(Exp.PATTERN _)) = true
  | isPattern _ = false

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
  | isStateTermOfIter _ _ = false
    

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
		     | Exp.META _ => 1 (*TODO: ???? *)
		     | Exp.CONTAINER c => 
		       (case c of
			    Exp.EXPLIST l => Util.sum (map (exp2size iterator_list) l)
			  | Exp.ARRAY a => Array.length a
			  | Exp.MATRIX m => (Array2.nCols m) * (Array2.nRows m))

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

fun exp2temporaliterator exp = 
    if isEquation exp then
	exp2temporaliterator (lhs exp)
    else
	case exp of 
	    Exp.TERM (term as (Exp.SYMBOL (sym, props))) => TermProcess.symbol2temporaliterator term
	  | Exp.TERM (term as (Exp.TUPLE (termlist))) => (case Util.uniquify_by_fun (fn((a,_),(a',_))=>a=a') (List.mapPartial (exp2temporaliterator o Exp.TERM) termlist) of
							      [] => NONE
							    | [iter] => SOME iter
							    | _ => DynException.stdException(("Too many temporal iterators found in tuple: " ^ (e2s exp)), "ExpProcess.exp2temporaliterator", Logger.INTERNAL))
	  | _ => NONE


fun hasTemporalIterator exp =
    case exp2temporaliterator exp
     of SOME _ => true
      | NONE => false
		
(* difference terms of the form x[n+d] or x[t[d]] where d<0 *)
fun isDelayedVarDifferenceTerm exp =
    case exp2temporaliterator exp
     of SOME (iterator, Iterator.RELATIVE d) => d < 0
      | _ => false


fun iterators_of_expression (Exp.FUN (typ, operands)) = 
    foldl SymbolSet.union SymbolSet.empty (map iterators_of_expression operands)

  | iterators_of_expression (Exp.TERM (Exp.SYMBOL (name, properties))) =
    SymbolSet.union (case Property.getScope properties
		      of Property.ITERATOR => SymbolSet.singleton name
		       | _ => SymbolSet.empty,
		     case Property.getIterator properties
		      of SOME iters => SymbolSet.fromList (map #1 iters)
		       | _ => SymbolSet.empty)

  | iterators_of_expression (Exp.TERM (Exp.TUPLE terms)) = 
    foldl SymbolSet.union SymbolSet.empty (map (iterators_of_expression o Exp.TERM) terms)

  | iterators_of_expression (Exp.TERM (Exp.LIST (terms, _))) = 
    foldl SymbolSet.union SymbolSet.empty (map (iterators_of_expression o Exp.TERM) terms)

  | iterators_of_expression (Exp.TERM (Exp.COMPLEX (real, imag))) = 
    SymbolSet.union (iterators_of_expression (Exp.TERM real),
		     iterators_of_expression (Exp.TERM imag))

  | iterators_of_expression (Exp.TERM (Exp.RANGE {low, step, high})) = 
    foldl SymbolSet.union SymbolSet.empty (map (iterators_of_expression o Exp.TERM) [low, step, high])

  | iterators_of_expression (Exp.CONTAINER c) = 
    let
	fun list2iters l = foldl SymbolSet.union SymbolSet.empty (map iterators_of_expression l)
    in
	list2iters (Container.container2elements c)
    end

  | iterators_of_expression _ = SymbolSet.empty


fun collect (sym, exp) =
  let
      val {find, test, replace} = 
	  case Rules.getRules "collect" of
	      [collectrule] => collectrule
	    | _ => DynException.stdException("Unexpected number of collect rule","Match.collect", Logger.INTERNAL)

      val replace_exp = case replace of
			    Rewrite.RULE exp => exp
			  | _ => DynException.stdException("Unexpected collect rule","Match.collect", Logger.INTERNAL)

      fun subTerm exp repl_exp = 
	  Normalize.normalize(Exp.META(Exp.APPLY{func=Exp.META(Exp.LAMBDA {arg=Symbol.symbol "term", 
									   body=exp}),
						 arg=repl_exp}))

      val collecter = {find=subTerm find (ExpBuild.var (Symbol.name sym)),
		       test=test,
		       replace=Rewrite.RULE (subTerm replace_exp (ExpBuild.var (Symbol.name sym)))}

  in
      Match.repeatApplyRewritesExp ((Rules.getRules "simplification") @ [collecter]) (Match.repeatApplyRewritesExp (Rules.getRules "expansion") exp)
  end

fun multicollect (syms, exp) =
  let
      val {find, test, replace} = 
	  case Rules.getRules "collect" of
	      [collectrule] => collectrule
	    | _ => DynException.stdException("Unexpected number of collect rule","Match.collect", Logger.INTERNAL)

      val replace_exp = case replace of
			    Rewrite.RULE exp => exp
			  | _ => DynException.stdException("Unexpected collect rule","Match.collect", Logger.INTERNAL)

      fun subTerm exp repl_exp = 
	  Normalize.normalize(Exp.META(Exp.APPLY{func=Exp.META(Exp.LAMBDA {arg=Symbol.symbol "term", 
									   body=exp}),
						 arg=repl_exp}))

      val collectors = map (fn(sym) => {find=subTerm find (ExpBuild.var (Symbol.name sym)),
					test=test,
					replace=Rewrite.RULE (subTerm replace_exp (ExpBuild.var (Symbol.name sym)))})
			   syms

      val exp' = (Match.repeatApplyRewritesExp (Rules.getRules "expansion") exp)
  in
      foldl (fn(collector,exp) => Match.repeatApplyRewritesExp ((Rules.getRules "simplification") @ [collector]) exp) 
	    exp' 
	    collectors
  end


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
	
fun exp2symbol exp =
    term2sym(exp2term (exp))

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


fun expEulerEqReformat(sym, titer, dt, exp) =
    let
	val a = ExpBuild.plus [ExpBuild.var "d1", ExpBuild.var "d4"]
	val b = ExpBuild.neg (ExpBuild.times [ExpBuild.var "d2", ExpBuild.var "d3"])

	val s = Symbol.name(term2sym(exp2term(sym)))
		    

	val reformatter = {find=ExpBuild.plus [Match.any "d1", ExpBuild.times [Match.any "d2", ExpBuild.var s, Match.any "d3"], Match.any "d4"],
			   test=NONE,
			   replace=Rewrite.RULE (ExpBuild.plus[ExpBuild.times [ExpBuild.avar s titer, 
									       ExpBuild.exp (ExpBuild.times [dt, 
													     ExpBuild.neg (b)])], 
							       ExpBuild.times [ExpBuild.divide (a, b),
									       ExpBuild.sub (ExpBuild.real 1.0,
											     ExpBuild.exp (ExpBuild.times [ExpBuild.neg b,
															   dt]))]])}
    in
	Match.repeatApplyRewritesExp ((Rules.getRules "simplification") @ [reformatter]) exp
    end

(*fun expEulerReformat ( *)



datatype p = PREPEND | APPEND

fun assignIteratorToSymbol (sym, p) exp =
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
 
fun prependIteratorToSymbol (sym) exp = assignIteratorToSymbol (sym, PREPEND) exp
fun appendIteratorToSymbol (sym) exp = assignIteratorToSymbol (sym, APPEND) exp

fun updateTemporalIteratorToSymbol (sym,symchangefun) exp = 
    let
	val (iter_sym, iter_index) = case exp2temporaliterator exp of
					 SOME iter => iter
				       | NONE => DynException.stdException(("No current temporal iterator found on exp: "^(e2s exp) ^ " where term is: " ^ (e2s (Exp.TERM(exp2term exp)))),
									   "ExpProcess.updateTemporalIteratorToSymbol",
									   Logger.INTERNAL)
	val spatial_iterators = TermProcess.symbol2spatialiterators (exp2term exp)
	val temporal_iterators = CurrentModel.iterators()				 

	val iter_type = case List.find (fn(iter_sym',_)=> iter_sym = iter_sym') temporal_iterators of
			    SOME (_,iter_type)=>iter_type
			  | NONE => DynException.stdException(("No global temporal iterator found matching: "^(Symbol.name iter_sym)),
							      "ExpProcess.updateTemporalIteratorToSymbol",
							      Logger.INTERNAL)

	val iterators' = (sym, iter_index)::spatial_iterators
	val props = Term.sym2props (exp2term exp)

	fun changeScopeIterator cur_scope_iter =
	    case iter_type of
		DOF.UPDATE v => if cur_scope_iter = v then
				    SOME (symchangefun v)
				else
				    NONE
	      | _ => if cur_scope_iter = iter_sym then
			 SOME sym
		     else NONE
			 
	val scope = Property.getScope props
	val scope' = case scope of
			 Property.READSTATE rd_sym => (case changeScopeIterator rd_sym of SOME sym => Property.READSTATE sym | NONE => scope)
		       | Property.WRITESTATE wr_sym => (case changeScopeIterator wr_sym of SOME sym => Property.WRITESTATE sym | NONE => scope)
		       | Property.READSYSTEMSTATE rd_sys_sym => (case changeScopeIterator rd_sys_sym of SOME sym => Property.READSYSTEMSTATE sym | NONE => scope)
		       | _ => scope

	val derivative = Property.getDerivative props
	val derivative' = case derivative of 
			      SOME (order, iter_list) => SOME (order, map (fn(d_sym)=>if d_sym=iter_sym then sym else d_sym) iter_list)
			    | NONE => NONE

	val props' = Property.setScope (Property.setIterator props iterators') scope'
	val props' = case derivative' of
			 SOME v => Property.setDerivative props' v
		       | NONE => props'
    in
	case (exp2term exp) of
	    Exp.SYMBOL (exp_sym, props) => 
	    term2exp (Exp.SYMBOL (exp_sym, props'))
	  | _ => DynException.stdException(("Unexpected non symbol '"^(e2s exp)^"'"),
					   "ExpProcess.updateTemporalIteratorToSymbol", 
					   Logger.INTERNAL)
	    
    end
    handle e => DynException.checkpoint "ExpProcess.updateTemporalIteratorToSymbol" e

fun updateTemporalIterator (iter as (iter_sym, iter_index)) (exp as Exp.TERM (t as Exp.SYMBOL (sym, props))) = 
    let
	val temporaliterator = case exp2temporaliterator exp of
				   SOME v => v
				 | NONE => DynException.stdException("No temporal iterator found",
								     "updateTemporalIterator",
								     Logger.INTERNAL)
	val spatialiterators = exp2spatialiterators exp
	val props' = Property.setIterator props (iter::spatialiterators)
    in
	Exp.TERM (Exp.SYMBOL (sym, props'))
    end
  | updateTemporalIterator iter _ = DynException.stdException("Non symbol encountered",
							      "ExpProcess.updateTemporalIterator",
							      Logger.INTERNAL)

fun assignCorrectScopeOnSymbol exp =
    case exp 
     of Exp.TERM (s as (Exp.SYMBOL (sym, props))) => 
	let
	    val iter = TermProcess.symbol2temporaliterator s
	    val iter' = case iter of 
			    SOME (iter_sym, iter_index) => SOME (iter_sym, iter_index, #2 (CurrentModel.itersym2iter iter_sym))
			  | NONE => NONE
	in
	    case iter' 
	     of SOME (iter_sym, iter_index, iter_type) => 
		if isFirstOrderDifferentialTerm exp then
		    Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ((*"wr_" ^ *)(Symbol.name iter_sym))))))
		else if isNextVarDifferenceTerm exp then
		    Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ((*"wr_" ^ *)(Symbol.name iter_sym))))))
		else if isNextPPTerm exp then
		    Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ((*"wr_" ^ *)(Symbol.name iter_sym))))))
		else if isNextUpdateTerm exp then
		    let
			val orig_iter = case iter_type of DOF.UPDATE v => v | _ => DynException.stdException("Unexpected non update iterator", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		    in
			Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol ((*"wr_" ^*) (Symbol.name orig_iter))))))
		    end
		else if isCurVarDifferenceTerm exp then
		    let
			val iter_sym' = case iter_type of DOF.UPDATE v => v | _ => iter_sym
		    in
			Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSYSTEMSTATE iter_sym')))
		    end
		else if isIntermediateTerm exp then
		    let
			val iter_sym' = case iter_type of DOF.UPDATE v => v | _ => iter_sym
		    in
			Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSYSTEMSTATE iter_sym')))
		    end
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
	

fun enableEPIndex is_top states exp = 
    if isSymbol exp then
	let 
	    val term = exp2term exp
	    val (sym, props) = (Term.sym2curname term, Term.sym2props term)
		      
	    fun enableEPProperty prop = 
		Exp.TERM (Exp.SYMBOL (sym, Property.setEPIndex props prop))
	in
	    if List.exists (fn(sym')=>sym=sym') states then
		enableEPProperty (SOME (if is_top then Property.STRUCT_OF_ARRAYS else Property.ARRAY))
	    else
		exp
	end
    else
	(Match.head exp) (map (enableEPIndex is_top states) (Match.level exp))
(*
    case exp of
	Exp.FUN (funtype, args) => Exp.FUN (funtype, map (enableEPIndex is_top states) args)
      | Exp.TERM (Exp.SYMBOL (sym, props)) => 
	if List.exists (fn(sym')=>sym=sym') states then
	    Exp.TERM (Exp.SYMBOL (sym, Property.setEPIndex props (SOME (if is_top then Property.STRUCT_OF_ARRAYS else Property.ARRAY))))
	else
	    exp
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => Exp.TERM (Exp.LIST (map (exp2term o (enableEPIndex is_top states) o Exp.TERM) termlist, dimlist))
      | Exp.TERM (Exp.TUPLE termlist) => Exp.TERM (Exp.TUPLE (map (exp2term o (enableEPIndex is_top states) o Exp.TERM) termlist))
      | _ => exp*)


(* find symbols and assign to output buffer *)
fun assignToOutputBuffer exp =
    if isSymbol exp then
	let
	    val term = exp2term exp
	    val (sym, props) = (Term.sym2curname term, Term.sym2props term)
	in
	    Exp.TERM (Exp.SYMBOL (sym, Property.setOutputBuffer props true))
	end
    else
	(Match.head exp) (map assignToOutputBuffer (Match.level exp))
	    
(*    case exp of
	Exp.FUN (funtype, args) => Exp.FUN (funtype, map assignToOutputBuffer args)
      | Exp.TERM (Exp.SYMBOL (sym, props)) => 
	Exp.TERM (Exp.SYMBOL (sym, Property.setOutputBuffer props true))
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => Exp.TERM (Exp.LIST (map (exp2term o assignToOutputBuffer o Exp.TERM) termlist, dimlist))
      | Exp.TERM (Exp.TUPLE termlist) => Exp.TERM (Exp.TUPLE (map (exp2term o assignToOutputBuffer o Exp.TERM) termlist))
      | _ => exp*)


fun sortStatesByDependencies nil =
    DynException.stdException("No relations passd in", "ExpProcess.sortStatesByDependencies", Logger.INTERNAL)
  | sortStatesByDependencies relations =
    let
	fun addCount (relation as (_,_,deps)) = (relation, SymbolSet.numItems deps)
	fun sort_lessthan ((_,count1),(_,count2)) = count1 < count2
	fun stateExists relation_list state =
	    List.exists (fn(state',_,_)=>state=state') relation_list
	fun findState relation_list state = 
	    let
		val (matched, unmatched) = List.partition (fn(state',_,_)=>state=state') relation_list
	    in
		(Util.hd matched, unmatched)
	    end

	(* need a relation with the smallest count *)
	fun fewestCount relation_list =
	    let 
		val sorted_list = StdFun.sort_compare sort_lessthan (map addCount relation_list)
		val (relation, _) = Util.hd sorted_list
	    in
		relation
	    end

	(* we don't want to add items from the set that already appear.  This should be less costly than searching 
	  through the larger sorted/remaining lists *)
	fun addSetToList l set =
	    foldl 
		(fn(item, list)=>
		   if List.exists (fn(item')=>item=item') list then
		       list
		   else
		       list @ [item])
		l
		(SymbolSet.listItems set)

	(* move one relation from the remaining list to the sorted list *)
	fun moveRelation (sorted, remaining, []) = 
	    let
		val r as (state, _, deps) = fewestCount remaining					    
		val (_, unmatched) = findState remaining state
	    in
		(r::sorted, unmatched, SymbolSet.listItems deps)
	    end
	  | moveRelation (sorted, remaining, backlog::backlog_list) =
	    if stateExists sorted backlog then
		(sorted, remaining, backlog_list)
	    else
		let
		    val (matched as (_, _, deps), unmatched) = findState remaining backlog
		in
		    (matched::sorted, unmatched, addSetToList (Util.tl backlog_list) deps)
		end
		
	(* recursive sortRelations list *)
	fun sortRelations (sorted, [], _) = sorted (* result *)
	  | sortRelations (var as (sorted, remaining, backlog)) = 
	    sortRelations (moveRelation var) (* recursive call *)
	    
    in
	sortRelations ([], relations, [])
    end
    handle e => DynException.checkpoint "ExpProcess.sortStatesByDependencies" e


local open mlJS in
fun jsValOf js (SOME x) = js x
  | jsValOf _ NONE = js_null

val js_symbol = js_string o Symbol.name

fun to_json (Exp.FUN (typ, expressions)) = fun_to_json (typ, expressions)
  | to_json (Exp.TERM term) = term_to_json term
  | to_json (Exp.META meta) = meta_to_json meta
  | to_json (Exp.CONTAINER c) = container_to_json c

and fun_to_json (Fun.BUILTIN operation, expressions) =
    js_object [("type", js_string "BUILTIN"),
	       ("operation", js_string (#name (FunProps.op2props operation))),
	       ("operands", js_array (map to_json expressions))]
  | fun_to_json (Fun.INST {classname,instname,props}, expressions) = 
    let val {dim,sourcepos,realclassname,realinstname,iterators,inline} 
	  = props

	val json_properties = js_object [("inline", js_boolean inline),
					 ("iterators", js_array (map js_symbol iterators)),
					 ("realInstanceName", jsValOf js_symbol realinstname),
					 ("realClassName", jsValOf js_symbol realclassname),
					 ("sourcePosition", jsValOf PosLog.to_json sourcepos),
					 ("dimensions", jsValOf (js_array o (map js_int)) dim)]
    in
	js_object [("type", js_string "INSTANCE"),
		   ("className", js_string (Symbol.name classname)),
		   ("instanceName", js_string (Symbol.name instname)),
		   ("properties", json_properties),
		   ("operands", js_array (map to_json expressions))]
    end

and term_to_json (Exp.RATIONAL (num, denom)) =
    js_object [("type", js_string "COMPLEX"),
	       ("numerator", js_int num),
	       ("denominator", js_int denom)]
  | term_to_json (Exp.INT z) = 
    js_object [("type", js_string "INT"),
	       ("value", js_int z)]
  | term_to_json (Exp.REAL r) = 
    js_object [("type", js_string "REAL"),
	       ("value", js_float r)]
  | term_to_json (Exp.BOOL b) = 
    js_object [("type", js_string "BOOL"),
	       ("value", js_boolean b)]
  | term_to_json (Exp.COMPLEX (r, i)) = 
    js_object [("type", js_string "COMPLEX"),
	       ("real", term_to_json r),
	       ("imaginary", term_to_json i)]
  | term_to_json (Exp.LIST (terms, dimens)) = 
    js_object [("type", js_string "LIST"),
	       ("terms", js_array (map term_to_json terms)),
	       ("dimensions", js_array (map js_int dimens))]
  | term_to_json (Exp.TUPLE terms) =
    js_object [("type", js_string "TUPLE"),
	       ("terms", js_array (map term_to_json terms))]
  | term_to_json (Exp.RANGE {low, high, step}) = 
    js_object [("type", js_string "RANGE"),
	       ("low", term_to_json low),
	       ("step", term_to_json step),
	       ("high", term_to_json high)]
  | term_to_json (Exp.SYMBOL (name, properties)) = 
    let val {dim, iterator, derivative, isevent, sourcepos, realname, scope, outputbuffer, ep_index}
	  = properties

	val json_iterators
	  = case iterator 
	     of SOME iters => 
		let fun iterator_to_json (name, index) =
			js_object [("name", js_symbol name),
				   ("index", iterator_index_to_json index)]

		    and iterator_index_to_json (Iterator.RELATIVE z) =
			js_object [("type", js_string "RELATIVE"), ("value", js_int z)]
		      | iterator_index_to_json (Iterator.ABSOLUTE z) = 
			js_object [("type", js_string "ABSOLUTE"), ("value", js_int z)]
		      | iterator_index_to_json (Iterator.RANGE (low, high)) = 
			js_object [("type", js_string "RANGE"), ("low", js_int low), ("high", js_int high)]
		      | iterator_index_to_json (Iterator.LIST zz) = 
			js_object [("type", js_string "LIST"), ("values", js_array (map js_int zz))]
		      | iterator_index_to_json Iterator.ALL = 
			js_object [("type", js_string "ALL")]
		in
		    js_array (map iterator_to_json iters)
		end
	      | NONE => js_null

	val json_scope
	  = case scope
	     of Property.LOCAL => 
		js_object [("type", js_string "LOCAL")]
	      | Property.READSTATE name => 
		js_object [("type", js_string "READSTATE"), ("name", js_symbol name)]
	      | Property.READSYSTEMSTATE name => 
		js_object [("type", js_string "READSYSTEMSTATE"), ("iterator", js_symbol name)]
	      | Property.WRITESTATE name => 
		js_object [("type", js_string "WRITESTATE"), ("name", js_symbol name)]
	      | Property.ITERATOR => 
		js_object [("type", js_string "ITERATOR")]

	val json_derivative
	  = case derivative
	     of SOME (degree, symbols) =>
		js_object [("degree", js_int degree),("iterators", js_array (map js_symbol symbols))]
	      | NONE => js_null

	val json_properties
	  = js_object [("dimensions", jsValOf (js_array o (map js_int)) dim),
		       ("iterators", json_iterators),
		       ("derivative", json_derivative),
		       ("isEvent", js_boolean isevent),
		       ("sourcePosition", jsValOf PosLog.to_json sourcepos),
		       ("realName", jsValOf js_symbol realname),
		       ("scope", json_scope),
		       ("outputBuffer", js_boolean outputbuffer),
		       ("parallelIndex", jsValOf (js_string o (fn Property.ARRAY => "ARRAY" | Property.STRUCT_OF_ARRAYS => "STRUCT_OF_ARRAYS")) ep_index)]
    in
	js_object [("type", js_string "SYMBOL"),
		   ("name", js_string (Symbol.name name)),
		   ("properties", json_properties)]
    end
  | term_to_json Exp.DONTCARE =
    js_object [("type", js_string "DONTCARE")]
  | term_to_json Exp.INFINITY = 
    js_object [("type", js_string "INFINITY")]
  | term_to_json Exp.NAN = 
    js_object [("type", js_string "NAN")]
  | term_to_json (Exp.PATTERN (name, predicate, arity)) = 
    let val json_arity 
	  = case arity 
	     of Pattern.ONE => js_object [("type", js_string "ONE")]
	      | Pattern.ONE_OR_MORE => js_object [("type", js_string "ONE_OR_MORE")]
	      | Pattern.ZERO_OR_MORE => js_object [("type", js_string "ZERO_OR_MORE")]
	      | Pattern.SPECIFIC_COUNT count => 
		js_object [("type", js_string "COUNT"), ("count", js_int count)]
	      | Pattern.SPECIFIC_RANGE (low, high) => 
		js_object [("type", js_string "RANGE"), ("low", js_int low), ("high", js_int high)]
    in
	js_object [("type", js_string "PATTERN"),
		   ("predicate", js_string "FIXME"),
		   ("arity", json_arity)]
    end

and meta_to_json (Exp.LAMBDA {arg, body}) =
    js_object [("type", js_string "LAMBDA"),
	       ("argument", js_symbol arg),
	       ("body", to_json body)]
  | meta_to_json (Exp.APPLY {func, arg}) = 
    js_object [("type", js_string "APPLY"),
	       ("function", to_json func),
	       ("argument", to_json arg)]
  | meta_to_json (Exp.MAP {func, args}) =  
    js_object [("type", js_string "MAP"),
	       ("function", to_json func),
	       ("arguments", to_json args)]
  | meta_to_json (Exp.SEQUENCE members) =  
    js_object [("type", js_string "SEQUENCE"),
	       ("members", js_array (map to_json members))]
   
and container_to_json (Exp.EXPLIST l) =
    js_object [("type", js_string "EXPLIST"),
	       ("members", js_array (map to_json l))]
  | container_to_json (Exp.ARRAY a) =
    js_object [("type", js_string "ARRAY"),
	       ("length", js_int (Array.length a)),
	       ("members", js_array (map to_json (Container.array2list a)))]
  | container_to_json (Exp.MATRIX m) =
    js_object [("type", js_string "MATRIX"),
	       ("rows", js_int (Array2.nRows m)),
	       ("columns", js_int (Array2.nCols m)),
	       ("members", js_array (map 
					 (to_json o Exp.CONTAINER o Exp.ARRAY)
					 (Container.matrix2rows m)))]

end

end
