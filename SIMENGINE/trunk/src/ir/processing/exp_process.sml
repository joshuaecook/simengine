signature EXPPROCESS =
sig

(* A visitor is a type of function which, 
 * when given an expression and some value,
 * returns a new value. *)
type 'a visitor = Exp.exp * 'a -> 'a

(* Commonly used functions *)
val exp2term : Exp.exp -> Exp.term (* must be of term expression, otherwise this fails *)
val term2exp : Exp.term -> Exp.exp

(* Basic extraction functions *)
val lhs : Exp.exp -> Exp.exp
val rhs : Exp.exp -> Exp.exp
val deconstructInst : Exp.exp -> {classname: Symbol.symbol,
				  instname: Symbol.symbol,
				  props: InstProps.instproperties,
				  inpargs: Exp.exp SymbolTable.table,
				  outargs: Exp.term SymbolTable.table}

(* Classification functions *)
val isTerm : Exp.exp -> bool
val isSymbol : Exp.exp -> bool
val isIterator : Exp.exp -> bool
val isEquation : Exp.exp -> bool
val isExpList : Exp.exp -> bool
val isArray : Exp.exp -> bool
val isArrayEq : Exp.exp -> bool
val isMatrix : Exp.exp -> bool
val isMatrixEq : Exp.exp -> bool
val isStateEq : Exp.exp -> bool
val isInitialConditionEq : Exp.exp -> bool
val isInstanceEq : Exp.exp -> bool
val isOutputEq : Exp.exp -> bool
val isFirstOrderDifferentialEq : Exp.exp -> bool
val isDifferenceEq : Exp.exp -> bool
val isIntermediateEq : Exp.exp -> bool
val isAlgebraicStateEq : Exp.exp -> bool
val isPreProcessStateEq : Exp.exp -> bool
val isInProcessStateEq : Exp.exp -> bool
val isPostProcessStateEq : Exp.exp -> bool
val isUpdateEq : Exp.exp -> bool
val isForwardReferencedContinuousEq : Exp.exp -> bool
val isReadStateEq : Exp.exp -> bool (* checks whether the equation writes to a "read" state *)
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
val renameTemporalIteratorForAggregating : (Symbol.symbol list * Symbol.symbol) -> Exp.exp -> Exp.exp
val exp2spatialiterators : Exp.exp -> Iterator.iterator list
val exp2temporaliterator : Exp.exp -> Iterator.iterator option

val hasTemporalIterator : Exp.exp -> bool

(* Returns the set of names of all iterators appearing within an expression. *)
val iterators_of_expression : Exp.exp -> SymbolSet.set

(* Rewriter related functions *)
(* Constructs a rule that will match the lhs of an equation and replace it with the rhs. *)
val equation2rewrite : Exp.exp -> Rewrite.rewrite
val intermediateEquation2rewrite : Exp.exp -> Rewrite.rewrite

val simplify: Exp.exp -> Exp.exp
val expand: Exp.exp -> Exp.exp
val collect : Exp.exp * Exp.exp -> Exp.exp
val multicollect : Exp.exp list * Exp.exp -> Exp.exp
val factorsym : (Exp.exp * Symbol.symbol) -> (Exp.exp * Exp.exp) (* return the coefficient and remainder *)
val hasSymbol : (Exp.exp * Symbol.symbol) -> bool
val isLinear : (Exp.exp * Symbol.symbol) -> bool
val coeff : (Exp.exp * Symbol.symbol) -> Exp.exp

(* Expression manipulation functions - get/set differing properties *)
val renameSym : (Symbol.symbol * Symbol.symbol) -> Exp.exp -> Exp.exp (* Traverse through the expression, changing symbol names from the first name to the second name *)
val renameInst : ((Symbol.symbol * Symbol.symbol) * (Symbol.symbol * Symbol.symbol)) -> Exp.exp -> Exp.exp (* Traverse through the expression, updating instance names *)
val exp2size : Exp.exp -> int
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
val analyzeRelations : (Symbol.symbol * Exp.exp * SymbolSet.set) list -> unit
val sortStatesByDependencies : (Symbol.symbol * Exp.exp * SymbolSet.set) list -> (Symbol.symbol * Exp.exp * SymbolSet.set) list

(* useful helper functions *)
val uniq : Symbol.symbol -> Symbol.symbol (* given a sym, create a unique sym name *)

end
structure ExpProcess : EXPPROCESS =
struct

type 'a visitor = Exp.exp * 'a -> 'a


val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val log = Util.log
val exp2str = ExpPrinter.exp2str
val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr
val head = ExpTraverse.head
val level = ExpTraverse.level

open Printer

(* TODO: Refactor*)
fun exp2termsymbols (Exp.FUN (_, exps)) = 
    Util.flatmap exp2termsymbols exps
  | exp2termsymbols (Exp.TERM (s as Exp.SYMBOL _)) = [s]
  | exp2termsymbols (Exp.TERM (Exp.TUPLE terms)) = 
    Util.flatmap (fn(t)=> exp2termsymbols (Exp.TERM t)) terms
  | exp2termsymbols (Exp.TERM (Exp.COMPLEX (t1, t2))) =
    (exp2termsymbols (Exp.TERM t1)) @ (exp2termsymbols (Exp.TERM t2))
  | exp2termsymbols (Exp.CONTAINER c) =
    Util.flatmap exp2termsymbols (Container.containerToElements c)
  | exp2termsymbols _ = []
    
fun exp2symbols (Exp.FUN (_, operands))		= Util.flatmap exp2symbols operands
  | exp2symbols (Exp.TERM term) 		= term2symbols term
  | exp2symbols (Exp.CONTAINER c)               = Util.flatmap exp2symbols (Container.containerToElements c)
  | exp2symbols _ 				= nil

and term2symbols (Exp.SYMBOL (var, _)) 		= [var]
  | term2symbols (Exp.TUPLE terms) 		= Util.flatmap term2symbols terms
  | term2symbols (Exp.COMPLEX (real, imag)) 	= (term2symbols real) @ (term2symbols imag)
  | term2symbols _ 				= nil

fun exp2symbol_names exp =
    let
	val symbols = exp2symbols exp
    in
	map Symbol.name symbols
    end



fun exp2symbolset exp = SymbolSet.fromList (exp2symbols exp)
    

fun exp2fun_names (Exp.FUN (funtype, exps)) = (FunProcess.fun2name funtype)::(Util.flatmap exp2fun_names exps)
  | exp2fun_names (Exp.CONTAINER c) = Util.flatmap exp2fun_names (Container.containerToElements c)
  | exp2fun_names _ = []

val uniqueid = ref 0

fun uniq(sym) =
    (uniqueid := (!uniqueid)+1;
     (Symbol.symbol ((Symbol.name sym) ^ "_U" ^ (i2s (!uniqueid)))))


fun exp2term (Exp.TERM t) = t
  | exp2term exp = Exp.NAN before (Logger.log_warning (Printer.$ ("Can not convert expression '"^(e2s exp)^"' to a term, replacing with NaN")))
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
	    fun renameArray a = (Container.listToArray o 
				 renameList o
				 Container.arrayToList) a
	    fun renameMatrix m = 
		let
		    val _ = Util.log ("Remaining Matrix (exp_process): " ^ (Matrix.infoString m))
		in
		    ((Matrix.fromRows (Exp.calculus())) o
		     (map renameArray) o
		     Matrix.toRows) m
		end
				 
	    val c' = case c of
			 Exp.EXPLIST l => Exp.EXPLIST (renameList l)
		       | Exp.ARRAY a => Exp.ARRAY (renameArray a)
		       | Exp.ASSOC t => Exp.ASSOC (SymbolTable.map (renameSym (orig_sym, new_sym)) t)
		       | Exp.MATRIX m => 
			 Exp.MATRIX 
			     (Container.expMatrixToMatrix 
				  ((head exp) (map (renameSym (orig_sym, new_sym)) (level exp))))
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
      | Exp.FUN (Fun.OUTPUT (output as {classname,instname,outname,props}), args) =>
	if classname = sym then
	    Exp.FUN (Fun.OUTPUT {classname = new_sym, instname = instname, outname = outname, props = InstProps.setRealClassName props new_orig_sym},
		     map (renameInst syms) args)
	else
	    Exp.FUN (Fun.OUTPUT output, map (renameInst syms) args)
      | Exp.FUN (f, args) => Exp.FUN (f, map (renameInst syms) args)
      | Exp.CONTAINER c => 
	let
	    fun renameList l = map (renameInst syms) l
	    fun renameArray a = (Container.listToArray o 
				 renameList o
				 Container.arrayToList) a
	    fun renameMatrix m = ((Matrix.fromRows (Exp.calculus())) o
				  (map renameArray) o
				  Matrix.toRows) m
				 
	    val c' = case c of
			 Exp.EXPLIST l => Exp.EXPLIST (renameList l)
		       | Exp.ARRAY a => Exp.ARRAY (renameArray a)
		       | Exp.ASSOC t => Exp.ASSOC (SymbolTable.map (renameInst syms) t)
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

fun user_error exp text = 
	(Logger.log_error (Printer.$("Error when processing '"^(e2s exp)^"': "^(text)));
	 DynException.setErrored())

fun error_no_return exp text = 
    if raiseExceptions then
	DynException.stdException(("Error when processing '"^(e2s exp)^"': "^(text)), "ExpProcess.error_no_return", Logger.INTERNAL)
    else
	user_error exp text

fun error exp text = (error_no_return exp text; 
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

fun isIterator exp = 
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) => (case Property.getScope props of
						 Property.ITERATOR => true
					       | _ => false)
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
	     
fun isOutputEq exp = 
    (case exp of 
	 Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, Exp.FUN (Fun.OUTPUT {props,...}, _)]) => 
	 not (InstProps.isInline props)
       | _ => false)
    handle e => DynException.checkpoint "ExpProcess.isOutputEq" e
	     
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
			    inpargs=SymbolTable.empty, 
			    outargs=SymbolTable.empty}

	(* take all the instance outputs and return a mapping of the class outputs to the instance outputs *)
	fun instance_outputs_to_output_table classname outargs = 
	    let
		val class = CurrentModel.classname2class classname
		val outputs = !(#outputs class)
	    in
		foldl
		    (fn((class_output, instance_output), table)=> SymbolTable.enter (table, class_output, instance_output))
		    SymbolTable.empty
		    (ListPair.zip (map (Term.sym2curname o DOF.Output.name) outputs, outargs))
	    end
	    
	fun singleton_output_to_output_table outname outarg = 
	    SymbolTable.enter (SymbolTable.empty, outname, outarg)
    in
	if isInstanceEq exp then
	    case exp of 
		Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE outargs), Exp.FUN (Fun.INST {classname, instname, props}, [Exp.CONTAINER (Exp.ASSOC inpargs)])]) => 
		{classname=classname, instname=instname, props=props, inpargs=inpargs, outargs=(instance_outputs_to_output_table classname outargs)}
	      | _ => (error_no_return exp "Malformed instance equation"; empty_return)
	else if isOutputEq exp then
	    case exp of
		Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE [outarg]), Exp.FUN (Fun.OUTPUT {classname, instname, outname, props}, [Exp.CONTAINER (Exp.ASSOC inpargs)])]) => 
		{classname=classname, instname=instname, props=props, inpargs=inpargs, outargs=(singleton_output_to_output_table outname outarg)}
	      | _ => (error_no_return exp "Malformed output equation"; empty_return)
	else
	    (error_no_return exp "Not an instance or output equation"; empty_return)
    end

fun instSpatialSize inst =
    if isInstanceEq inst then 
	1
    else 
   (* we need to add a way to compute the instance size since deprecating getDim *) 
   (* if isInstanceEq inst then
	let
	    val {props,...} = deconstructInst inst
	in
	    case InstProps.getDim props  
	     of SOME l => Util.prod l
	      | NONE => 1
	end	    
    else*)
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
	instname
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

(*
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
*)
fun isAlgebraicStateTermOfProcessType process exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val discrete_iterators = List.filter (fn(sym, itertype)=>case (itertype, process) of 
									 (DOF.ALGEBRAIC (DOF.PREPROCESS,_), SOME DOF.PREPROCESS) => true
								       | (DOF.ALGEBRAIC (DOF.INPROCESS,_), SOME DOF.INPROCESS) => true
								       | (DOF.ALGEBRAIC (DOF.POSTPROCESS,_), SOME DOF.POSTPROCESS) => true
								       | (DOF.ALGEBRAIC _, NONE) => true
								       | _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 1)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | SOME ((iterator, Iterator.RELATIVE 0)::rest) => List.exists (fn(sym, _)=> iterator = sym) discrete_iterators
	      | _ => false
	end
      | _ => false

fun isAlgebraicStateTerm exp = isAlgebraicStateTermOfProcessType NONE exp

fun isAlgebraicStateEqOfProcessType process exp =
    isEquation exp andalso
    isAlgebraicStateTermOfProcessType process (lhs exp)

fun isAlgebraicStateEq exp = isAlgebraicStateEqOfProcessType NONE exp
fun isPreProcessStateEq exp = isAlgebraicStateEqOfProcessType (SOME DOF.PREPROCESS) exp
fun isInProcessStateEq exp = isAlgebraicStateEqOfProcessType (SOME DOF.INPROCESS) exp
fun isPostProcessStateEq exp = isAlgebraicStateEqOfProcessType (SOME DOF.POSTPROCESS) exp

fun isForwardReferencedContinuousTerm exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (_, props)) =>
	let
	    val iterators = Property.getIterator props
	    val continuous_iterators = List.filter (fn(sym, itertype)=>case itertype of 
									 DOF.CONTINUOUS _ => true
								       | _ => false) (CurrentModel.iterators())
	in
	    case iterators of
		SOME ((iterator, Iterator.RELATIVE 1)::rest) => List.exists (fn(sym, _)=> iterator = sym) continuous_iterators
	      | _ => false
	end
      | _ => false

fun isForwardReferencedContinuousEq exp =
    isEquation exp andalso
    isForwardReferencedContinuousTerm (lhs exp)


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
    
fun isStateBufferTerm (Exp.TERM (Exp.SYMBOL (_, props))) =
    (case Property.getScope props of
	Property.READSYSTEMSTATE _ => true
      | Property.READSTATE _ => true
      | Property.WRITESTATE _ => true
      | _ => false)
  | isStateBufferTerm _ = false

fun isReadStateTerm (Exp.TERM (Exp.SYMBOL (_, props))) =
    (case Property.getScope props of
	Property.READSYSTEMSTATE _ => true
      | Property.READSTATE _ => true (* is the case for pre-process iterators when aggregated *)
      | _ => false)
  | isReadStateTerm _ = false

fun isReadStateEq exp =
    isEquation exp andalso
    isReadStateTerm (lhs exp)

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
	     val derivative = Property.getDerivative props
	     val iterators = Property.getIterator props
	 in
	     case (derivative,iterators) of
		 (SOME (_, _::_), SOME ((iterator, _)::rest)) => 
		 if iterator = name then
		     (* this user error is caused if the user defines a differential equation with a discrete iterator. It's possible that this could happen later,
			and then should be an exception, but without adding specific checks for this case earlier, it would be hard to determine.  *)
		     (user_error exp ("Unexpected derivative found with discrete iterator '"^(Symbol.name name)^"'. Derivatives can only be used with continuous iterators."); false)
		 else
		     false
	       | (NONE, SOME ((iterator, Iterator.RELATIVE 1)::rest)) => iterator = name
	       | _ => false
	 end
       | _ => false)
    
  | isStateTermOfIter (iter as (name, DOF.ALGEBRAIC (DOF.PREPROCESS, _))) exp =
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
  | isStateTermOfIter (iter as (name, DOF.ALGEBRAIC (DOF.INPROCESS, _))) exp =
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
  | isStateTermOfIter (iter as (name, DOF.ALGEBRAIC (DOF.POSTPROCESS, _))) exp =
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
    (isEquation exp andalso
     isStateTermOfIter iter (lhs exp))
    handle e => DynException.checkpoint ("ExpProcess.isStateEqOfIter [iter="^(Symbol.name (#1 iter))^", exp="^(e2s exp)^"]") e

fun isStateTerm exp = 
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(iter)=>isStateTermOfIter iter exp) iterators
    end
    handle e => DynException.checkpoint ("ExpProcess.isStateTerm [exp="^(e2s exp)^"]") e

fun isStateEq exp =
    (isEquation exp andalso
     isStateTerm (lhs exp))
    handle e => DynException.checkpoint ("ExpProcess.isStateEq [exp="^(e2s exp)^"]") e

fun isArrayEq exp =
    isEquation exp andalso
    isArray (rhs exp)

fun isMatrixEq exp =
    isEquation exp andalso
    isMatrix (rhs exp)

(* intermediate equations *)
fun isIntermediateTerm exp =
    let
	val isIntermediate = 
	    case exp of
		Exp.TERM (Exp.SYMBOL (_, props)) =>
		let
		    val derivative = Property.getDerivative props
		    val iterators = Property.getIterator props
		    val islocal = case Property.getScope props of
				      Property.LOCAL => true
				    | _ => false
		    val all_iterators = CurrentModel.iterators()
		in
		    islocal andalso not (isAlgebraicStateTerm exp) andalso not (isNextUpdateTerm exp) andalso
		    case (derivative, iterators) of
			(SOME _, _) => false
		      | (_, SOME ((itersym, Iterator.ABSOLUTE _)::rest)) => not (List.exists (fn(sym,_)=>sym=itersym) all_iterators)
		      | (_, SOME ((itersym, Iterator.RELATIVE 1)::rest)) => not (List.exists (fn(sym,_)=>sym=itersym) all_iterators)
		      | (_, _) => true
		end
	      | _ => false
	(*val _ = Util.log ("isIntermediate? ["^(e2s exp)^"]: " ^ (Util.b2s isIntermediate))*)
    in
	isIntermediate
    end

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
    handle e => DynException.checkpoint "ExpProcess.doesEqHaveIterator" e

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

fun intermediateEquation2rewrite exp = 
    if isEquation exp then
	let
	    val lhs = lhs exp

	    (* for an intermediate equation, just substitute by ignoring properties - now Vm[] will match Vm[t_imp], for example *)
	    (* if we allow spatial iterators, we're going to need to be more strict about this check, for example only substituting if the spatial indices are the same *)
	    (* if and when we all that, we'll have to refactor a bunch of this type of code that is used in unifying and elsewhere that verifies equivalency. *)
	    val find = case lhs of
			   Exp.TERM (Exp.SYMBOL (sym, props)) => Match.asym sym
			 | _ => lhs

	    val rhs = rhs exp
	in
	    {find=find,
	     test=NONE,
	     replace=Rewrite.RULE rhs}
	end
    else
	DynException.stdException(("Can't write a rewrite around an expression '"^(e2s exp)^"' that is not already an equation"),
				  "ExpProcess.intermediateEquation2rewrite",
				  Logger.INTERNAL)


fun exp2size exp : int = 
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
				   (*case List.find (fn{name,...}=>name=itersym) iterator_list of
					    SOME {name, low, high, step} => Real.ceil((high-low)/step + 1.0)
					  | _ => *)DynException.stdException(
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
			       (*Util.prod (map iterator2size spatial_iterators)*)
			       (* TODO: We're not supporting spatial iterators anyway... *)
			       1
			   end
		       else 
			       1 (* out of default - need to do something better here *)
		     | Exp.FUN (Fun.BUILTIN f, args) => 
		       let
			   val codomain = #codomain (MathFunctionProperties.op2props f)
		       in
			   Util.prod(codomain (map (fn(a) => [exp2size a]) args))
(*		       foldl combineSizes 1 (map (exp2size iterator_list) args)*)
		       end
		     | Exp.FUN (Fun.INST _, args) => 1 (*TODO: ???? *)
		     | Exp.FUN (Fun.OUTPUT _, args) => 1 (*TODO: ???? *)
		     | Exp.META _ => 1 (*TODO: ???? *)
		     | Exp.CONTAINER c => 
		       (case c of
			    Exp.EXPLIST l => Util.sum (map exp2size l)
			  | Exp.ARRAY a => Array.length a
			  | Exp.ASSOC t => Util.sum (map exp2size (SymbolTable.listItems t))
			  | Exp.MATRIX m => 
			    let
				val (nrows, ncols) = Matrix.size m
			    in
				nrows * ncols
			    end)
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
    (if isEquation exp then
	 exp2temporaliterator (lhs exp)
     else
	 case exp of 
	     Exp.TERM (term as (Exp.SYMBOL (sym, props))) => 
	     TermProcess.symbol2temporaliterator term
	   | Exp.TERM (term as (Exp.TUPLE (termlist))) => 
	     (case Util.uniquify_by_fun (fn((a,_),(a',_))=>a=a') (List.mapPartial (exp2temporaliterator o Exp.TERM) termlist) of
		  [] => NONE
		| [iter] => SOME iter
		| _ => DynException.stdException(("Too many temporal iterators found in tuple: " ^ (e2s exp)), "ExpProcess.exp2temporaliterator", Logger.INTERNAL))
	   | _ => NONE)
    handle e => DynException.checkpoint ("ExpProcess.exp2temporaliterator ["^(e2s exp)^"]") e

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

  | iterators_of_expression (Exp.TERM (Exp.COMPLEX (real, imag))) = 
    SymbolSet.union (iterators_of_expression (Exp.TERM real),
		     iterators_of_expression (Exp.TERM imag))

  | iterators_of_expression (Exp.TERM (Exp.RANGE {low, step, high})) = 
    foldl SymbolSet.union SymbolSet.empty (map (iterators_of_expression o Exp.TERM) [low, step, high])

  | iterators_of_expression (Exp.CONTAINER c) = 
    let
	fun list2iters l = foldl SymbolSet.union SymbolSet.empty (map iterators_of_expression l)
    in
	list2iters (Container.containerToElements c)
    end

  | iterators_of_expression _ = SymbolSet.empty

fun simplify exp =
    Match.repeatApplyRewritesExp (Rules.getRules "simplification") exp

fun expand exp =
    Match.repeatApplyRewritesExp (Rules.getRules "expansion") exp

fun collect (symexp, exp) =
  let
      fun buildCollecter {find, test, replace} = 
	  let
	      val replace_exp = case replace of
				    Rewrite.RULE exp => exp
				  | _ => DynException.stdException("Unexpected collect rule","Match.collect", Logger.INTERNAL)

	      fun subTerm exp repl_exp = 
		  Normalize.normalize(Exp.META(Exp.APPLY{func=Exp.META(Exp.LAMBDA {arg=Symbol.symbol "term", 
										   body=exp}),
							 arg=repl_exp}))

	      val collecter = {find=subTerm find symexp,
			       test=test,
			       replace=Rewrite.RULE (subTerm replace_exp symexp)}
	  in 
	      collecter
	  end

      val collecters = map buildCollecter (Rules.getRules "collect")

  in
      Match.repeatApplyRewritesExp ((Rules.getRules "simplification") @ collecters) (Match.repeatApplyRewritesExp (Rules.getRules "expansion") exp)
  end
    handle e => DynException.checkpoint "ExpProcess.collect" e

fun multicollect (symexps, exp) =
    let
	fun buildCollecter sym {find, test, replace} = 
	    let
		val replace_exp = case replace of
				      Rewrite.RULE exp => exp
				    | _ => DynException.stdException("Unexpected collect rule","Match.collect", Logger.INTERNAL)

		fun subTerm exp repl_exp = 
		    Normalize.normalize(Exp.META(Exp.APPLY{func=Exp.META(Exp.LAMBDA {arg=Symbol.symbol "term", 
										     body=exp}),
							   arg=repl_exp}))

		val collecter = {find=subTerm find sym,
				 test=test,
				 replace=Rewrite.RULE (subTerm replace_exp sym)}
	    in 
		collecter
	    end

      val collecters = map (fn(sym) => 
			       map (buildCollecter sym) (Rules.getRules "collect"))
			    symexps
(*
      val _ = Util.log ("# collecters = " ^ (Int.toString (length collecters)))
      val _ = Util.log ("# of collector members = " ^ (String.concatWith ", " (map (i2s o length) collecters)))
      val _ = app (fn(rules)=> (Util.log(" - ");app (fn(rule)=>Util.log (" -> Rule: " ^ (Rewrite.rewrite2str rule))) rules)) collecters
*)
      val exp' = (Match.repeatApplyRewritesExp (Rules.getRules "expansion") exp)
      (*val _ = Util.log ("exp' -> " ^ (e2s exp'))*)
  in
      foldl (fn(collecter_group,exp) => 
	       let
		   (*val _ = Util.log ("In foldl: exp=" ^ (e2s exp))
		   val _ = app (fn(rule)=>Util.log (" -> Rule: " ^ (Rewrite.rewrite2str rule))) collecter_group*)
	       in
		   Match.repeatApplyRewritesExp ((Rules.getRules "simplification") @ collecter_group) exp
	       end) 
	    exp' 
	    collecters
  end
    handle e => DynException.checkpoint "ExpProcess.multicollect" e


(* factorsym - given an expression, attempt to factor out the coefficient, returning the coefficient and remainder 
 * Note that this only works at the top level
 * examples: 
 *   factorsym (a + x*b + c, x) => (b, a + c)
 *   factorsym (a + x + c, x) =>  (1, a + c)
 *   factorsym (a + x + exp(y), y) => (0, a + x + exp(y))
 *)

fun factorsym (exp, sym) =
    let
	(* create a variable to seach for *)
	val var = Match.asym sym

	(* create a rewrite to match the resulting equation *)
	val coeff_rewrite =
	    {find=ExpBuild.plus[Match.any "d1", 
				ExpBuild.times [Match.any "d2", 
						var, 
						Match.any "d3"], 
				Match.any "d4"],
	     (* use a test to make sure that we're only looking at the top most expression *)
	     test=SOME (fn(matched_exp, matchedPatterns)=>ExpEquality.equiv (exp, matched_exp)),
	     replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.times [ExpBuild.pvar "d2", ExpBuild.pvar "d3"],
							    ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}

	(* it could be that there is no coefficient in front of the variable *)
	val coeff_rewrite_degenerate_case =
	    {find=ExpBuild.plus[Match.any "d1", 
				var, 
				Match.any "d4"],
	     (* use a test to make sure that we're only looking at the top most expression *)
	     test=SOME (fn(matched_exp, matchedPatterns)=>ExpEquality.equiv (exp, matched_exp)),
	     replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.int 1,
							    ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}

    in
	(* run a rewrite to pull out the coeff and remainder *)
	case Match.applyRewritesExp [coeff_rewrite, coeff_rewrite_degenerate_case] exp of
	    Exp.CONTAINER(Exp.EXPLIST [coeff, remainder]) =>
	    (coeff, remainder)
	  | _ =>
	    (ExpBuild.int 0, exp)
    end	    
    handle e => DynException.checkpoint "ExpProcess.factorsym" e

fun hasSymbol (exp, sym) =
    isSome (Match.findOnce (Match.asym sym, exp))
    handle e => DynException.checkpoint "ExpProcess.hasSymbol" e

fun isLinear (exp, sym) =
    let
	val (coeff, remainder) = factorsym (exp, sym)
	(*val _ = Util.log ("Sym: "^(Symbol.name sym)^", Coeff: " ^ (e2s coeff) ^ ", Remainder: " ^ (e2s remainder))*)
    in
	(* Verify that remainder and coefficient does not contain 'sym' (indicating non-linearity) *)  		
	not (hasSymbol (ExpBuild.explist [coeff, remainder], sym))
    end
    handle e => DynException.checkpoint "ExpProcess.isLinear" e

(* coeff - given an expression, find the coefficient to that expression
 * Note that this only works at the top level
 * examples: 
 *   coeff (a + x*b + c, x) => b
 *   coeff (a + x + c, x) =>  1
 *   coeff (a + x + exp(y), y) => 0
 *)
fun coeff (exp, sym) = 
    let
	val (coeff, _) = factorsym (exp, sym)
    in
	coeff
    end
    handle e => DynException.checkpoint "ExpProcess.coeff" e

fun symterm2symterm term = 
    (case term of 
	 Exp.SYMBOL s => Exp.SYMBOL s
       | _ => (error_no_return (term2exp term) ("No valid symbol found on term");
	       Exp.SYMBOL (Symbol.symbol "???", Property.default_symbolproperty)))
    handle e => DynException.checkpoint "ExpProcess.symterm2symterm" e

fun getLHSTerm exp = 
    symterm2symterm (exp2term (lhs exp))
    handle e => DynException.checkpoint "ExpProcess.getLHSTerm" e

fun getTermsfromTerm exp = 
    (case exp2term exp of
	 Exp.SYMBOL s => [Exp.SYMBOL s]
       | Exp.TUPLE terms => Util.flatmap (getTermsfromTerm o term2exp) terms
       | Exp.DONTCARE => []
       | _ => (error_no_return exp ("No valid symbols found on LHS");
	       [Exp.SYMBOL (Symbol.symbol "???", Property.default_symbolproperty)]))
    handle e => DynException.checkpoint "ExpProcess.getTermsfromTerm" e

fun getLHSTerms exp = 
    getTermsfromTerm (lhs exp)
    handle e => DynException.checkpoint "ExpProcess.getLHSTerms" e


fun term2sym term = 
    (case term of 
	 Exp.SYMBOL (sym,_) => sym
       | _ => (error_no_return (term2exp term) ("No valid symbol found on term");
	       Symbol.symbol "???"))
    handle e => DynException.checkpoint "ExpProcess.term2sym" e

fun getLHSSymbol exp = 
    term2sym(exp2term (lhs exp))
    handle e => DynException.checkpoint "ExpProcess.getLHSSymbol" e

fun exp2symbol exp =
    term2sym(exp2term (exp))
    handle e => DynException.checkpoint "ExpProcess.exp2symbol" e

fun getSymbolsfromTerm exp = 
    (case exp2term exp of
	 Exp.SYMBOL (sym,_) => [sym]
       | Exp.TUPLE terms => Util.flatmap (getSymbolsfromTerm o term2exp) terms
       | Exp.DONTCARE => []
       | _ => (error_no_return exp ("No valid symbols found on LHS");
	       [Symbol.symbol "???"]))
    handle e => DynException.checkpoint "ExpProcess.getSymbolsFromTerm" e

fun getLHSSymbols exp = 
    getSymbolsfromTerm (lhs exp)
    handle e => DynException.checkpoint "ExpProcess.getLHSSymbols" e

fun countTerms exp =
    length (Match.findRecursive (Match.anyterm "a", exp))

fun countFuns exp =
    length (Match.findRecursive (Match.anyfun "a", exp))


fun expEulerEqReformat (sym, titer, dt, exp) =
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
				 SOME (sym_in_symbol, _) => 
				 if sym = sym_in_symbol then
				     (* already assigned *)
				     true
				 else
				     let (* we have to see if it's an update/post process iterator *)
					 val iter = valOf (List.find (fn(sym',_)=>sym_in_symbol=sym') temporal_iterators)
					 val correct_sym_in_symbol = case iter of
									  (_, DOF.UPDATE sym) => sym
									| (_, DOF.ALGEBRAIC (_, sym)) => sym
									| (sym, _) => sym
					 val iter' = valOf (List.find (fn(sym',_)=>sym=sym') temporal_iterators)
					 val correct_sym_assignment = case iter' of
									  (_, DOF.UPDATE sym) => sym
									| (_, DOF.ALGEBRAIC (_,sym)) => sym
									| (sym, _) => sym
				     in
					 if correct_sym_in_symbol = correct_sym_assignment then 
					     true (* it's already assigned and it is matching *)
					 else
					     (Logger.log_error (Printer.$ ("Quantity " ^ (e2ps exp) ^ " is already assigned to iterator "^(Symbol.name sym)^", therefore can not use iterator "^(Symbol.name sym_in_symbol)^" to reference quantity.")); (* sym and sym_in_symbol seem backwards.  in general, the iterator passed into assignIteratorToSymbol is correct as it is generated from assignCorrectScope.  The one that was there originally is generally user defined and incorrect. *)
					      DynException.setErrored();
					      (*DynException.stdException(
					       ("Can't assign iterator '"^(Symbol.name sym)^"' over previously defined iterator on expression '"^(e2s exp)^"'"),
					       "ExpProcess.assignIteratorToSymbol",
					       Logger.INTERNAL)*)true)
				     end
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
    (let
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
     handle e => DynException.checkpoint ("ExpProcess.updateTemporalIterator [exp="^(e2s exp)^", new_iter="^(Iterator.iterator2str iter)^"]") e)
  | updateTemporalIterator iter _ = DynException.stdException("Non symbol encountered",
							      "ExpProcess.updateTemporalIterator",
							      Logger.INTERNAL)

(* here is a special function that will fix all the temporal iterators as is required for aggregating shards.  This is called by a rewrite rule in ShardedModel.combineDiscreteShards *)
(* this only works across symbols - the term rewriter will match only symbols *)
fun renameTemporalIteratorForAggregating (before_iter_sym_list, after_iter_sym) exp =
    (* first check to see if it is an iterator *)
    (case exp of
	Exp.TERM (Exp.SYMBOL (sym, props)) => 
	if List.exists (fn(sym')=>sym=sym') before_iter_sym_list then
	    Exp.TERM (Exp.SYMBOL (after_iter_sym, props))
	else
	    (case exp2temporaliterator exp of
		 SOME (iter_sym, iter_index) => if List.exists (fn(sym')=>iter_sym=sym') before_iter_sym_list then
						    replaceIterator {before_iter_sym=iter_sym,
								     before_iter_sym_list=before_iter_sym_list,
								     after_iter_sym=after_iter_sym,
								     sym=sym,
								     symprops = props,
								     iter_index=iter_index,
								     exp=exp}
						else 
						    exp (* not an iterator that matters *)
	       | NONE => exp) (* does not have a temporal iterator *)
      | _ => exp) (* not a symbol *)
    handle e => DynException.checkpoint "ExpProcess.renameTemporalIteratorForAggregating" e

and replaceIterator {before_iter_sym, before_iter_sym_list, after_iter_sym, sym, symprops, iter_index, exp} =
    let
	fun needsReplacing iter_sym = List.exists (fn(iter_sym')=>iter_sym=iter_sym') before_iter_sym_list
	fun updateIterSym iter_sym = if needsReplacing iter_sym then
					 after_iter_sym
				     else
					 iter_sym

	val (_, iter_type) = CurrentModel.itersym2iter before_iter_sym
	val isPostProcess = case iter_type of DOF.ALGEBRAIC (DOF.POSTPROCESS, _) => true | _ => false
	val isPreProcess = case iter_type of DOF.ALGEBRAIC (DOF.PREPROCESS, _) => true | _ => false
	val isUpdate = case iter_type of DOF.UPDATE _ => true | _ => false

	val spatial_iterators = exp2spatialiterators exp
	(* ignore derivative property - shouldn't exist *)
	val scope = Property.getScope symprops

	(* update the scope *)
	val scope' = case scope of
			 Property.READSTATE sym => 
			 (case iter_index of
			      Iterator.RELATIVE 0 => 
			      Property.READSTATE (updateIterSym sym)
			    | Iterator.RELATIVE 1 => 
			      Property.WRITESTATE (updateIterSym sym)
			    | _ => DynException.stdException(("Unexpected iterator1 ['"^(e2s exp)^"']"), "ExpProcess.replaceIterator", Logger.INTERNAL))
		       | Property.READSYSTEMSTATE sym => 
			 let val updated_sym = updateIterSym sym
			 in if not(needsReplacing sym) then
				scope
			    else
				case iter_index of
				    Iterator.RELATIVE 1 => Property.WRITESTATE updated_sym
				  | _ => Property.READSTATE updated_sym
			 end
		       | Property.WRITESTATE sym => 			 
			 (case iter_index of
			      Iterator.RELATIVE 0 => 
			      Property.READSTATE (updateIterSym sym)
			    | Iterator.RELATIVE 1 => 
			      Property.WRITESTATE (updateIterSym sym)
			    | Iterator.ABSOLUTE 0 => (* initial values for states will have this iterator type and need to be updated *)
			      Property.WRITESTATE (updateIterSym sym)
			    | _ => DynException.stdException(("Unexpected iterator2 ['"^(e2s exp)^"']"), "ExpProcess.replaceIterator", Logger.INTERNAL))
		       | _ => scope

	(* update system properties *)
	val symprops' = Property.setIterator (Property.setScope symprops scope') ((updateIterSym before_iter_sym, iter_index)::spatial_iterators)

    in
	Exp.TERM (Exp.SYMBOL (sym, symprops'))
    end
    handle e => DynException.checkpoint ("ExpProcess.replaceIterator [from '"^(Symbol.name before_iter_sym)^"' to '"^(Symbol.name after_iter_sym)^"']") e

fun assignCorrectScopeOnSymbol exp =
    (case exp 
      of Exp.TERM (s as (Exp.SYMBOL (sym, props))) => 
	 let
	     val iter = TermProcess.symbol2temporaliterator s
	     val iter' = case iter of 
			     SOME (iter_sym, iter_index) => SOME (iter_sym, iter_index, #2 (CurrentModel.itersym2iter iter_sym))
			   | NONE => NONE
	     fun isForwardReference (Iterator.RELATIVE i) = i > 0
	       | isForwardReference _ = false
					
	 in
	     case iter' 
	      of SOME (iter_sym, iter_index, iter_type) => 
		 if isFirstOrderDifferentialTerm exp then
		     Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol (Symbol.name iter_sym)))))
		 else if isNextVarDifferenceTerm exp then
		     Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol (Symbol.name iter_sym)))))
		 else if isAlgebraicStateTerm exp then
		     case iter_index of
			 Iterator.RELATIVE 1 => 
			 Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol (Symbol.name iter_sym)))))
		       | _ => 
			 Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSYSTEMSTATE iter_sym)))
 		 else if isNextUpdateTerm exp then
		     let
			 val orig_iter = case iter_type of DOF.UPDATE v => v | _ => DynException.stdException("Unexpected non update iterator", "ExpProcess.assignCorrectScope", Logger.INTERNAL)
		     in
			 Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.WRITESTATE (Symbol.symbol (Symbol.name orig_iter)))))
		     end
		 else if isCurVarDifferenceTerm exp then
		     let
			 val iter_sym' = case iter_type of DOF.UPDATE v => v | _ => iter_sym
		     in
			 Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSYSTEMSTATE iter_sym')))
		     end
		 else if isIntermediateTerm exp orelse isStateBufferTerm exp then
		     let
			 val iter_sym' = case iter_type of DOF.UPDATE v => v | _ => iter_sym
		     in
			 Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSYSTEMSTATE iter_sym')))
		     end
		 else if isInitialConditionTerm exp then
		     let val props = Property.setScope props (Property.WRITESTATE (Symbol.symbol (Symbol.name iter_sym)))
			 val props = Property.setEPIndex props (SOME Property.STRUCT_OF_ARRAYS)
		     in
			 Exp.TERM (Exp.SYMBOL (sym, props))
		     end
		 else if isForwardReference iter_index then
		     (user_error exp "Invalid positive temporal index found on quantity";
		      exp)
		 else
		     error exp "Unexpected expression found when assigning correct scope - unknown expression type"
	       | NONE => (*(Logger.log_error($("Unexpected expression '"^(e2s exp)^"' found when assigning correct scope - no temporal iterator"));
			    DynException.setErrored();*)
		 exp
	 end
       | _ => error exp "Unexpected expression found when assign correct scope - not a symbol")
    handle e => DynException.checkpoint "ExpProcess.assignCorrectScopeOnSymbol" e


fun assignCorrectScope states exp =
    (if isSymbol exp then
	 let
	     val sym = Util.hd (exp2symbols exp)
	 in
	     if List.exists (fn(sym')=>(sym=sym')) states then
		 assignCorrectScopeOnSymbol exp
	     else
		 exp
	 end
     else
	 (Match.head exp) (map (assignCorrectScope states) (Match.level exp)))
    handle e => DynException.checkpoint "ExpProcess.assignCorrectScope" e
	

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
	    

fun analyzeRelations relations = 
    let
	val sym2index = foldl (fn(((s, _, _), i), t) => SymbolTable.enter(t,s,i))
			      SymbolTable.empty 
			      (Util.addCount relations)

	fun ind sym = valOf (SymbolTable.look (sym2index, sym))
			  

	fun distance state dep = 
	    Int.abs ((ind state)-(ind dep))

	fun dep2str state dep = 
	    (Symbol.name dep) ^ ":" ^ (i2s (distance state dep))

	val _ = Util.log("All relationships:")
	fun relation2str (s, eq, deps) = 
	    (Symbol.name s) ^ " " ^
	    "(dep_count=" ^ (i2s (SymbolSet.numItems deps)) ^", " ^ 
	    "max_distance=" ^ (i2s (StdFun.max (map (distance s) (SymbolSet.listItems deps)))) ^ "): " ^
	    "{" ^ (String.concatWith ", " (map (dep2str s) (SymbolSet.listItems deps))) ^ "}"
	    
	val _ = app (Util.log o relation2str) relations

	val _ = Util.log ("")
		
    in
	()
    end


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
		    (matched::sorted, unmatched, addSetToList backlog_list deps)
		end
		
	(* recursive sortRelations list *)
	fun sortRelations (sorted, [], _) = sorted (* result *)
	  | sortRelations (var as (sorted, remaining, backlog)) = 
	    sortRelations (moveRelation var) (* recursive call *)
	    
    in
	sortRelations ([], relations, [])
    end
    handle e => DynException.checkpoint "ExpProcess.sortStatesByDependencies" e

end
