structure ModelProcess : sig

    (* Primary functions to execute optimizations and commands across all classes within a model *)

    (* Flattens all equations and instances into a single, monolithic model. *)
    val unify : DOF.model -> DOF.model

    (* optimizeModel: algebraic and performance optimizations all occur in this function.  All transformations
     * performed here should be independent of back-end.  If the data structure is to be saved prior to writing 
     * a particular back-end, the data structure returned from optimizeModel would be a good one to save.  
     * Depending on where this is called, we might want to perform an ordering step when redundancy eliminating. 
     * This is set through the first boolean argument. *)			     
    val optimizeModel : bool -> DOF.model -> unit

    (* normalizeModel and normalizeParallelModel: a normalization step for writing into a C back-end.  This 
      function performs transformations that are used solely for fitting within a back-end.  This
      can include renaming symbols to fit within compiler rules or adding code generation flags. *)
    val normalizeModel : DOF.model -> unit

    (* prune unused iterators from the system *)
    val pruneIterators : DOF.model -> unit

    (* model2statesizebyiterator: Computes the total state space of the model on a per iterator basis *)
    val model2statesize : DOF.model -> int
    val model2statesizebyiterator : DOF.systemiterator -> DOF.model -> int

(*    (* createIteratorForkedModels: Creates a structure list of models that are unique by iterator *)
    val createIteratorForkedModels : DOF.model -> {top_class: Symbol.symbol,
						   iter: DOF.systemiterator,
						   model: DOF.model} list
*)
    val duplicateModel : DOF.model -> (Symbol.symbol -> Symbol.symbol) -> DOF.model
    val pruneModel : (DOF.systemiterator option) -> DOF.model -> unit

	
    (* Iterator related functions - these all grab the iterators from CurrentModel *)
    val returnContinuousIterators : unit -> DOF.systemiterator list
    val returnUserIterators : unit -> DOF.systemiterator list
    val returnIndependentIterators : unit -> DOF.systemiterator list
    val returnDependentIterators : unit -> DOF.systemiterator list
    val returnStatefulIterators : unit -> DOF.systemiterator list
    val returnStatelessIterators : unit -> DOF.systemiterator list

    val hasUpdateIterator : Symbol.symbol -> bool
    val hasAlgebraicIterator : Symbol.symbol -> bool
    val requiresMatrixSolution : DOF.systemiterator -> bool

    (* Returns the list of algebraic iterators matching a given name. *)
    val algebraicIterators: Symbol.symbol -> DOF.systemiterator list

    (* Indicates whether an iterator is dependent upon another. *)
    val isDependentIterator : DOF.systemiterator -> bool 
    val isImmediateIterator : DOF.systemiterator -> bool
    (* Indicates whether an iterator can produce an output - all iterators except UPDATE *)
    val isOutputIterator : DOF.systemiterator -> bool
    val isStatefulIterator : DOF.systemiterator -> bool
    val isStatelessIterator : DOF.systemiterator -> bool

    val isDebugging : DOF.model -> bool
    val isProfiling : DOF.model -> bool

end = struct


fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$ str)

val i2s = Util.i2s

val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr

fun isContinuousIterator (_, DOF.CONTINUOUS _) = true
  | isContinuousIterator _ = false

fun isUserIterator (_, DOF.CONTINUOUS _) = true
  | isUserIterator (_, DOF.DISCRETE _) = true
  | isUserIterator _ = false

fun isDependentIterator (_, DOF.UPDATE _) = true
  | isDependentIterator (_, DOF.ALGEBRAIC _) = true
  | isDependentIterator _ = false

fun isImmediateIterator (_, DOF.IMMEDIATE) = true
  | isImmediateIterator _ = false

fun isOutputIterator (_, DOF.UPDATE _) = false
  | isOutputIterator _ = true

fun isStatefulIterator (_, DOF.CONTINUOUS _) = true
  | isStatefulIterator (_, DOF.DISCRETE _) = true
  | isStatefulIterator (_, DOF.ALGEBRAIC _) = true
  | isStatefulIterator _ = false

fun isStatelessIterator (_, DOF.UPDATE _) = true
  | isStatelessIterator (_, DOF.IMMEDIATE) = true
  | isStatelessIterator _ = false

val isStatefulIterator = not o isStatelessIterator

fun returnContinuousIterators () = 
    List.filter isContinuousIterator (CurrentModel.iterators())

fun returnUserIterators () = 
    List.filter isUserIterator (CurrentModel.iterators())

fun returnIndependentIterators () =
    List.filter (not o isDependentIterator) (CurrentModel.iterators ())

fun returnDependentIterators () =
    List.filter isDependentIterator (CurrentModel.iterators ())

fun returnStatelessIterators () =
    List.filter isStatelessIterator (CurrentModel.iterators ())

fun returnStatefulIterators () = 
    List.filter isStatefulIterator (CurrentModel.iterators ())

fun hasUpdateIterator iter_sym =
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(_,iter_type)=>case iter_type of
					  DOF.UPDATE v => v=iter_sym
					| _ => false) iterators
    end

(* Beginning at the top instance's class,
 * flatten the model into a single class. *)
fun unify model =
    let val (classes, instance, properties) : DOF.model = model
	val flatclass = CurrentModel.withModel model (fn _ => ClassProcess.unify DOFPrinter.printClass (CurrentModel.classname2class (#classname instance)))
			
    in
	([flatclass], instance, properties)
    end


    
fun hasAlgebraicIterator iter_sym =
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(_,iter_type)=>case iter_type of
					  DOF.ALGEBRAIC (_,v) => v=iter_sym
					| _ => false) iterators
    end

fun algebraicIterators iterName =
    List.filter (fn (_, DOF.ALGEBRAIC (_, name)) => name = iterName | _ => false) (CurrentModel.iterators ())
    

fun pruneModel iter_sym_opt model = 
    CurrentModel.withModel model (fn _ =>
    let val (classes, {classname,...}, _) = model
	fun isTop {name,...} = name = classname
    in 
	app (fn(c)=> ClassProcess.pruneClass (iter_sym_opt, isTop c) c) classes
      ; app (fn c => ClassProcess.pruneUnusedInputs c) classes
    end)

fun duplicateClasses classes namechangefun = 
    let
	(* create name mapping *)
	val orig_names = map ClassProcess.class2orig_name classes
	val name_mapping = map (fn({name,...},orig_name)=> (name, namechangefun name, orig_name, namechangefun orig_name)) (ListPair.zip(classes, orig_names))

	(* duplicate the classes *)
	val new_classes = 
	    map (fn((c as {name,...},(_,new_name,_,new_orig_name)))=> 
		   let
		       val c' = ClassProcess.duplicateClass c new_name
		       val c'' = ClassProcess.updateRealClassName c' new_orig_name
		   in
		       c''
		   end) (ListPair.zip (classes, name_mapping))

	(* update all the references in instances - this is O(n^2) - check for performance issues... *)
	val _ = app
		    (fn(name, new_name, orig_name, new_orig_name)=> app (fn(c as {name=name',...}) => ClassProcess.renameInsts ((name, new_name),(orig_name,new_orig_name)) c) new_classes)
		    name_mapping
    in
	new_classes
    end

fun duplicateModel model namechangefun = 
    let
	val (classes, top_inst as {name, classname}, props) = model
	val classes' = duplicateClasses classes namechangefun
	val classname' = namechangefun classname
	val top_inst' =  {name=name, classname=classname'}
    in
	(classes', top_inst', props)
    end
	

(* TODO: this should really be called at the end right before code gen so we don't create too many classes.  I still need to add the pruning step... *)

(* duplicate all classes by iterator and then remove the original classes *)
fun duplicateByIterator (model:DOF.model as (orig_classes, top_inst, props)) =
    let
	val iterators = CurrentModel.iterators()

	val new_classes_by_iterator = 
	    Util.flatmap
		(fn(iter_sym, _)=> duplicateClasses orig_classes (fn(name)=> Symbol.symbol ((Symbol.name name) ^ "_" ^ (Symbol.name iter_sym))))
		iterators
    in
	(new_classes_by_iterator, top_inst, props)
    end

fun model2statesize (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesize (CurrentModel.classname2class classname)
    end

fun model2statesizebyiterator (iter:DOF.systemiterator) (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesizebyiterator iter (CurrentModel.classname2class classname)
    end

fun pruneIterators (model:DOF.model as (classes, top_inst, properties)) =
    let
	val {iterators, precision, target, parallel_models, debug, profile} = properties

	(* is there an algebraic iterator that matches a particular iterator *)
	val algebraic_iterators = List.filter (fn(_, iter_type)=> case iter_type of DOF.ALGEBRAIC _ => true | _ => false) iterators
	fun find_matching_algebraic_iterators (iterator as (iter_sym,_)) = 
	    List.filter (fn(_, iter_type)=> case iter_type of DOF.ALGEBRAIC (_, iter_sym') => iter_sym=iter_sym' | _ => false) algebraic_iterators

	fun filter_iter iterator =
	    0 < model2statesizebyiterator iterator model orelse
	    List.exists
		(fn (class) => 0 < List.length (ClassProcess.inputsByIterator iterator class) orelse
			       0 < List.length (ClassProcess.outputsByIterator iterator class))
		classes orelse
	    let

		val matching_iterators = find_matching_algebraic_iterators iterator					 
		val result = foldl (fn(iterator',result)=> if result then
							       true
							   else 
							       filter_iter iterator') false matching_iterators
		(* do a quick error check - can't have a variable time step continuous iterator *)
		val _ = if result then
			    case iterator of
				(iter_sym, DOF.CONTINUOUS solver) => if Solver.isVariableStep solver then
									 (Logger.log_error (Printer.$("If iterator "^(Symbol.name iter_sym)^" has no states, it can not use a variable time step solver.  Iterator "^(Symbol.name iter_sym)^" was set to use the "^(Solver.solver2shortname solver)^" solver."));
									  DynException.setErrored())
								     else
									 ()
			      | _ => ()
			else
			    ()

	    in
		result
	    end
	    
	(* here are the iterators with states or outputs *)
	val iterators' = List.filter filter_iter iterators

	(* remove undefined iterators *)
	fun convertUndefined (iter_sym, DOF.CONTINUOUS Solver.UNDEFINED) = 
	    let
		(* set ODE45 as a default solver, should work in most cases *)
		val solver' = Solver.ODE45 {dt=0.1, abs_tolerance=0.000001, rel_tolerance=0.001}
		val _ = Logger.log_warning (Printer.$("No solver specified for continuous iterator '"^(Symbol.name iter_sym)^"', using default ode45 solver"))
	    in
		(iter_sym, DOF.CONTINUOUS solver')
	    end
	  | convertUndefined iter = iter
	val iterators'' = map convertUndefined iterators'				      

	val properties' = {iterators=iterators'',
			   precision=precision,
			   target=target,
			   parallel_models=parallel_models,
			   debug=debug,
			   profile=profile}
	val model' = (classes, top_inst, properties')
    in
	CurrentModel.setCurrentModel(model')
    end
    handle e => DynException.checkpoint "ModelProcess.pruneIterators" e

fun applyRewritesToModel rewrites (model as (classes,_,_)) =
    app (fn(c)=>ClassProcess.applyRewritesToClass rewrites c) classes

fun fixTemporalIteratorNames (model as (classes, inst, props)) =
    let
	val {iterators,precision,target,parallel_models,debug,profile} = props
	val iterators' =  map 
			      (fn(iter_sym, iter_type)=>
				 (Util.sym2codegensym iter_sym,
				  case iter_type of
				      DOF.UPDATE v => DOF.UPDATE (Util.sym2codegensym v)
				    | DOF.ALGEBRAIC (processtype, v) => DOF.ALGEBRAIC (processtype, Util.sym2codegensym v)
				    | _ => iter_type))
			      iterators
			      
	val iter_name_map = map
				(fn((sym,_),(sym',_))=>(sym, sym'))
				(ListPair.zip (iterators, iterators'))

	(* replace temporal iterators *)
	val iterator_rewrites = map
				    (fn(sym,sym')=>
				       let
					   val pred = ("Matching:"^(Symbol.name sym), (fn(exp)=> case ExpProcess.exp2temporaliterator exp of
												     SOME (iter_sym,_) => iter_sym=sym
												   | NONE => false))
					   val find = Match.anysym_with_predlist [PatternProcess.predicate_anysymbol, pred] (Symbol.symbol "a")
					   val test = NONE
					   val replace = Rewrite.ACTION (sym', (fn(exp)=>ExpProcess.updateTemporalIteratorToSymbol (sym',Util.sym2codegensym) exp))
				       in
					   {find=find,
					    test=test,
					    replace=replace}
				       end)
				    iter_name_map

	fun updateSymbolName new_symbol (Exp.TERM (Exp.SYMBOL (prev_symbol, props))) = Exp.TERM (Exp.SYMBOL (new_symbol, Property.setRealName props prev_symbol))
	  | updateSymbolName new_symbol _ = DynException.stdException("Unexpected non symbol matched", "ModelProcess.fixTemporalIteratorNames", Logger.INTERNAL)
				    
	(* replace iterators used explicitly *)
	val symbol_rewrites = map
			      (fn(sym,sym')=>
				 let
				     val find = Match.asym sym
				     val replace = Rewrite.ACTION (sym', updateSymbolName sym')
				 in
				     {find=find,
				      test=NONE,
				      replace=replace}
				 end)
			      iter_name_map

	val _ = applyRewritesToModel iterator_rewrites model
	val _ = applyRewritesToModel symbol_rewrites model
	val {iterators,precision,target,parallel_models,debug,profile} = props
	val props'={iterators=iterators',precision=precision,target=target,parallel_models=parallel_models,debug=debug,profile=profile}
    in
	(classes, inst, props')
    end

fun requiresMatrixSolution (_,iter_type) = 
    case iter_type of 
	DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _)=> true
      | _ => false
    
(* requiresFlattening - only requires it if it requires a matrix solver *)
fun requiresFlattening () =
    let
	val iterators = CurrentModel.iterators()

	fun solverRequiresFlattening (_,iter_type) =
	    case iter_type of
		DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _) => true
	      | DOF.CONTINUOUS (Solver.EXPONENTIAL_EULER _) => true
	      | DOF.CONTINUOUS (Solver.FORWARD_EULER _) => DynamoOptions.isFlagSet "aggregate"
	      | _ => false
    in
	(DynamoOptions.isFlagSet "flatten") orelse
	(List.exists solverRequiresFlattening iterators)
    end

			
fun optimizeModel order (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()
	val (classes, _, _) = model

	val _ = app 
		    (fn(c)=>
		       let
			   val msg = "Optimizing Class '"^(Symbol.name (#name c))^"'"
			   val _ = Profile.time msg ClassProcess.optimizeClass c
			   val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
		       in
			   ()
		       end) classes

	val _ = DynException.checkToProceed()

	val _ = if DynamoOptions.isFlagSet "redundancy" then
		    let
			val _ = Logger.log_notice (Printer.$("Removing redundancy ..."))
			val verbose = DynamoOptions.isFlagSet "verbose"
			val cost_before = if verbose then
					      Cost.model2cost model
					  else
					      0
			val _ = app ClassProcess.removeRedundancy classes
			val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())

		    in
			if verbose then
			    Logger.log_notice (Printer.$("Rendundancy elimination step before/after: "^(i2s cost_before)^"/"^(i2s (Cost.model2cost model))))
			else
			    ()
		    end
		else
		    ()

	val _ = DynException.checkToProceed()
    in
	()
    end


local
    fun create_replace_function (stateopt, iter_sym) =
	fn(exp) => 
	  (case exp of 
	      Exp.TERM (Exp.SYMBOL (sym, props)) => 
	      (case stateopt of 
		   SOME state =>
		   if sym = state andalso ExpProcess.hasTemporalIterator exp then
		       ExpProcess.updateTemporalIteratorToSymbol (iter_sym, fn(_)=>iter_sym) exp
		   else
		       exp
		 | NONE => if ExpProcess.hasTemporalIterator exp then
			       ExpProcess.updateTemporalIteratorToSymbol (iter_sym, fn(_)=>iter_sym) exp
			   else
			       exp)
	    | _ => exp)

    fun iter_table_to_rewrites table = 
	map 
	    (fn(s, (iter_sym, _))=>
	       {find=Match.asym s, 
		test=NONE,
		replace=Rewrite.ACTION (s, create_replace_function (SOME s, iter_sym))})
	    (SymbolTable.listItemsi table)

    fun iter_rewrites (from_sym, to_sym) =
	[(* find and replace literal iterators *)
	 {find=Match.asym from_sym,
	  test=NONE,
	  replace=Rewrite.RULE (ExpBuild.itersvar to_sym)},
	 (* find and replace use anywhere else of that iterator, such as in inputs or outputs *)
	 {find=Match.anysym_with_temporal_iterator from_sym "#a",
	  test=NONE,
	  replace=Rewrite.ACTION (to_sym, create_replace_function (NONE, to_sym))}]

    fun updateSystemProperties sysprops (auto_iter as (auto_iter_sym, _), fe_iter as (fe_iter_sym, _)) other_iters =
	let
	    val {iterators, precision, target, parallel_models, debug, profile} = sysprops

	    (* first replace auto_iter with fe_iter *)
	    val iterators' = map (fn(iter as (iter_sym, iter_type)) => 
				    if iter_sym = auto_iter_sym then 
					fe_iter
				    else 
					case iter_type of
					    DOF.UPDATE iter_sym' => 
					    if iter_sym' = auto_iter_sym then
						(iter_sym, DOF.UPDATE fe_iter_sym)
					    else
						iter
					  | DOF.ALGEBRAIC (processtype, iter_sym') => 
					    if iter_sym' = auto_iter_sym then
						(iter_sym, DOF.ALGEBRAIC (processtype, fe_iter_sym))
					    else
						iter
					  | _ => iter
				 ) iterators

	    (* now, append all the new ones *)
	    val append_list = List.filter (fn(iter as (iter_sym, _)) => not (iter_sym = fe_iter_sym)) other_iters
	    val iterators'' = iterators' @ append_list
	in
	    {iterators=iterators'',
	     precision=precision,
	     target=target,
	     parallel_models=parallel_models,
	     debug=debug,
	     profile=profile}
	end

    fun log_linearity_table msg table = 
	let
	    open Layout
	    fun entry_to_layout (key, (value1, value2)) =
		label (Symbol.name key, align [label ("linear", SymbolSet.toLayout value1),
					       label ("nonlinear", SymbolSet.toLayout value2)])
	    val t = heading (msg, 
			     align (map entry_to_layout (SymbolTable.listItemsi table)))
	in
	    log (add_newline t)
	end

    fun log_groups msg groups = 
	let
	    open Layout
	    val t = heading (msg,
			     align (map SymbolSet.toLayout groups))
	in
	    log (add_newline t)
	end

    fun except msg = DynException.stdException (msg, "ModelProcess.ExpandAutoSolver", Logger.INTERNAL)

    fun group_next_element table (grouped, []) = grouped
      | group_next_element table (grouped, next::ungrouped) = 
	let
	    fun state_to_sets state = 
		case SymbolTable.look (table, state) of
		    SOME sets => sets
		  | NONE => except "No state in table"

	    fun recurse (linear_set : SymbolSet.set) =
		let
		    val list = SymbolSet.listItems linear_set
		    val linear_set' = SymbolSet.union (linear_set, SymbolSet.flatmap (#1 o state_to_sets) list)
		    val nonlinear_set' = SymbolSet.flatmap (#2 o state_to_sets) list
		    (*val _ = Util.log ("Recurse: Going from "^(SymbolSet.toStr linear_set)^" to " ^ (SymbolSet.toStr linear_set'))*)
		in
		    if SymbolSet.equal (linear_set, linear_set') then
			(* here, we bottomed out *)
			(linear_set, nonlinear_set')
		    else
			recurse linear_set'
		end

	    val (linear_set, nonlinear_set) = recurse (SymbolSet.singleton next)
	    (*val _ = Util.log ("Linear set: "^(SymbolSet.toStr linear_set)^", Nonlinear set: " ^ (SymbolSet.toStr nonlinear_set))*)
	    val common = SymbolSet.intersection (linear_set, nonlinear_set)

	    (* compute group now *)
	    val group = if SymbolSet.numItems common = 0 andalso SymbolSet.numItems linear_set > 1 then
			    linear_set
			else
			    SymbolSet.singleton next
	    val remaining = SymbolSet.listItems (SymbolSet.difference (SymbolSet.fromList ungrouped, group))
	    (*val _ = Util.log ("Group: "^(SymbolSet.toStr group)^", Remaining: " ^ (Util.symlist2s remaining))*)
	in
	    group_next_element table (group::grouped, remaining)
	end

    fun group_linearity_table table = 
	let
	    (* create initial sets *)
	    val grouped = []
	    val ungrouped = SymbolTable.listKeys table
	in
	    group_next_element table (grouped, ungrouped)
	end

    fun replaceAutoIterator (auto_iter as (auto_iter_sym, DOF.CONTINUOUS (Solver.AUTO {dt}))) =
	(let
	    val model as (classes, inst, systemproperties) = CurrentModel.getCurrentModel()
	    val class = CurrentModel.top_class()
			
	    (* find all the states with that iterator *)
	    val (state_equs, other_equs) = List.partition (ExpProcess.isStateEqOfIter auto_iter) (!(#exps class))
	    val (init_equs, other_equs) = List.partition ExpProcess.isInitialConditionEq other_equs
	    val states = map ExpProcess.getLHSSymbol state_equs

	    (* create a table mapping the states with their new iterator *)
	    val itertable = SymbolTable.empty
	    val newiterlist = []

	    (* create a useful function to return the differential equation matching a state *)
	    fun sym2differential_equation s = 
		List.find ExpProcess.isFirstOrderDifferentialEq (ClassProcess.symbol2exps class s)

	    (* create a table of linearity relationships *)
	    val linearity_table = 
		foldl
		    (fn(s, table)=> 
		       case sym2differential_equation s of
			   SOME equ => 
			   let
			       val rhs = ExpProcess.rhs equ
			       val terms = ExpProcess.exp2termsymbols rhs
			       val state_vars = map Exp.TERM (List.filter (fn(t)=> List.exists (fn(s)=> Term.sym2curname t = s) states) terms)
			       val state_syms = map ExpProcess.exp2symbol state_vars
			       val state_symbolset = SymbolSet.fromList state_syms
			       (*val _ = Util.log ("Original equation: " ^ (e2s equ))
			       val _ = Util.log (" -> Symbols: " ^ (SymbolSet.toStr state_symbolset))*)
			       val rhs' = ExpProcess.multicollect (map (fn(sym)=>ExpBuild.svar sym) (SymbolSet.listItems state_symbolset), rhs)
			       (*val _ = Util.log (" -> new rhs: " ^ (e2s rhs'))*)
			       val (linear_states, non_linear_states) = SymbolSet.partition (fn(sym)=> ExpProcess.isLinear (rhs', sym)) state_symbolset
			   in
			       SymbolTable.enter (table, s, (linear_states, non_linear_states))
			   end
			 | NONE => table)
		    SymbolTable.empty
		    states
		    handle e => DynException.checkpoint "ModelProcess.replaceAutoIterator.linearity_table" e

	    (*val _ = log_linearity_table "All dependencies" linearity_table*)

	    val groups = group_linearity_table linearity_table
	    (*val _ = log_groups "Grouped dependencies" groups*)

	    (* define new iterators for the system *)
	    val fe_iter_sym = ExpProcess.uniq (Symbol.symbol "#iter")
	    val fe_iter = (fe_iter_sym, DOF.CONTINUOUS (Solver.FORWARD_EULER {dt=dt}))
	    val ee_iter_sym = ExpProcess.uniq (Symbol.symbol "#iter")
	    val ee_iter = (ee_iter_sym, DOF.CONTINUOUS (Solver.EXPONENTIAL_EULER {dt=dt}))
	    val discrete_iter_sym = ExpProcess.uniq (Symbol.symbol "#iter")
	    val discrete_iter = (discrete_iter_sym, DOF.DISCRETE {sample_period=dt})
	    fun be_iter () = (ExpProcess.uniq (Symbol.symbol "#iter"), DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER {dt=dt, solv=Solver.LSOLVER_DENSE}))

	    (* helper function to determine if exponential euler applies *)
	    fun isLinearToSelf sym =
		case SymbolTable.look (linearity_table, sym) of
		    SOME (linear_set, nonlinear_set) => SymbolSet.equal (linear_set, SymbolSet.singleton sym)
		  | NONE => except "Can't determine linearity of self"

	    fun log (msg) = 
		if DynamoOptions.isFlagSet "verbose" then
		    Util.log msg
		else
		    ()

	    (* go through the groups and apply iterators as needed *)
	    val (itertable, newiterlist) = 
		foldl
		    (fn(group, (table, iterlist))=>
		       case SymbolSet.listItems group of
			   [sym] => if isLinearToSelf sym then
					(log ("Assiging exp_euler to state " ^ (Symbol.name sym));
					 (SymbolTable.enter (table, sym, ee_iter), iterlist))
				    else
					(log ("Assiging fwd_euler to state " ^ (Symbol.name sym));
					 (SymbolTable.enter (table, sym, fe_iter), iterlist))
			 | syms => 
			   let
			       (* create a unique be iterator for this group *)
			       val iter = be_iter()
			       val iterlist' = iter::iterlist
			       val _ = log ("Assiging bwd_euler to states " ^ (Util.symlist2s syms))
			   in
			       (foldl
				    (fn(sym, table')=>SymbolTable.enter (table', sym, iter))
				    table
				    syms,
				iterlist')
			   end)
		    (itertable, newiterlist)
		    groups

	    (* if there is no other iterator assigned, use forward Euler or a discrete iterator *)
	    val newiterlist = ee_iter::fe_iter::discrete_iter::newiterlist
	    val itertable = foldl 
				(fn(s, table)=> case SymbolTable.look (itertable, s) of
						    SOME _ => table
						  | NONE => 
						    (case sym2differential_equation s of
							 SOME _ => SymbolTable.enter (table, s, fe_iter)
						       | NONE => SymbolTable.enter (table, s, discrete_iter)))
				itertable
				states

	    (* update all the states with the new iterators *)
	    val state_equs' = map (Match.applyRewritesExp (iter_table_to_rewrites itertable)) state_equs
	    val init_equs' = map (Match.applyRewritesExp (iter_table_to_rewrites itertable)) init_equs
	    val _ = #exps class := (init_equs' @ state_equs' @ other_equs)

	    (* replace all other occurrences of auto_iter with fe_iter *)
	    val _ = ClassProcess.applyRewritesToClass (iter_rewrites (auto_iter_sym, fe_iter_sym)) class
		    
	    (* update the model definition with the new iterator list *)
	    val model' = (classes, inst, updateSystemProperties systemproperties (auto_iter, fe_iter) newiterlist)
	in
	    CurrentModel.setCurrentModel(model')
	end
	handle e => DynException.checkpoint ("ModelProcess.replaceAutoIterator [iter="^(Symbol.name auto_iter_sym)^"]") e)
      | replaceAutoIterator _ = 
	DynException.stdException ("called with invalid arguments", "ModelProcess.replaceAutoIterator", Logger.INTERNAL)
in
(* expandAutoSolver - replaces the auto solver with actual implementable solvers *)
fun expandAutoSolver (model:DOF.model) =
    let
	(* first flatten the model *)
	val model' = Profile.time "Unifying" unify model
	val _ = CurrentModel.setCurrentModel(model')

	(* find all the auto iterators *)
	fun isAuto (_, DOF.CONTINUOUS (Solver.AUTO _)) = true
	  | isAuto _ = false
	val auto_iterators = List.filter isAuto (CurrentModel.iterators())

    in
	(* now replace them one by one*)
	app replaceAutoIterator auto_iterators
    end
    handle e => DynException.checkpoint "ModelProcess.expandAutoSolver" e
end

fun normalizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, {name,classname}, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

	val _ = if DynamoOptions.isFlagSet "generateMathematica" then
		    CurrentModel.withModel (CurrentModel.getCurrentModel())
		    (fn()=>
		       (log ("Creating Mathematica model description (first propagating state iterators) ...");
			app ClassProcess.propagateStateIterators (CurrentModel.classes()); (* pre-run the assignCorrectScope *)
			log ("Creating Mathematica model description (converting model to code) ...");
			Printer.progs2file (MathematicaWriter.model2progs (CurrentModel.getCurrentModel()), Symbol.name classname ^ ".nb")))
		else
		    ()

	(* expand out auto-solver *)
	fun isAuto (_, DOF.CONTINUOUS (Solver.AUTO _)) = true
	  | isAuto _ = false
	val _ = if List.exists isAuto (CurrentModel.iterators()) then
		    (log ("Expanding auto-solver ...");
		     Profile.time "Expanding auto-solver" (fn()=>expandAutoSolver (CurrentModel.getCurrentModel())) ();
		     DOFPrinter.printModel (CurrentModel.getCurrentModel());
		     DynException.checkToProceed())
		else
		    ()

	(* assign correct scopes for each symbol *)
	val _ = log ("Creating event iterators ...")
	val () = Profile.time "Creating event iterators" (fn()=>app ClassProcess.createEventIterators (CurrentModel.classes())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* expand out delays *)
	val _ = log ("Adding delays to difference equations")
	val () = Profile.time "Adding difference equation delays" (fn()=>app ClassProcess.addDelays (CurrentModel.classes())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	val _ = log ("Assigning correct scope ...")
	val () = Profile.time "Assigning correct scope" (fn()=>app ClassProcess.assignCorrectScope (CurrentModel.classes())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

(*	val _ = log ("Propagating temporal iterators ...")
	val () = app ClassProcess.propagatetemporalIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
*)
	val _ = log ("Propagating spatial iterators ...")
	val () = Profile.time "Propagating spatial iterators" (fn()=>app ClassProcess.propagateSpatialIterators (CurrentModel.classes())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	val _ = log ("Pruning excess iterators ...")
	val () = Profile.time "Pruning excess iterators" (fn()=>pruneIterators (CurrentModel.getCurrentModel())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* generate all offsets for instances *)
	(*val () = app ClassProcess.generateOffsets classes*)

(*
	(* reorder all the statements *)
	val () = app 
		     (fn(class)=> 
			let
			    val eqs' = EqUtil.order_eqs (!(#eqs class))
			in
			    (#eqs class) := eqs'
			end) 
		     classes
	*)

	val _ = if requiresFlattening() then
		    let
			val _ = log ("Flattening model ...")
			val _ = Profile.write_status "Flattening model"
			val model' = Profile.time "Unifying " unify (CurrentModel.getCurrentModel())
			val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())
			val _ = log ("Optimizing ...")
			val _ = Profile.time "Optimizing" optimizeModel model'
			val _ = CurrentModel.setCurrentModel(model')
			val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())
		    in
			()
		    end
		else
		    ()
		     


	(* remap all names into names that can be written into a back-end *)
	val _ = log ("Fixing symbol names ...")
	val model' = fixTemporalIteratorNames(CurrentModel.getCurrentModel())
	val _ = CurrentModel.setCurrentModel(model')
	val () = (app ClassProcess.fixSymbolNames (CurrentModel.classes()))
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = Profile.mark()


	val _ = DynException.checkToProceed()

	val _ = log ("Adding EP index to class ...")
	val () = app (ClassProcess.addEPIndexToClass false) (CurrentModel.classes())
	val top_class = CurrentModel.classname2class (#classname (CurrentModel.top_inst()))
	val () = ClassProcess.addEPIndexToClass true top_class

	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* add intermediates for update equations if required - they are reading and writing to the same vector so we have to make sure that ordering doesn't matter. *)
	val _ = log ("Adding buffered intermediates ...")
	val () = Profile.time "Adding buffered intermediates" (fn()=>app ClassProcess.addBufferedIntermediates (CurrentModel.classes())) ()
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()



    in
	() (* all changes are in the model, we don't have to return this model *)
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeModel" e


fun isDebugging model = 
    let val (_, _, props : DOF.systemproperties) = model
    in #debug props
    end

fun isProfiling model =
    let val (_, _, props : DOF.systemproperties) = model
    in #profile props
    end


end
