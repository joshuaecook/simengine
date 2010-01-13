signature SHARDEDMODEL =
sig

    type shard = {classes: DOF.class list,
		  instance: DOF.instance,
		  iter_sym: Symbol.symbol}
    type shardedModel = (shard list * DOF.systemproperties)
			
    val forkModel : DOF.model -> shardedModel
				 
    val updateShardForSolver : DOF.systemproperties -> (shard * DOF.systemiterator) -> (shard * DOF.systemiterator)

    (* shardedModel utilities *)
    val toModel : shardedModel -> Symbol.symbol -> DOF.model
    val toInputs : shardedModel -> DOF.input list
    val toOutputs : shardedModel -> DOF.output list
    val toIterator : shardedModel -> Symbol.symbol -> DOF.systemiterator

    (* pull out a list of iterator symbols *)
    val iterators : shardedModel -> Symbol.symbol list

end
structure ShardedModel : SHARDEDMODEL =
struct

type shard = {classes: DOF.class list,
	      instance: DOF.instance,
	      iter_sym: Symbol.symbol}

type shardedModel = (shard list * DOF.systemproperties)		    

fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  ()
val i2s = Util.i2s
val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr


fun updateShardForSolver systemproperties (shard as {classes, instance, ...}, iter as (itername, DOF.CONTINUOUS solvertype)) =
    let
	val model = (classes, instance, systemproperties)
    in
	(case solvertype of 
	     Solver.EXPONENTIAL_EULER {dt=dtval} => 
	     (let
		  (* at this point, there should be just one flattened class *)
		  val flatclass = case model of
				      ([class], _, _) => class
				    | _ => DynException.stdException ("Flattening resulted in more than one class", 
								      "ShardedModel.updateSHardForSolver.EXPONENTIAL_EULER", 
								      Logger.INTERNAL)

		  (* get list of all states *)
		  val states = ClassProcess.class2states flatclass
		  val stateSet = SymbolSet.fromList states

		  (* grab all the equations *)
		  val exps = !(#exps flatclass)

		  (* rewrite each equation, one state at a time *) 
		  fun rewriteEquationForExponentialEuler (state, eq) =
		      let
			  (* pull out the LHS term *)
			  val lhsterm = ExpProcess.getLHSTerm eq

			  (* for debugging *)
			  fun state_str() = e2ps (Exp.TERM lhsterm)

			  fun setAsReadState (Exp.TERM (Exp.SYMBOL (sym, props))) =
			      Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSTATE itername)))
			    | setAsReadState _ = DynException.stdException("Unexpected non-symbol",
									   "ShardedModel.updateShardForSolver.rewriteEquationForExponentialEuler.setAsReadState",
									   Logger.INTERNAL)
			  fun varfromsym sym = 
			      setAsReadState (ExpBuild.avar (Symbol.name sym) (Symbol.name itername))
			  val var = varfromsym state

			  (* first thing is to factor out the state from the rhs of the equation *)
			  val rhs = ExpProcess.rhs eq
			  val exp' = ExpProcess.multicollect ([var], rhs)
			  (*val _ = Util.log ("Collected expression: " ^ (e2s exp'))*)

			  (* create a rewrite to match the resulting equation *)
			  val coeff_rewrite =
			      {find=ExpBuild.plus[Match.any "d1", 
						  ExpBuild.times [Match.any "d2", 
								  var, 
								  Match.any "d3"], 
						  Match.any "d4"],
			       test=NONE,
			       replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.times [ExpBuild.pvar "d2", ExpBuild.pvar "d3"],
									      ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}
			  (* run a rewrite to pull out the coeff and remainder *)
			  val (coeff, remainder) = 
			      case Match.applyRewriteExp coeff_rewrite exp' of
				  Exp.CONTAINER(Exp.EXPLIST [coeff, remainder]) =>
				  (coeff, remainder)
				| _ =>
				  (Logger.log_error (Printer.$("Cannot factor out state '"^(state_str())^"' from expression '"^(e2ps exp')^"'.  The system may be nonlinear."));
				   DynException.setErrored();
				   (ExpBuild.int 0, rhs))
				  
			  (* Verify that remainder and coefficient does not contain 'state' (indicating non-linearity) *)  		
			  val _ = case Match.findOnce (Match.asym state, ExpBuild.explist [coeff, remainder]) of
				      SOME e =>
				      Logger.log_warning (Printer.$("Nonlinear equation for state " ^ (state_str()) ^ " is not ideal for exponential euler. Exponential-euler assumes that the stiffness of an equation is concentrated in its linear term.  Having non-linear terms violates that assumption. Eq: " ^ (e2ps eq)))
				    | NONE => ()

			  (* Exponential Euler requires that the equation is of the form dy/dt = A-B*y *)
			  (* Therefore, A = remainder and B = -coeff *)
			  val A = remainder
			  val B = ExpBuild.neg (coeff)
			  (*val _ = Util.log("A = " ^ (e2s A))
			  val _ = Util.log("B = " ^ (e2s B))*)
			  val y = var
			  val dt = ExpBuild.real dtval
			  val e = ExpBuild.exp
			  fun add(a,b) = ExpBuild.plus [a, b]
			  fun sub(a,b) = ExpBuild.sub (a, b)
			  fun mul(a,b) = ExpBuild.times [a, b]
			  fun divide(a,b) = ExpBuild.divide (a, b)
			  val one = ExpBuild.int 1
			  val neg = ExpBuild.neg

			  (* the transformation looks like: *)
			  (* y(t+h) = y(t)*e^(-B*dt)+(A/B)*(1-e^(-B*dt))  *)
			  (*val rhs' = add(mul(y, e(mul(neg(B),dt))),
					 mul(divide(A,B),sub(one, e(mul(neg(B),dt)))))*)
			  (* slightly more efficient implementation *)
			  val rhs' = add(y,mul(sub(divide(A,B),y),sub(one,e(mul(neg(B),dt)))))

			  (* transform the lhs term y'[t] into y[t+1] *)
			  val lhsterm' = case lhsterm of
					     Exp.SYMBOL (sym, props) => 
					     let
						 val derivative = Property.getDerivative props
						 val derivative' = 
						     case derivative of
							 SOME (1, symlist) => ()
						       | _ => DynException.stdException (("Original equation '"^(e2s eq)^"' is not a first order differential equation"), "ShardedModel.updateShardForSolver.[EXPONENTIAL_EULER].setAsReadState", Logger.INTERNAL)
						 val props' = Property.clearDerivative props
						 val iterators = Property.getIterator props'
						 val iterators' = case iterators of
								      SOME iters =>
								      (map (fn(sym,index)=>
									      if sym=itername then
										  case index of
										      Iterator.RELATIVE 0 => (sym, Iterator.RELATIVE 1)
										    | _ => DynException.stdException(("Unexpected iterator found in lhs symbol"), 
"ShardedModel.updateShardForSolver.[EXPONENTIAL_EULER].setAsReadState", 
														     Logger.INTERNAL) iters
									      else
										  (sym, index))
									   iters)
								    | NONE => DynException.stdException (("Original equation '"^(e2s eq)^"' does not have any defined iterators"), "ShardedModel.updateShardForSolver.[EXPONENTIAL_EULER].setAsReadState", Logger.INTERNAL)
						 val props'' = Property.setIterator props' iterators'
					     in
						 Exp.SYMBOL (sym, props'')
					     end
					   | _ => DynException.stdException(("No valid symbol found"),
									    "ShardedModel.updateShardForSolver.[EXPONENTIAL_EULER].setAsReadState",
									    Logger.INTERNAL)

			  (* Create updated equation *)
			  val eq' = ExpBuild.equals (Exp.TERM lhsterm', ExpProcess.simplify rhs')
					     
		      in
			  eq'
		      end
		      handle e => DynException.checkpoint ("ShardedModel.updateShardForSolver.[EXPONENTIAL_EULER].rewriteEquationForExponentialEuler [state="^(Symbol.name state)^"]") e

		  (* loop through each equation, pulling out differential equations for remapping *)
		  val exps' = map (fn(eq)=> 
				     if ExpProcess.isFirstOrderDifferentialEq eq then
					 rewriteEquationForExponentialEuler (ExpProcess.getLHSSymbol eq, eq)
				     else
					 eq)
				  exps

		  (* update exps in class *)
		  val _ = #exps flatclass := exps'

		  (* change the form of the iterator *)
		  val iter' = (itername, DOF.DISCRETE {sample_period=dtval})

	      in
		  (shard, iter')
	      end)
	   | Solver.LINEAR_BACKWARD_EULER {dt, solv} =>
	     (let
 		  (*val _ =  
			(log("\n ============ pre-unified model ============ \n");
			 DOFPrinter.printModel model)
			
		   (* flatten model *)
		   val _ = log("Flattening model ...")
		   val model' = unify model			  

 		   val _ =  
		       (log("\n ============ unified model ============ \n");
			DOFPrinter.printModel model')*)

		  
		  val flatclass = case model of
				      ([class], _, _) => class
				    | _ => DynException.stdException ("Flattening resulted in more than one class", 
								      "ShardedModel.updateSHardForSolver.LINEAR_BACKWARDS_EULER", 
								      Logger.INTERNAL)

		  val (_, instance, sysprops) = model 

		  (* get list of all states *)
		  val states = ClassProcess.class2states flatclass

		  val stateSet = SymbolSet.fromList states

		  (* compute state relationships *)
		  fun computeRelationships state =
		      let
			  val stateeq = 
			      case List.filter ExpProcess.isStateEq (ClassProcess.symbol2exps flatclass state)
			       of eq::_ => eq
				| nil => DynException.stdException ("Couldn't find state equation for '" ^ (Symbol.name state) ^"'",
								    "ShardedModel.updateShardForSolver.computeRelationships", 
								    Logger.INTERNAL)

			  val rhs = ExpProcess.rhs stateeq
			  (*val _ = Util.log ("StateEq: " ^ (e2s stateeq))*)

			  val usedSyms = SymbolSet.add(ExpProcess.exp2symbolset rhs, state)
		      (*val _ = Util.log("in computeRelationships: state=" ^ (Symbol.name state) ^ ": usedSyms=" ^ (SymbolSet.toStr usedSyms))
		       val _ = Util.log("in computeRelationships: state=" ^ (Symbol.name state) ^ ": deps=" ^ (SymbolSet.toStr (SymbolSet.intersection (stateSet, usedSyms))))*)
		      in
			  (state, stateeq, SymbolSet.intersection (stateSet, usedSyms))
		      end
		      handle e => DynException.checkpoint "ShardedModel.updateShardForSolver.computeRelationships" e
				  

		  val _ = log ("Computing dependencies  ... ")
		  val relations = map computeRelationships states
		  (*val _ = ExpProcess.analyzeRelations relations*)

		  (* order the states to make matrix banded *)
			  
		  val _ = log ("Ordering relationships ...")
		  val orderedRelationships = ExpProcess.sortStatesByDependencies relations
		  (*val _ = ExpProcess.analyzeRelations orderedRelationships*)
		  (*val _ = DynException.exit()*)

		  val numberedRelationships = (ListPair.zip (orderedRelationships, 
							     List.tabulate (length orderedRelationships, fn(i) => i)))

		  val sym2index = foldl (fn(((s, _, _), i), t) => SymbolTable.enter(t,s,i))
					SymbolTable.empty 
					numberedRelationships

   		  (* populate matrix*)
		  val matrix = Container.expMatrixToMatrix(Container.zeros_matrix (length states, length states))
		  (*val _ = print (" initial matrix -> ")
		  val _ = Matrix.print matrix*)

		  val bvector = Container.expArrayToArray (Container.zeros_array (length states))

		  fun buildIteratorUpdate state =
		      {find=ExpBuild.avar (Symbol.name state) (Symbol.name itername),
		       test=NONE,
		       replace=Rewrite.ACTION (Symbol.symbol ("t->t+1 on " ^ (Symbol.name state)), 
					       ExpProcess.updateTemporalIterator (itername, Iterator.RELATIVE 1))}

		  val iteratorUpdateRules = map buildIteratorUpdate states

		  fun setAsReadState (Exp.TERM (Exp.SYMBOL (sym, props))) =
		      Exp.TERM (Exp.SYMBOL (sym, Property.setScope props (Property.READSTATE itername)))
		    | setAsReadState _ = DynException.stdException("Unexpected non-symbol",
								   "ShardedModel.updateShardForSolver.setAsReadState",
								   Logger.INTERNAL)

		  fun updateMatrix ((state, eq, deps), rowIndex) =		
		      let
			  (*val _ = log ("Updating matrix for state '"^(Symbol.name state)^"': "^(i2s (rowIndex+1))^" of " ^ (i2s (List.length states)))*)
			  (* backwards euler transformation *)
			  (* update rhs to make any states in matrix [t+1] from [t] *)
			  val rhs' = Match.repeatApplyRewritesExp iteratorUpdateRules (ExpProcess.rhs eq)

			  (* for debugging *)
			  fun state_str () = e2ps (ExpProcess.lhs eq)
				     
			  (* plug rhs into x[t+1] = x[t] + dt * rhs' *)
			  (* subtract lhs from rhs, making an expression*)
			  (* so really, we're doing exp = x[t] + dt * rhs' - x[t+1] (implicitly == 0) *)

			  fun varfromsym sym = 
			      setAsReadState (ExpBuild.avar (Symbol.name sym) (Symbol.name itername))

			  fun nextvarfromsym sym = 
			      let
				  val base_var = varfromsym sym
				  val next_var = ExpProcess.updateTemporalIterator (itername, Iterator.RELATIVE 1) base_var
			      in
				  next_var
			      end

			  val exp = ExpBuild.plus [varfromsym state,
						   ExpBuild.times [ExpBuild.real dt, rhs'],
						   ExpBuild.times [ExpBuild.int (~1), (nextvarfromsym state)]] 
				    
			  (* collect *)
			  val exp' = ExpProcess.multicollect (map nextvarfromsym (SymbolSet.listItems deps), exp)
			  (*val _ = Util.log("Calling multicollect: from '"^(e2s exp)^"' to '"^(e2s exp')^"'")*)

			  (* for each dep (column) in the row: *)	
			  fun addEntry (statedep, exp) =
			      let
				  (*val _ = Util.log("In addEntry for state '"^(Symbol.name statedep)^"', exp: "^(e2s exp))*)
				  (* pull out coefficient for statedep from collected eq *)
				  fun extractCoefficient sym =
				      let
					  val var = ExpProcess.updateTemporalIterator (itername, Iterator.RELATIVE 1) 
										      (ExpBuild.nextavar (Symbol.name sym) itername)
					  val coeff_rewrite = 
					      {find=ExpBuild.plus[Match.any "d1", 
								  ExpBuild.times [Match.any "d2", 
										  var, 
										  Match.any "d3"], 
								  Match.any "d4"],
					       (* use a test to make sure that we're only looking at the top most expression *)
					       test=SOME (fn(exp', matchedPatterns)=>ExpEquality.equiv (exp, exp')),
					       replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.times [ExpBuild.pvar "d2", ExpBuild.pvar "d3"],
											      ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}
					      
				      in
					  case Match.applyRewriteExp coeff_rewrite exp of
					      Exp.CONTAINER(Exp.EXPLIST [coeff, remainder]) =>
					      (coeff, remainder)
					    | _ => (ExpBuild.int 0, exp)
						   
				      end
				      handle e => DynException.checkpoint
						      "ShardedModel.updateShardForSolver.updateMatrix.addEntry.extractCoefficient"
						      e

						      
				  val (coeff, remainder) = extractCoefficient statedep
				  (*val _ = Util.log (" -> coeff="^(e2s coeff)^", remainder="^(e2s remainder))*)

				  (* insert coefficient into matrix at (rowindex, sym2index statedep)*)
				  val columnIndex = case SymbolTable.look(sym2index, statedep) of
							SOME i => i
						      | NONE => DynException.stdException ("Unknown symbol dependency: " ^ (Symbol.name statedep), 
											   "ShardedModel.updateSHardForSolver.LINEAR_BACKWARDS_EULER.updateMatrix.addEntry", 
											   Logger.INTERNAL)
								
				  val _ = Matrix.setIndex matrix (rowIndex, columnIndex) (ExpProcess.simplify coeff)
			      (* val _ = Array2.update (matrix, rowIndex, columnIndex, ExpProcess.simplify coeff)*)

			      in
				  (coeff, remainder)
			      end
			      handle e => DynException.checkpoint "ShardedModel.updateShardForSolver.updateMatrix.addEntry" e
					  
			  val (coefficients, b_entry) = foldl (fn(dep, (coefficients, remainder))=> 
								 let
								     val (coeff, remainder') = addEntry (dep, remainder)
								 in
								     (coeff::coefficients, remainder')
								 end) ([],exp') (SymbolSet.listItems deps)

			  (* we need to negate the b_entry - since we're constructing Ax-b=0 instead of Ax=b *)
			  val neg_b_entry = ExpBuild.neg (b_entry)

			  (* Verify that remainder/coefficients do not contain states with [t+1] iterators (indicating non-linearity) *)  		
			  val preds = [("is symbol", ExpProcess.isSymbol),
				       ("symbol in state syms", fn(exp) => SymbolSet.member (stateSet, ExpProcess.exp2symbol exp)),
				       ("relative iterator is 1", fn(exp) => case ExpProcess.exp2temporaliterator exp of 
										 SOME (_, Iterator.RELATIVE 1) => true
									       | _ => false)]

			  val all_values = ExpBuild.explist (b_entry::coefficients)
				      
			  (*
			   val _ = Util.log("Expression: " ^ (e2s exp'))
			   val _ = Util.log(" -> Coeff: " ^ (e2s (ExpBuild.explist coefficients)))
			   val _ = Util.log(" -> Remainder: " ^ (e2s b_entry))
			   *)
			  val _ = case Match.findOnce (Match.anysym_with_predlist preds (Symbol.symbol "#pattern"), all_values) of
				      SOME e =>
				      (Logger.log_error (Printer.$("Cannot use backwards euler because the equation for state " ^ (state_str()) ^ " is nonlinear.  Eq: " ^ (e2ps eq)));
				       DynException.setErrored())
				    | NONE => ()

			  (* insert "rest" coefficient into bvector at stateindex*)
			  val _ = Array.update (bvector, rowIndex, neg_b_entry)

			  (* print for debugging *)
			  (*val _ = Matrix.print matrix*)
		      in
			  ((*foldl addEntry eq (SymbolSet.listItems deps);*)
			   ())
		      end
		      handle e => DynException.checkpoint "ShardedModel.updateShardForSolver.updateMatrix" e

		  val _ = app updateMatrix numberedRelationships

  		  (* create new shard using matrix equation Mx = b *)
		  (* optimize call will find the best known data structure for the matrix type za*)
		  val () = Matrix.optimize matrix
			   (*
		  val _ = print ("After optimization -> ")
		  val _ = Matrix.print matrix
			    *)
		  (*	     val (upperbw, lowerbw) = Matrix.findBandwidth (Matrix.DENSE matrix)
		   val _ = Util.log("Upper BW: "^(i2s upperbw)^", Lower BW: " ^ (i2s lowerbw))

		   val dims = List.length states
		   val m' = 
		       if (upperbw > Real.floor(Real.fromInt dims / 2.0)) orelse 
			  (lowerbw > Real.floor(Real.fromInt dims / 2.0)) then
			   Matrix.DENSE matrix
		       else
			   Matrix.dense2bandedmatrix (Matrix.DENSE matrix)*)

		  val m_eq = ExpBuild.equals (ExpBuild.var ("#M"),
					      Exp.CONTAINER(Exp.MATRIX matrix))
		  val b_eq = ExpBuild.equals (ExpBuild.var ("#b"),
					      Exp.CONTAINER(Exp.ARRAY bvector))
			     
		  (* sort initial conditions so order matches x element order *)
		  (* see findStateInitValues in CParallelWriter *)
		  val inits = List.filter ExpProcess.isInitialConditionEq (!(#exps flatclass))

		  fun getInitialCondition (state, _, _) =
		      case List.find (fn(init) => ExpProcess.getLHSSymbol(init) = state) inits of
			  SOME init => init
			| NONE => DynException.stdException ("State has no initial condition: " ^ (Symbol.name state), 
							     "ShardedModel.updateSHardForSolver.LINEAR_BACKWARDS_EULER", 
							     Logger.INTERNAL)

		  val sorted_inits = map getInitialCondition orderedRelationships

		  val newExps = sorted_inits @ [m_eq, b_eq]

		  val _ = (#exps flatclass) := newExps


		  (* Replaces the system iterator with a new one having a linear solver with the appropriate bandwidth attributes. *)
		  local
		      val linSolver = case !matrix of 
					  Matrix.DENSE _ => Solver.LSOLVER_DENSE
					| Matrix.BANDED {upperbw,lowerbw,...} => Solver.LSOLVER_BANDED {upperhalfbw=upperbw,
													lowerhalfbw=lowerbw}
		      val solver = Solver.LINEAR_BACKWARD_EULER {dt = dt, solv = linSolver}
		  in
		  val iter' = (itername, DOF.CONTINUOUS solver)

		  val shard' = {classes=[flatclass],instance=instance,iter_sym=itername}
		  end
	      in
		  (shard', iter')
	      end
	      handle e => DynException.checkpoint "ShardedModel.updateShardForSolver.[LINEAR_BACKWARD_EULER]" e)
	   | _ => (shard, iter))
    end
  | updateShardForSolver systemproperties (shard, iter) = (shard, iter)



fun forkModelByIterator model (iter as (iter_sym,_)) = 
    let
	fun namechangefun iter_sym = (fn(name)=> Symbol.symbol ((Symbol.name name) ^ "_" ^ (Symbol.name iter_sym)))
	val model' as (classes',_,_) = ModelProcess.duplicateModel model (namechangefun iter_sym)
	val _ = ModelProcess.pruneModel (SOME iter) model'
	val _ = map (ClassProcess.updateForkedClassScope iter) classes'
    in
	model'
    end

fun createIteratorForkedModels model =
    let
	val iterators = CurrentModel.iterators()
	fun forkedModel (iter as (iter_sym,_)) = 
	    let 
		val model' as (classes, instance as {name,classname},_) = forkModelByIterator model iter
	    in
		{classes=classes,
		 instance=instance,
		 iter_sym=iter_sym}
	    end
    in
	map forkedModel iterators
    end

fun forkModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, instance, sysprops) = model
	(* TODO, write the checks of the model IR as they are needed *)


	val forkedModels = createIteratorForkedModels model

	val {iterators,...} = sysprops
	val forkedModelsWithIterators = map
					    (fn(shard as {iter_sym,...})=>
					       case List.find (fn(iter_sym',_)=>iter_sym=iter_sym') iterators of
						   SOME iter => (shard, iter)
						 | NONE => DynException.stdException(("Can't find iterator with name '"^(Symbol.name iter_sym)^"' in system properties"),"ShardedModel.forkModel",Logger.INTERNAL)
					    )
					    forkedModels

	val prevModel = CurrentModel.getCurrentModel()
	val iter_count = List.length (CurrentModel.iterators())
	val _ = app
		    (fn({classes,instance,iter_sym},n)=>
		       let
			   val model' = (classes, instance, sysprops)
		       in
			   (CurrentModel.setCurrentModel(model');
			    log("\n==================   Iterator '"^(Symbol.name iter_sym)^"' ("^(i2s (n+1))^" of "^(i2s iter_count)^") =====================");
			    DOFPrinter.printModel model')
		       end)
		    (StdFun.addCount forkedModels)
	val _ = CurrentModel.setCurrentModel(prevModel)

	val shardsWithIterators = map 
				      (fn(s as {classes,instance,...}, iter)=> 
					 CurrentModel.withModel (classes,instance,sysprops) 
								(fn(_)=>updateShardForSolver sysprops (s, iter))) 
				      forkedModelsWithIterators

	val (shards,iterators') = ListPair.unzip shardsWithIterators
				  

	val sysprops' =
	    let 
		val {iterators, precision, target, num_models, debug, profile} = sysprops
	    in
		{iterators = iterators',
		 precision = precision,
		 target = target, 
		 num_models = num_models,
		 debug = debug,
		 profile = profile}
	    end

	val _ = app
		    (fn({classes,instance,iter_sym},n)=>	
		       let
			   val model' = (classes, instance, sysprops')
		       in
			   (CurrentModel.setCurrentModel(model');
			    log("\n==================   () Iterator '"^(Symbol.name iter_sym)^"' ("^(i2s (n+1))^" of "^(i2s iter_count)^") (Updated shards) =====================");
			    DOFPrinter.printModel model')
		       end)
		    (StdFun.addCount shards)

	(* we changed the system properties, so assign the new instance properties to the current model *)
	val _ = CurrentModel.setCurrentModel (classes, instance, sysprops') 


	val _ = DynException.checkToProceed()
    in
	(shards, sysprops')
    end
    handle e => DynException.checkpoint "ShardedModel.forkModel" e

(* take a sharded model and an iterator and return a model *)
fun toModel (shards, sysprops) iter_sym =
    let
	val {classes, instance, ...} : shard = case List.find (fn{iter_sym=iter_sym',...}=> iter_sym=iter_sym') shards of
						   SOME s => s
						 | NONE => DynException.stdException(("Can't find shard with iterator '"^(Symbol.name iter_sym)^"'"), "ShardedModel.toModel", Logger.INTERNAL)
    in
	(classes, instance, sysprops)
    end

(* take a sharded model and return the union of all the inputs across each model *)
fun toInputs (shards, sysprops) = 
    let
	fun shard2inputs (shard:shard) = 
	    let
		val {classes, instance, ...} = shard
		val model = (classes, instance, sysprops)
		fun model2inputs (_, {classname,...}, _) =
		    let
			val top_class = CurrentModel.classname2class classname
		    in
			!(#inputs top_class)
		    end
	    in
		CurrentModel.withModel 
		    model 
		    (fn()=> model2inputs model)
	    end
	    
	val input_lists = Util.flatmap shard2inputs shards
			  
	fun cmp_fun ({name=name1,...}, {name=name2,...}) =
	    Term.sym2curname name1 = Term.sym2curname name2
    in
	Util.uniquify_by_fun cmp_fun input_lists
    end

(* take a sharded model and return the union of all the outputs across each model *)
fun toOutputs (shards, sysprops) = 
    let
	fun shard2outputs (shard:shard) = 
	    let
		val {classes, instance, ...} = shard
		val model = (classes, instance, sysprops)
		fun model2outputs (_, {classname,...}, _) =
		    let
			val top_class = CurrentModel.classname2class classname
		    in
			!(#outputs top_class)
		    end
	    in
		CurrentModel.withModel 
		    model 
		    (fn()=> model2outputs model)
	    end
	    
	val output_lists = Util.flatmap shard2outputs shards
			  
	fun cmp_fun ({name=name1,...}, {name=name2,...}) =
	    Term.sym2curname name1 = Term.sym2curname name2
    in
	Util.uniquify_by_fun cmp_fun output_lists
    end



fun toIterator (shards, sysprops : DOF.systemproperties) iter_sym = 
    let
	val {iterators,...} = sysprops
    in
	case List.find (fn(iter_sym',_)=> iter_sym=iter_sym') iterators of
	    SOME i => i
	  | NONE => DynException.stdException(("Can't find iterator '"^(Symbol.name iter_sym)^"' in shard"),"toIterator",Logger.INTERNAL)
    end
    handle e => DynException.checkpoint "ShardedModel.toIterator" e

fun iterators (shards : shard list, _) =
    map #iter_sym shards

end
