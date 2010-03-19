signature SHARDEDMODEL =
sig

    type shard = {classes: DOF.class list,
		  instance: DOF.instance,
		  iter_sym: Symbol.symbol}
    type shardedModel = (shard list * DOF.systemproperties)

    val empty_shardedModel : shardedModel
			
    val forkModel : DOF.model -> shardedModel
				 
    val updateShardForSolver : DOF.systemproperties -> (shard * DOF.systemiterator) -> (shard * DOF.systemiterator)
    val combineDiscreteShards : shardedModel -> shardedModel

    (* shardedModel utilities *)
    val toModel : shardedModel -> Symbol.symbol -> DOF.model
    val toInputs : shardedModel -> DOF.input list
    val toOutputs : shardedModel -> DOF.output list
    val toIterator : shardedModel -> Symbol.symbol -> DOF.systemiterator
    (* Returns the shard associated with a given named iterator. *)
    val findShard: shardedModel * Symbol.symbol -> shard option

    (* pull out a list of iterator symbols *)
    val iterators : shardedModel -> Symbol.symbol list

    (* compute the state size *)
    val statesize : shardedModel -> int

    (* order the classes in a particular shared *)
    val orderShard : (DOF.model * shard) -> shard

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
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr

(* define an empty sharded model useful for return when a current sharded model is invalid *)
val empty_shardedModel = 
    let
	val (_, _, emptySysProps) = CurrentModel.empty_model
    in
	([], emptySysProps)
    end

(* Ensures that classes within a shard model are in dependency order. *)
fun orderShard (model, shard as {classes, instance, iter_sym} : shard) =
    let 
	val topClassName = 
	    case #name instance 
	     of SOME x => x | NONE => #classname instance		

	fun sort (c1, c2) =
	    (* The top class should always appear at the end of the list. *)
	    if topClassName = ClassProcess.class2classname c1 then GREATER
	    else if topClassName = ClassProcess.class2classname c2 then LESS
	    else sort' (c1, c2)

	and sort' (c1, c2) =
	    (* Other classes appear after their instances. *)
	    let val (instanceClassNames, _) = 
		    CurrentModel.withModel model (fn _ => ListPair.unzip (ClassProcess.class2instnames' c1))
	    in 
		if List.exists (fn cn => cn = ClassProcess.class2classname c2) instanceClassNames
		then GREATER else LESS
	    end
    in
	{classes = Sorting.sorted sort classes, 
	 instance = instance, 
	 iter_sym = iter_sym}
    end


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
			       (* use a test to make sure that we're only looking at the top most expression *)
			       test=SOME (fn(matched_exp, matchedPatterns)=>ExpEquality.equiv (exp', matched_exp)),
			       replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.times [ExpBuild.pvar "d2", ExpBuild.pvar "d3"],
									      ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}

			  (* it could be that there is no coefficient in front of the variable *)
			  val coeff_rewrite_degenerate_case =
			      {find=ExpBuild.plus[Match.any "d1", 
						  var, 
						  Match.any "d4"],
			       (* use a test to make sure that we're only looking at the top most expression *)
			       test=SOME (fn(matched_exp, matchedPatterns)=>ExpEquality.equiv (exp', matched_exp)),
			       replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.int 1,
									      ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}

			  (* run a rewrite to pull out the coeff and remainder *)
			  val (coeff, remainder) = 
			      case Match.applyRewritesExp [coeff_rewrite, coeff_rewrite_degenerate_case] exp' of
				  Exp.CONTAINER(Exp.EXPLIST [coeff, remainder]) =>
				  (coeff, remainder)
				| _ =>
				  (Logger.log_error (Printer.$("Cannot factor out state "^(state_str())^" from expression '"^(e2ps exp')^"'.  The system may be nonlinear."));
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
					  (* it could be that there is no coefficient in front of the variable *)
					  val coeff_rewrite_degenerate_case =
					      {find=ExpBuild.plus[Match.any "d1", 
								  var, 
								  Match.any "d4"],
					       (* use a test to make sure that we're only looking at the top most expression *)
					       test=SOME (fn(matched_exp, matchedPatterns)=>ExpEquality.equiv (exp', matched_exp)),
					       replace=Rewrite.RULE(Exp.CONTAINER(Exp.EXPLIST[ExpBuild.int 1,
											      ExpBuild.plus  [ExpBuild.pvar "d1", ExpBuild.pvar "d4"]]))}
					      
				      in
					  case Match.applyRewritesExp [coeff_rewrite, coeff_rewrite_degenerate_case] exp of
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
	   | Solver.FORWARD_EULER {dt=dtval} => 
	     if DynamoOptions.isFlagSet "aggregate" then
		 let
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
		     fun rewriteEquationForForwardEuler (state, eq) =
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

			     (* make code easier to read *)
			     val y = var
			     val dt = ExpBuild.real dtval
			     fun add(a,b) = ExpBuild.plus [a, b]
			     fun mul(a,b) = ExpBuild.times [a, b]

			     (* forward euler -> y' = f(y) -> y[n+1] = y[n] + dt*f(y[n]) *)
			     val rhs' = add(y, mul(dt, rhs))

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
					    rewriteEquationForForwardEuler (ExpProcess.getLHSSymbol eq, eq)
					else
					    eq)
				     exps

		     (* update exps in class *)
		     val _ = #exps flatclass := exps'

		     (* change the form of the iterator *)
		     val iter' = (itername, DOF.DISCRETE {sample_period=dtval})

		 in
		     (shard, iter')
		 end
	     else
		 (shard, iter)
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
		val {iterators, precision, target, parallel_models, debug, profile} = sysprops
	    in
		{iterators = iterators',
		 precision = precision,
		 target = target, 
		 parallel_models = parallel_models,
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

fun findShard ((shards, _): shardedModel, iter_sym) =
    List.find (fn s => iter_sym = #iter_sym s) shards

fun iterators (shards : shard list, _) =
    map #iter_sym shards


fun statesize shardedModel =
    let
	fun shard2statesize iter_sym = 
	    let
		val model as (classes, {name, classname}, sysprops) = toModel shardedModel iter_sym
		val statesize = CurrentModel.withModel model (fn()=>ModelProcess.model2statesize model)
		(*val _ = Util.log ("Shard '"^(Symbol.name iter_sym)^"' has "^(i2s statesize)^" states")*)
	    in
		statesize
	    end
    in
	Util.sum (map shard2statesize (iterators shardedModel))
    end

(* combine discrete iterators - here we take iterators that have the same time step and are either discrete iterators, 
continuous iterators (of type FORWARD_EULER or EXPONENTIAL_EULER), algebraic iterators, and update iterators and aggregate
them in one iterator. When the aggregate option is enabled, the continuous iterators will already be converted into discrete
iterators in the updateShardforSolver function. The main function called is combineDiscreteIterators.  *)
local
    fun sameSymbolinTerms (Exp.SYMBOL (sym1,_), Exp.SYMBOL (sym2, _)) = sym1=sym2
      | sameSymbolinTerms _ = false

    (* where do you want the position of expressions to be - for preprocess, it's before, everything else is after *)			      
    datatype pos = BEFORE | AFTER

    (* this is a helper function that will combine the contents of shard2 into shard1 *)
    fun combineTwoShards before_iters ((shard1 as {classes=classes1, instance=instance1, iter_sym=iter_sym1}: shard),
				       (shard2 as {classes=classes2, instance=instance2, iter_sym=iter_sym2}: shard)) position = 
	let
	    (* there should be a one-to-one mapping of classes within each shard *)
	    val all_class_names = Util.intersection (map ClassProcess.class2preshardname classes1, 
						     map ClassProcess.class2preshardname classes2)
	    (* create a one-to-one mapping of class names *)
	    fun empty_class (name, props) = {name=name, properties=props, inputs=ref [], outputs=ref [], iterators=[], exps=ref []}
	    val mapped_classes = map (fn(name)=> case (List.find (fn(c)=>ClassProcess.class2preshardname c = name) classes1, 
						       List.find (fn(c)=>ClassProcess.class2preshardname c = name) classes2) of 
						     (SOME c1, SOME c2) => (c1, c2)
						   | (SOME (c1 as {name, properties, ...}), NONE) => (c1, empty_class (name, properties))
						   | (NONE, SOME (c2 as {name, properties, ...})) => (empty_class (name, properties), c2)
						   | (NONE, NONE) => DynException.stdException(("Unexpected class name: "^(Symbol.name name)),
											       "ShardedModel.combineTwoShards", Logger.INTERNAL))
				     all_class_names

	    (* merge each of the classes independently per shard *)
	    val classes' = 
		map
		    (fn(c1 as {name as name1, properties, inputs=inputs1, outputs=outputs1, iterators=iters1, exps=exps1},
			c2 as {name=name2, inputs=inputs2, outputs=outputs2, iterators=iters2, exps=exps2, ...})=>
		       let
			   val _ = if ClassProcess.class2preshardname c1 = ClassProcess.class2preshardname c2 then
				       ()
				   else
				       DynException.stdException(("Unexpected differing class names: "^(Symbol.name name1)^" != " ^ (Symbol.name name2)),
								 "ShardedModel.combineTwoShards", Logger.INTERNAL)

				       

			   (* create a change iterator function *)
			   fun updateIterator exp = 
			       let
				   val rewrite_symbol = {find=Match.onesym "#a",
							 test=NONE,
							 replace=Rewrite.ACTION (Symbol.symbol "RewriteIterator", 
										 (fn(exp)=>if ExpProcess.hasTemporalIterator exp then
											       ExpProcess.renameTemporalIteratorForAggregating (before_iters, iter_sym1) exp
											   else
											       exp))}
				   val rewrite_iterators = map (fn(sym')=> {find=Match.asym sym',
									    test=NONE,
									    replace=Rewrite.RULE (ExpBuild.var (Symbol.name iter_sym1))}) before_iters
			       in
				   Match.applyRewritesExp (rewrite_iterators @ [rewrite_symbol]) exp
			       end

			   (* combine inputs *)
			   val unmatched_inputs = List.filter (fn{name,...}=> not (List.exists (fn{name=name',...}=> sameSymbolinTerms (name,name')) (!inputs1))) (!inputs2)
			   fun updateInput {name, default} = 
			       {name=(ExpProcess.exp2term o updateIterator o ExpProcess.term2exp) name,
				default= case default of 
					     SOME d => SOME (updateIterator d)
					   | NONE => NONE}
			   val inputs' = map updateInput ((!inputs1) @ unmatched_inputs)

			   (* combine outputs *)
			   val unmatched_outputs = List.filter (fn{name,...}=> not (List.exists (fn{name=name',...}=> sameSymbolinTerms(name,name')) (!outputs1))) (!outputs2)
			   fun updateOutput {name,contents,condition} =
			       {name=(ExpProcess.exp2term o updateIterator o ExpProcess.term2exp) name,
				contents=map updateIterator contents,
				condition=updateIterator condition}
			   val outputs' = map updateOutput ((!outputs1) @ unmatched_outputs)

			   (* combine iterators *)
			   val unmatched_iterators = List.filter (fn{name,...}=> not (List.exists (fn{name=name',...}=> name=name') iters1)) iters2
			   val iters' = iters1 @ unmatched_iterators

			   (* combine exps *)
			   val (init_eqs1, rest1) = List.partition ExpProcess.isInitialConditionEq (!exps1)
			   val (init_eqs2, rest2) = List.partition ExpProcess.isInitialConditionEq (!exps2)
			   val exps' = map 
					   updateIterator 
					   (case position of
						BEFORE => init_eqs2 @ init_eqs1 @ rest2 @ rest1 
					      | AFTER => init_eqs1 @ init_eqs2 @ rest1 @ rest2)
			       
			   (* update references *)
			   val _ = inputs1 := inputs'
			   val _ = outputs1 := outputs'
			   val _ = exps1 := exps'
		       in
			   {name=name,
			    properties=properties,
			    inputs=inputs1,
			    outputs=outputs1,
			    iterators=iters',
			    exps=exps1}
		       end)
		    mapped_classes

	    val instance' = instance1
	    val iter_sym' = iter_sym1
	in
	    (* return the new combined shard *)
	    {classes=classes', 
	     instance=instance',
	     iter_sym=iter_sym'}
	end

    fun iter_sym2next_iter_sym sym =
	Symbol.symbol ("#next__" ^ (Symbol.name sym))

    (* make all iterators that are include in iters to be set to be RELATIVE 1 if currently set to RELATIVE 0 *)
    fun applyIteratorRewrites (pos, iters, new_iter_sym) (shard : shard as {classes,...}) =
	let
	    val id = Symbol.symbol "ApplyPostIteratorRewrites"

	    fun update_exp_to_post (Exp.TERM (Exp.SYMBOL (sym, props as {iterator=SOME ((iter_sym,Iterator.RELATIVE 0)::rest),...}))) =
		let
		    val props' = Property.setIterator props ((iter_sym,Iterator.RELATIVE 1)::rest)
		in
		    Exp.TERM (Exp.SYMBOL (sym, props'))
		end
	      | update_exp_to_post exp = exp

	    fun update_exp_to_pre (Exp.TERM (Exp.SYMBOL (sym, props as {iterator=SOME ((iter_sym,Iterator.RELATIVE 1)::rest),...}))) =
		let
		    val props' = Property.setIterator props ((iter_sym,Iterator.RELATIVE 0)::rest)
		in
		    Exp.TERM (Exp.SYMBOL (sym, props'))
		end
	      | update_exp_to_pre exp = exp

	    fun iter_sym2iter iter_sym = 
		case (List.find (fn(iter_sym', _)=>iter_sym=iter_sym') iters) of
		    SOME iter => iter
		  | NONE => DynException.stdException(("Iterator '"^(Symbol.name iter_sym)^"' does not exist"), "ShardedModel.applyIteratorRewrites.iter_sym2iter", Logger.INTERNAL)
	    fun isPreProcess iter_sym =
		case iter_sym2iter iter_sym of
		    (_, DOF.ALGEBRAIC (DOF.PREPROCESS, _)) => true
		  | _ => false

	    fun isPostProcess iter_sym =
		case iter_sym2iter iter_sym of
		    (_, DOF.ALGEBRAIC (DOF.POSTPROCESS, _)) => true
		  | (_, DOF.UPDATE _) => true
		  | _ => false

	    val rewrite = {find=Match.onesym "#a",
			   test=SOME (fn(exp,_)=> case (pos, ExpProcess.exp2temporaliterator exp) of
						      (AFTER, SOME (iter_sym, Iterator.RELATIVE 0)) => List.exists (fn(sym, _)=>iter_sym=sym) iters andalso
												       not (isPreProcess iter_sym)
						    | (BEFORE, SOME (iter_sym, Iterator.RELATIVE 1)) => List.exists (fn(sym, _)=>iter_sym=sym) iters andalso
													not (isPostProcess iter_sym)
						    | _ => false),
			   replace=Rewrite.ACTION (id, case pos of 
							   AFTER => update_exp_to_post
							 | BEFORE => update_exp_to_pre)}
	    val next_sym = iter_sym2next_iter_sym new_iter_sym

	    val iterator_rewrite = case pos of 
				       AFTER => [{find=Match.onesym "#i",
						  test=SOME (fn(exp,_) => case exp of
									      Exp.TERM (Exp.SYMBOL (sym, props)) => List.exists (fn(sym', _)=> sym=sym') iters
									    | _ => false),
						  replace=Rewrite.ACTION (id, fn(exp) => case exp of 
											     Exp.TERM (Exp.SYMBOL (sym, props)) => Exp.TERM (Exp.SYMBOL (next_sym, props))
											   | _ => exp)}]
				     | BEFORE => []
	in
	    app (ClassProcess.applyRewritesToClass (rewrite::iterator_rewrite)) classes
	end

in
fun combineDiscreteShards (shardedModel as (_, sysprops)) =
    let
	(* create a table of iterators *)
	fun isIndependent (_, DOF.CONTINUOUS _) = true
	  | isIndependent (_, DOF.DISCRETE _) = true
	  | isIndependent _ = false
	fun isDependentOn sym (_, DOF.ALGEBRAIC (_, sym')) = sym=sym'
	  | isDependentOn sym (_, DOF.UPDATE sym') = sym=sym'
	  | isDependentOn sym (_, DOF.IMMEDIATE) = true
	  | isDependentOn sym _ = false
	val iter_list : (DOF.systemiterator list) = map (toIterator shardedModel) (iterators shardedModel)
	val independent_iterators : (DOF.systemiterator list)  = List.filter isIndependent iter_list
	val iterator_dependencies : (Symbol.symbol * DOF.systemiterator list) list = 
	    map 
		(fn(iter_sym, _)=> (iter_sym, List.filter (isDependentOn iter_sym) iter_list))
		independent_iterators

	(* first combine all iterators with the same time step *)
	fun iterator_to_timestep (x, DOF.DISCRETE {sample_period}) = SOME (x, sample_period)
	  | iterator_to_timestep _ = NONE

	(* combine_dependent_shards merges the algebraic iterators and update iterator into the original discrete shard *)
	fun combine_dependent_shards all_iters (base_shard, []) = base_shard
	  | combine_dependent_shards all_iters (base_shard, (iter_sym, DOF.ALGEBRAIC (DOF.PREPROCESS, _))::rest) = combine_dependent_shards all_iters (combineTwoShards all_iters (base_shard, valOf (findShard (shardedModel, iter_sym))) BEFORE, rest)
	  | combine_dependent_shards all_iters (base_shard, (iter_sym, _)::rest) = combine_dependent_shards all_iters (combineTwoShards all_iters (base_shard, valOf (findShard (shardedModel, iter_sym))) AFTER, rest)

	(* combine_independent_shards merges each of the discrete iterators into one shard, combining all the dependent shards found in the process into one list *)
	fun combine_independent_shards all_iters nil = DynException.stdException("Can't combine nil shards", "ShardedModel.combineDiscreteIterators.combine_independent_shards", Logger.INTERNAL)
	  | combine_independent_shards all_iters ((shard, dependent_iterators)::nil) = (shard, dependent_iterators)
	  | combine_independent_shards all_iters ((shard1, dependent_iterators1)::(shard2, dependent_iterators2)::rest) = 
	    combine_independent_shards all_iters ((combineTwoShards all_iters (shard1, shard2) AFTER, 
						   dependent_iterators1 @ dependent_iterators2)::rest)

	val iters_with_dt : ((Symbol.symbol * DOF.systemiterator list) * real) list = (List.mapPartial (fn(mapping as (iter_sym, _))=> iterator_to_timestep (mapping, #2 (toIterator shardedModel iter_sym))) iterator_dependencies)
	(* group all shards by unique timesteps - only merge if the time step is the same *)
	val unique_dt_list : real list = Util.uniquify_by_fun Real.== (map (fn(_, dt)=> dt) iters_with_dt)

	(* merge all the shards here - create a list of all iterator symbols that have been merged *)
	val (reducedShards, all_iterator_symbols) = 
	    ListPair.unzip 
		(map
		     (fn(dt)=>  
			let
			    val matching_iterators = map #1 (List.filter (fn(_, dt') => Real.== (dt,dt')) iters_with_dt)
			    val all_iterator_symbols = Util.uniquify (Util.flatmap (fn(iter_sym, iter_dependency_list)=> iter_sym::(map (fn(iter_sym, _)=> iter_sym) iter_dependency_list)) matching_iterators)
			    (* we have the symbol names, go back to a list of DOF.systemiterator's *)
			    val iter_list' = map (fn(iter_sym)=> valOf (List.find (fn(iter_sym',_)=>iter_sym=iter_sym') iter_list)) all_iterator_symbols

			    val new_iter_sym = case matching_iterators of
						   (iter_sym,_)::_ => iter_sym
						 | _ => DynException.stdException("No iterators of a given sample period", "ShardedModel.combineDiscreteIterators", Logger.INTERNAL)

			    (* update the iterators to from n->n+1 or n+1->n depending on if needed *)
			    val _ = 
				app 
				    (fn(iter_sym, sys_iters)=> 
				       app 
					   (fn(iter_sym', iter_type) => 
					      case iter_type of 
						  DOF.UPDATE _ => 
						  applyIteratorRewrites (AFTER, iter_list', new_iter_sym) (valOf (findShard (shardedModel, iter_sym')))
						| DOF.ALGEBRAIC (DOF.POSTPROCESS, _) => 
						  applyIteratorRewrites (AFTER, iter_list', new_iter_sym) (valOf (findShard (shardedModel, iter_sym')))
						| DOF.ALGEBRAIC (DOF.PREPROCESS, _) => 
						  applyIteratorRewrites (BEFORE, iter_list', new_iter_sym) (valOf (findShard (shardedModel, iter_sym')))
						| _ => ())
					   sys_iters)
				    matching_iterators

			    (*val _ = Util.log ("Updating matching iterator symbols: " ^ (Util.symlist2s all_iterator_symbols))*)
			    val combined_independent = combine_independent_shards all_iterator_symbols (map (fn(iter_sym, dependent_iterators)=> (valOf (findShard (shardedModel, iter_sym)), dependent_iterators)) matching_iterators)
			    val combined_dependent = combine_dependent_shards all_iterator_symbols combined_independent

			    (* create a next time equation used for post process and update states *)
			    val next_time_exp = ExpBuild.equals (ExpBuild.var (Symbol.name (iter_sym2next_iter_sym new_iter_sym)),
								 ExpBuild.plus [ExpBuild.var (Symbol.name new_iter_sym), ExpBuild.real dt])
						
			    (* update shard to include next time expression *)
			    fun updateClass (c as {exps,...}) = 
				exps := (next_time_exp::(!exps))
			    val _ = app updateClass (#classes combined_dependent)
				
			in
			    (combined_dependent, all_iterator_symbols)
			end)
		     unique_dt_list)
	val all_iterator_symbols = Util.uniquify (List.concat all_iterator_symbols)

	(* now create a new shard list based on the ones that have been reduced down and the remaining ones that have been untouched *)
	val shard_list = reducedShards @ 
			 (map (fn(iter_sym, _)=> valOf (findShard (shardedModel, iter_sym)))
			      (List.filter (fn(iter_sym,_)=> not (List.exists (fn(iter_sym')=> iter_sym=iter_sym') all_iterator_symbols)) iter_list))
				   

	(* update the system properties with the new iterator information *)
	val shard_list_iterators = map #iter_sym shard_list
	val {iterators, precision, target, parallel_models, debug, profile} = sysprops
	val iterators' = List.filter (fn(iter_sym, _)=> List.exists (fn(iter_sym')=>iter_sym=iter_sym') shard_list_iterators) iterators
	val sysprops' = {iterators=iterators',
			 precision=precision,
			 target=target,
			 parallel_models=parallel_models,
			 debug=debug,
			 profile=profile}


	(* for debugging *)
	val iter_count = List.length shard_list
	val _ = app
		    (fn({classes,instance,iter_sym},n)=>	
		       let
			   val model' = (classes, instance, sysprops')
		       in
			   (CurrentModel.setCurrentModel(model');
			    log("\n==================   () Iterator '"^(Symbol.name iter_sym)^"' ("^(i2s (n+1))^" of "^(i2s iter_count)^") (After aggregating shards) =====================");
			    DOFPrinter.printModel model')
		       end)
		    (StdFun.addCount shard_list)


    in
	(shard_list, sysprops')
    end
end

end
