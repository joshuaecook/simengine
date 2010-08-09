signature DOFOUTLINE =
sig

    type dependencies = {syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type dependency_table = dependencies SymbolTable.table

    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    type class_outline = {name: Symbol.symbol,
			  inputs: dependency_table,
			  init_exps: dependency_table,
			  intermediate_exps: dependency_table,
			  instance_exps: instance_dependency_table,
			  state_exps: dependency_table,
			  outputs: dependency_table}

    type dof_outline = class_outline SymbolTable.table

    val class_to_outline : DOF.class -> class_outline

end
structure DOFOutline : DOFOUTLINE =
struct

    type dependencies = {syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type dependency_table = dependencies SymbolTable.table

    type instance_dependencies = {instclass: Symbol.symbol, syms: SymbolSet.set, iter_syms: SymbolSet.set}
    type instance_dependency_table = instance_dependencies SymbolTable.table

    type class_outline = {name: Symbol.symbol,
			  inputs: dependency_table,
			  init_exps: dependency_table,
			  intermediate_exps: dependency_table,
			  state_exps: dependency_table,
			  instance_exps: instance_dependency_table,
			  outputs: dependency_table}

    type dof_outline = class_outline SymbolTable.table

    fun exp_to_symbols_and_iterators exp =
	let
	    val terms = ExpProcess.exp2termsymbols exp
	    val syms = SymbolSet.fromList (map Term.sym2curname terms)
	    val embedded_iterators = SymbolSet.fromList
					 (map (fn(iter_sym, _)=>iter_sym)
					      (List.mapPartial TermProcess.symbol2temporaliterator terms))
	    val literal_iterators = SymbolSet.fromList 
					(map Term.sym2curname 
					     (List.filter Term.isIterator terms))
	in
	    (syms, SymbolSet.union (embedded_iterators, literal_iterators))
	end

    fun exps_to_symbols_and_iterators nil =
	{syms=SymbolSet.empty, iter_syms=SymbolSet.empty}
      | exps_to_symbols_and_iterators (exp::rest) =
	let
	    val (syms, iter_syms) = exp_to_symbols_and_iterators exp
	    val {syms=syms', iter_syms=iter_syms'} = exps_to_symbols_and_iterators rest
	in
	    {syms=SymbolSet.union (syms, syms'), iter_syms=SymbolSet.union (iter_syms, iter_syms')}
	end

    (* class_to_outline - construct a simpler representation of the common elements that need to be
     * traversed, such as finding all the symbols or iterators that are arguments of an intermediate *)
    fun class_to_outline (class:DOF.class) =
	let
	    (* few helper calls *)
	    val empty = SymbolTable.empty
	    val emptyset = SymbolSet.empty
	    val singleton = SymbolSet.singleton

	    (* first, pull out the relevent pieces *)
	    val name = #name class
	    val inputs = !(#inputs class)
	    val exps = !(#exps class)
	    val (init_exps, rest) = List.partition ExpProcess.isInitialConditionEq exps
	    val (instance_exps, rest) = List.partition ExpProcess.isInstanceEq rest
	    val (state_exps, intermediate_exps) = List.partition ExpProcess.isStateEq rest
	    val outputs = !(#outputs class)

	    (* first, the inputs - here just grab the iterator present *)
	    val inputs_outline = 
		foldl 
		    (fn(inp, table)=> 
		       let
			   val name = DOF.Input.name inp
			   val sym = Term.sym2curname name
			   val entry = {syms=emptyset,
					iter_syms=case TermProcess.symbol2temporaliterator name of
						      SOME (iter_sym, _) => singleton iter_sym
						    | NONE => emptyset}
		       in
			   SymbolTable.enter (table, sym, entry)
 		       end)
		    empty
		    inputs

	    (* next, the outputs *)
	    val outputs_outline =
		foldl
		    (fn(out, table)=>
		       let
			   val name = DOF.Output.name out
			   val sym = Term.sym2curname name
			   val contents = DOF.Output.contents out
			   val condition = DOF.Output.condition out
			   val entry as {syms, iter_syms} = exps_to_symbols_and_iterators (condition::contents)
			   val entry' = case TermProcess.symbol2temporaliterator name of
					    SOME (iter_sym, _) => {syms=syms, iter_syms=SymbolSet.add(iter_syms, iter_sym)}
					  | NONE => entry
		       in
			   SymbolTable.enter (table, sym, entry')
		       end)
		    empty
		    outputs

	    (* next, the expressions *)
	    fun exps_to_outline exps =
		foldl
		    (fn(exp, table)=>
		    let
			val (lhs_syms, lhs_iter_syms) = exp_to_symbols_and_iterators (ExpProcess.lhs exp)
			val (rhs_syms, rhs_iter_syms) = exp_to_symbols_and_iterators (ExpProcess.rhs exp)
			val sym = ExpProcess.getLHSSymbol exp
			val entry = {syms=rhs_syms, iter_syms=SymbolSet.union (lhs_iter_syms, rhs_iter_syms)}
		    in
			SymbolTable.enter (table, sym, entry)
		    end)
		    empty
		    exps

	    val init_exps_outline = exps_to_outline init_exps
	    val state_exps_outline = exps_to_outline state_exps
	    val intermediate_exps_outline = exps_to_outline intermediate_exps

	    (* finally, the instances are the hardest ... we are going to need a foldl inside a foldl since there
	     * could be multiple outputs for a given instance *)
	    val instance_outline = 
		foldl
		    (fn(inst, table)=>
		       let
			   val {classname, outargs, ...} = ExpProcess.deconstructInst inst
			   val (syms, iter_syms) = exp_to_symbols_and_iterators (ExpProcess.rhs inst)
								    
			   val table' = foldl
					    (fn(term, table')=>
					       let
						   val sym = Term.sym2curname term
						   val iter_syms' = case TermProcess.symbol2temporaliterator term of
									SOME (iter_sym, _) => SymbolSet.add (iter_syms, iter_sym)
								      | NONE => iter_syms
						   val entry' = {instclass=classname, syms=syms, iter_syms=iter_syms'}
					       in
						   SymbolTable.enter (table', sym, entry')
					       end)
					    table
					    outargs
		       in
			   table'
		       end)
		    empty
		    instance_exps

	in
	    {name=name,
	     inputs=inputs_outline,
	     init_exps=init_exps_outline,
	     intermediate_exps=intermediate_exps_outline,
	     state_exps=state_exps_outline,
	     instance_exps=instance_outline,
	     outputs=outputs_outline}
	end

end
