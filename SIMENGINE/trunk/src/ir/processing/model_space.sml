signature MODELSPACE =
sig

    (* propagate spaces through a model *)
    val propagateSpacesThroughModel : DOF.model -> unit

end
structure ModelSpace : MODELSPACE =
struct


local

    fun error msg = (Logger.log_error (Printer.$ msg);
		     DynException.setErrored())
    fun except msg = DynException.stdException (msg, "ModelSpace.propagateSpacesThroughModel",
						Logger.INTERNAL)

    val e2s = ExpPrinter.exp2str
    fun expToSpace exp = ExpSpace.expToSpace exp
	handle ExpSpace.SpaceException {exp=exp', spaces} => 
	       (Logger.log_error (Printer.$("Can not evaluate the size of '"
					    ^(e2s exp')
					    ^"' with spatial dimensions "
					    ^(Util.list2str Space.toString spaces)));
		DynException.setErrored();
		Space.emptyCollection)
	     | e => DynException.checkpoint "ModelSpace.propagateSpaceThroughModel.expToSpace" e

    (* function that given an exp and a space table, will use the term rewriter to 
     * replace all known symbols in the expression *)
    fun update_symbols_in_exp space_table exp =
	let
	    val symbol_space_pair = SymbolTable.listItemsi space_table

	    fun symbol_space_pair_to_rewrite (sym, space) =
		let
		    val find = Match.asym sym
		    val action = (fn(exp)=>
				    case exp of 
					Exp.TERM (Exp.SYMBOL (sym, props)) =>
					Exp.TERM (Exp.SYMBOL (sym, Property.setSpace props space))
				      | _ => exp)
		in
		    {find=find,
		     test=NONE,
		     replace=Rewrite.ACTION (Symbol.symbol ("PropagateSpatial:"^(Symbol.name sym)), action)}
		end
	    val rewrites = map symbol_space_pair_to_rewrite symbol_space_pair
	in
	    Match.applyRewritesExp rewrites exp
	end
	handle e => DynException.checkpoint "ModelSpace.update_symbols_in_exp" e

    fun update_instances_in_exp class_to_space_table exp =
	let
	    val classname_table_pair = SymbolTable.listItemsi class_to_space_table
	    val classname_output_space_triple = 
		Util.flatmap 
		    (fn(classname, (_,output_space_table))=> 
		       map 
			   (fn(output, space)=>(classname, output, space))
			   (SymbolTable.listItemsi output_space_table))
		    classname_table_pair
		    
	    fun classname_output_space_triple_to_rewrite (sym, out, space) =
		let
		    val find = Match.an_instance_by_classname_and_output (sym, out)
		    val action = fn(exp)=>
				   case exp of
				       Exp.FUN (Fun.OUTPUT {classname, instname, outname, props}, args) => 
				       Exp.FUN (Fun.OUTPUT {classname=classname, 
							    instname=instname, 
							    outname=outname,
							    props=InstProps.setSpace props space}, args)
				     | _ => exp
		in
		    {find=find,
		     test=NONE,
		     replace=Rewrite.ACTION (Symbol.symbol ("PropagateInstance:"^(Symbol.name sym)), action)}
		end
	    val rewrites = map classname_output_space_triple_to_rewrite classname_output_space_triple
	in
	    (*Match.applyRewritesExp rewrites*) exp
	end
	handle e => DynException.checkpoint "ModelSpace.update_instances_in_exp" e

    (* take the right hand side space value and assign it to the left hand side quantity *)
    fun propagate_spaces_right_to_left exp = 
	let
	    val rhs = ExpProcess.rhs exp
	    val rhs_space = expToSpace rhs
	    val lhs_term = ExpProcess.exp2term (ExpProcess.lhs exp)

	    fun updateTermWithSpace (t, space) =
		case t of
		    Exp.SYMBOL (sym, props) => 
		    Exp.SYMBOL (sym, Property.setSpace props space)
		  | _ => except "Received non term in updateTermWithSpace"
		
	    val lhs_with_space = 
		ExpProcess.term2exp 
		(case lhs_term of
		     Exp.SYMBOL _ => updateTermWithSpace (lhs_term, rhs_space)
		   | Exp.TUPLE nil => lhs_term
		   | Exp.TUPLE [term] => Exp.TUPLE [updateTermWithSpace (term, rhs_space)]
		   | Exp.TUPLE terms => 
		     if Space.isCollection rhs_space then
			 let val spaces = Space.separate rhs_space
			 in if (List.length spaces) = (List.length terms) then
				Exp.TUPLE (map updateTermWithSpace (ListPair.zip (terms, spaces)))
			    else
				except "Non equal number of spaces/terms in tuple"
			 end
		     else
			 except "Non collection on rhs of probably instance equation"
		   | _ => lhs_term) (* not sure what to do here *)

	in
	    ExpBuild.equals (lhs_with_space, rhs)
	end
	handle e => DynException.checkpoint "ModelSpace.propagate_spaces_right_to_left" e

    fun add_terms_to_table table terms =
	let fun add_symbol_to_table (Exp.SYMBOL (sym, props), table) =
		SymbolTable.enter (table, sym, Property.getSpace props)
	      | add_symbol_to_table (_, table) = table
	in foldl
	    (fn(t, table')=> case t of 
				 Exp.TUPLE terms => foldl add_symbol_to_table table' terms
			       | _ => add_symbol_to_table (t, table'))
	    table
	    terms
	end
	handle e => DynException.checkpoint "ModelSpaces.add_terms_to_table" e

    fun log_space_table table =
	let
	    open Layout
		 
	    fun entryToLayout (sym, space) =
		label (Symbol.name sym, Space.toLayout space)
	    val l = 
		heading ("Symbol Space Table:",
			 align (map entryToLayout (SymbolTable.listItemsi table)))
	in
	    log l
	end

    fun log_instance_table table =
	let
	    open Layout

	    fun entryToLayout (sym, space) =
		label (Symbol.name sym, Space.toLayout space)
	    fun instanceTableToLayout (sym, (symbol_table, instance_table)) = 
		heading (Symbol.name sym, 
			 align [heading ("Outputs",
					 align (map entryToLayout (SymbolTable.listItemsi instance_table))),
				heading ("Symbols",
					 align (map entryToLayout (SymbolTable.listItemsi symbol_table)))])
	    val l = 
		heading ("Instance Space Table",
			 align (map instanceTableToLayout (SymbolTable.listItemsi table)))
	in
	    log l
	end

    (* we need to supply the correct spaces attribute on the name *)
    fun propagateSpacesThroughOutputs output =
	let
	    val (name, contents, condition) = (DOF.Output.name output,
					       DOF.Output.contents output,
					       DOF.Output.condition output)

(*
	    local
		open Layout
		val e2l = ExpPrinter.exp2layout
		val s2l = Space.toLayout
		fun exp2layout exp =
		    parenList [s2l (expToSpace exp), 
			       e2l exp]

		val l = heading("Output " ^ (Symbol.name (Term.sym2curname name)),
				align [heading("Name", exp2layout (ExpProcess.term2exp name)),
				       heading("Contents", align (map exp2layout contents)),
				       heading("Condition", exp2layout condition)])
	    in
	    val _ = log l
	    end
*)

	    (* when we propagate the spaces from the contents/condition, there are two valid and supported 
	     * options.  Either the space definition for the name can come from the contents or from the
	     * condition.  It can not come from both. *)
	    val space = case (map expToSpace contents, expToSpace condition) of
			    (* case #1: we have only one output in the contents, and the condition space must be compatible with 
			     * the output space*)
			    ([output_space], condition_space) => if Space.isScalar output_space then
								     condition_space
								 else if Space.isScalar condition_space then
								     output_space
								 else if Space.equal (output_space, condition_space) then
								     output_space
								 else
								     (error ("Can't have incompatible multi-demensional outputs and conditions");
								      Space.scalar)
			  (* case #2: we have multiple outputs in contents so we support only a scalar condition. This
			   * also works in the empty case *)
			  | (output_spaces, condition_space) => if Space.isScalar condition_space then
								    Space.collection output_spaces
								else
								    (error ("Can't define a multi-demensional condition with a grouped output");
								     Space.collection output_spaces)
								     
	    fun updateName (Exp.SYMBOL (sym, props)) = Exp.SYMBOL (sym, Property.setSpace props space)
	      | updateName name = name
	in
	    DOF.Output.rename updateName output
	end
	handle e => DynException.checkpoint "ModelSpace.propagateSpacesThroughOutputs" e


    fun add_class_to_table table symbol_table (class:DOF.class) =
	let
	    val classname = #name class
	    val outputs = !(#outputs class)
	    val output_terms = map DOF.Output.name outputs
	    val output_syms = map Term.sym2curname output_terms
	    val output_spaces = map (expToSpace o ExpProcess.term2exp) output_terms

	    val output_table = foldl
				   (fn((out, space), output_table)=>
				      SymbolTable.enter (output_table, out, space))
				   SymbolTable.empty
				   (ListPair.zip (output_syms, output_spaces))
	in
	    SymbolTable.enter (table, classname, (symbol_table, output_table))
	end
	handle e => DynException.checkpoint "ModelSpace.add_class_to_table" e
	

    fun recurse_through_intermediates (class_to_space_table, symbol_to_space_table) exps =
	let
	    val instances = List.filter ExpProcess.isInstanceEq exps
	    val classnames = SymbolSet.fromList (map (#classname o ExpProcess.deconstructInst) instances)
	    val unmatched_classnames = 
		SymbolSet.filter (fn(sym)=> not (SymbolTable.iskey (class_to_space_table, sym))) classnames
		
	    val class_to_space_table' = 
		SymbolSet.foldl
		    (fn(name,table)=> propagateSpacesThroughClass table (CurrentModel.classname2class name))
		    class_to_space_table
		    unmatched_classnames

	    (* update all the instances first since they can be determined with out traversing through intermediates *)
	    val exps = 
		map
		    (fn(exp)=> if ExpProcess.isOutputEq exp then
				   propagate_spaces_right_to_left 
				       (update_instances_in_exp class_to_space_table' exp)
			       else
				   exp)
		    exps
		    
	    (* and update the symbol table with instance equations *)
	    val symbol_to_space_table = 
		add_terms_to_table 
		    symbol_to_space_table
		    (map ExpProcess.getLHSTerm (List.filter ExpProcess.isOutputEq exps))

	    (* create two lists of symbols, those that have been cleared (or determined) and those that haven't *)
	    val cleared_symbols = SymbolSet.fromList (SymbolTable.listKeys symbol_to_space_table)
	    val all_lhs_symbols = SymbolSet.fromList (List.mapPartial 
							  (fn(exp)=> if ExpProcess.isInstanceEq exp then
									 NONE
								     else
									 SOME (ExpProcess.getLHSSymbol exp)) exps)
	    val remaining_symbols = SymbolSet.difference (all_lhs_symbols, cleared_symbols)

	    fun recurse ((table, exps), (cleared, remaining)) = 
		(if SymbolSet.isEmpty remaining then
		    ((table, exps), (cleared, remaining))
		else
		    SymbolSet.foldl
			(fn(sym, (return as ((table', exps'), (cleared', remaining'))))=> 
			   if SymbolSet.member (remaining', sym) then
			       let
				   fun find_matching_exp sym = 
				       List.find (fn(exp)=> 
						    if ExpProcess.isInstanceEq exp then
							false
						    else
							ExpProcess.getLHSSymbol exp = sym) exps'
				       handle e => DynException.checkpoint "ModelSpace.recurse_through_intermediates.recurse.find_matching_exp" e

				   val exp = case (find_matching_exp sym) of
						 SOME equation => equation
					       | NONE => except ("Can't find equation from symbol '"^(Symbol.name sym)^"'")
				   val rhs_syms = SymbolSet.fromList (ExpProcess.exp2symbols (ExpProcess.rhs exp))
						  
				   (* here's the list that we're going to have to keep on iterating over *)
				   val ((table', exps'), (cleared', _)) = 
				       recurse ((table', exps'), (cleared', SymbolSet.intersection (remaining', rhs_syms)))
										   
				   (* now update this expression with the new table *)
				   val exp' = propagate_spaces_right_to_left
						  (update_symbols_in_exp table' exp)

				   (* and update the table and the cleared, remaining lists *)
				   val table' = add_terms_to_table table' [ExpProcess.getLHSTerm exp']
				   val cleared' = SymbolSet.add (cleared', sym)
				   val remaining' = SymbolSet.difference (cleared', remaining')
				   val exps' = map 
						   (fn(exp)=> if ExpProcess.isInstanceEq exp then
								  exp
							      else if ExpProcess.getLHSSymbol exp = sym then
								  exp'
							      else
								  exp)
						   exps'
			       in
				   ((table', exps'), (cleared', remaining'))
			       end
			   else
			       return
			)
			((table, exps), (cleared, remaining))
			remaining)
		handle e => DynException.checkpoint "ModelSpace.recurse_through_intermediates.recurse" e



	    (* call the recursive function *)
	    val ((symbol_to_space_table', exps'),_) = recurse ((symbol_to_space_table, exps), (cleared_symbols, remaining_symbols))
				    

	    (* go back and just update the arguments to the instances with the spaces *)
	    val exps' = map 
			    (fn(exp)=> if (ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp) then
					   update_symbols_in_exp symbol_to_space_table' exp
				       else
					   exp)
			    exps

	in
	    ((class_to_space_table', symbol_to_space_table'), exps')

	end
	handle e => DynException.checkpoint "ModelSpace.recurse_through_intermediates" e

    and propagateSpacesThroughClass class_to_space_table class = 
	let
	    (* create a table for symbols to map to spaces *)
	    val symbol_to_space_table = SymbolTable.empty

	    (* we're going to propagate the spaces in the following order: 
	     * inputs, states, intermediates ... *)
	    val input_terms = map DOF.Input.name (!(#inputs class))
	    val symbol_to_space_table = add_terms_to_table symbol_to_space_table input_terms (* update table with inputs *)
			      
	    (* now add the states *)
	    val (init_conditions, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	    val init_conditions' = map (update_symbols_in_exp symbol_to_space_table) init_conditions
	    val init_conditions' = map propagate_spaces_right_to_left init_conditions'
	    val state_terms = map 
				  (ExpProcess.exp2term o ExpProcess.lhs)
				  init_conditions'
	    val symbol_to_space_table = add_terms_to_table symbol_to_space_table state_terms (* update table with states *)
					
	    (* now propagate through the intermediates and state equations *)
	    val (state_exps, intermediate_exps) = List.partition ExpProcess.isStateEq rest_exps
	    val ((class_to_space_table, symbol_to_space_table), intermediate_exps') = 
		recurse_through_intermediates (class_to_space_table, symbol_to_space_table) intermediate_exps
	    val state_exps' = map (update_symbols_in_exp symbol_to_space_table) state_exps
	    val _ = (#exps class) := (init_conditions' @ intermediate_exps' @ state_exps')

	    (* finally, propagate through the outputs *)
	    val outputs = !(#outputs class)
	    val outputs' = map (DOF.Output.rewrite (update_symbols_in_exp symbol_to_space_table)) outputs
	    val outputs'' = map propagateSpacesThroughOutputs outputs'
	    val _ = (#outputs class) := outputs''

	    (* add this class to the class_to_space_table symbol table *)
	    val class_to_space_table = add_class_to_table class_to_space_table symbol_to_space_table class

	in
	    class_to_space_table 
	end
	handle e => DynException.checkpoint ("ModelSpace.propagateSpacesThroughClass ["^(Symbol.name (#name class))^"]") e
in
fun propagateSpacesThroughModel (model: DOF.model) = 
    let
	(* we're going to start with the top instance *)
	val (classes, {classname, ...}, _) = model					 
	val top_class = valOf (List.find (fn{name,...}=> name=classname) classes) 

	(* we're going to iterate through the models, adding all the spaces, but first, we need to create a mapping
	 * of classes to spaces so that we know not to through the same model twice *)
	val class_to_space_table = SymbolTable.empty

	(* now, we can call the function to update the class *)
	val class_to_space_table = 
	    CurrentModel.withModel 
		model 
		(fn()=>propagateSpacesThroughClass class_to_space_table top_class)

	val () = if DynamoOptions.isFlagSet "logspaces" then
		     log_instance_table class_to_space_table
		 else
		     ()
    in
	(* no return *)
	()
    end
    handle e => DynException.checkpoint "ModelSpace.propagateSpacesThroughModel" e


end


end
