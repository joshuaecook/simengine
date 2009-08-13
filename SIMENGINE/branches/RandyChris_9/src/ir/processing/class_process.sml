structure ClassProcess = 
struct

val i2s = Util.i2s
val e2s = ExpPrinter.exp2str

fun duplicate_class (class: DOF.class) new_name =
    let
	val {name, properties, inputs, outputs, exps, iterators} = class						       
    in
	{name=new_name,
	 iterators=iterators,
	 properties=properties,
	 inputs=ref (!inputs),
	 outputs=ref (!outputs),
	 exps=ref (!exps)}
    end

fun getStates (class:DOF.class) =
    let
	val exps = !(#exps class)
    in
	map (Term.sym2curname o ExpProcess.exp2term o ExpProcess.lhs) (List.filter ExpProcess.isStateEq exps)
    end

fun isSymInput (class:DOF.class) sym = 
    let
	val inputs = !(#inputs class)
    in
	List.exists (fn{name,...}=>Term.sym2curname name = sym) inputs
    end

fun isSymOutput (class:DOF.class) sym = 
    let
	val outputs = !(#outputs class)
    in
	List.exists (fn{name,...}=>Term.sym2curname name = sym) outputs
    end

fun findMatchingEq (class:DOF.class) sym =  
    let
	val exps = !(#exps class)
	(* we generally want to avoid initial conditions since there are better equations to look at ... *)
	val (init_equs, exps) = List.partition ExpProcess.isInitialConditionEq exps
	val outputs = !(#outputs class)
    in
	case (List.find (fn(exp)=> 
			   List.exists (fn(sym')=>sym=sym') (ExpProcess.exp2symbols (ExpProcess.lhs exp))) exps) of
	    SOME exp => SOME exp
	  | NONE => (* check outputs *)
	    (case (List.find (fn{name,...}=> Term.sym2curname name = sym) outputs) of
		 SOME {name, contents, condition} => (case contents of
							  [] => DynException.stdException(("No equations define output '"^(e2s (Exp.TERM name))^"'"),
											  "ClassProcess.findMatchingEq",
											  Logger.INTERNAL)
							| [oneexp] => SOME (ExpBuild.equals (Exp.TERM name, oneexp))
							| rest => SOME (ExpBuild.equals (Exp.TERM name, ExpBuild.group rest)))
	       | NONE => NONE)
	     
    end

(* flattenEq does not pass through instance equations - we need a different one that will pass through instance equations *)
fun flattenEq (class:DOF.class) sym = 
    ((*Util.log ("Calling flattenEq on sym '"^(Symbol.name sym)^"'");*)
    if isSymInput class sym then
	Exp.TERM (#name (valOf (List.find (fn{name,...}=>Term.sym2curname name = sym) (!(#inputs class)))))
    else
	case findMatchingEq class sym of
	    SOME exp => 
	    let
		(*val _ = Util.log ("Found matching eq for sym '"^(Symbol.name sym)^"' -> '"^(e2s exp)^"'")*)
		val symbols = ExpProcess.exp2termsymbols (ExpProcess.rhs exp)
		val local_symbols = List.filter Term.isLocal symbols
		val matching_equations = map ((findMatchingEq class) o Term.sym2curname) local_symbols
		(* only use symbols that are from intermediates *)
		val filtered_symbols = map (fn(sym, equ)=>sym) 
					   (List.filter 
						(fn(_,equ)=> case equ of 
								 SOME e => not (ExpProcess.isInstanceEq e)
							       | NONE => false)
						(ListPair.zip (local_symbols,matching_equations)))
		val local_symbols_rewrite_rules = 
		    map
			(fn(termsym)=>
			   let
			       val find = Exp.TERM termsym
			       val test = NONE
			       val repl_exp = ExpProcess.rhs (flattenEq class (Term.sym2curname termsym))
			       (*val _ = Util.log ("flattenEq ("^(Symbol.name (#name class))^"): '"^(e2s find)^"' ->  '"^(e2s repl_exp)^"'")*)
			       val replace = Rewrite.RULE repl_exp
			   in
			       {find=find,
				test=test,
				replace=replace}			       
			   end)
			filtered_symbols
	    in
		Match.applyRewritesExp local_symbols_rewrite_rules exp
	    end
	  | NONE => DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenEq", Logger.INTERNAL))


fun findSymbols (class: DOF.class) =
    let
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)
	val exps = !(#exps class)

	fun input2symbols (inp as {name, default}) =
	    ExpProcess.exp2symbols (Exp.TERM (name)) @ 
	    (case default of
		 SOME v => ExpProcess.exp2symbols v
	       | NONE => [])

	fun output2symbols (out as {name, contents, condition}) =
	    (ExpProcess.exp2symbols (Exp.TERM name)) @
	    (Util.flatmap ExpProcess.exp2symbols contents) @
	    (ExpProcess.exp2symbols condition)

	fun exp2symbols exp =
	    ExpProcess.exp2symbols exp
	    
    in
	Util.uniquify (Util.uniquify (Util.flatmap input2symbols inputs) @
		       Util.uniquify ((Util.flatmap output2symbols outputs)) @
		       Util.uniquify ((Util.flatmap exp2symbols exps)))
    end

fun renameSym (orig_sym, new_sym) (class: DOF.class) =
    let
	(*val eqs = !(#eqs class)*)
	val exps = !(#exps class)
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)		      

	val exp_rename = ExpProcess.renameSym (orig_sym, new_sym)

	fun renameInput (input as {name, default}) =
	    {name=ExpProcess.exp2term (exp_rename (Exp.TERM name)),
	     default=case default of SOME v => SOME (exp_rename v) | NONE => NONE}
		      
	fun renameOutput (output as {name, contents, condition}) = 
	    {name=ExpProcess.exp2term (exp_rename (Exp.TERM name)),
	     contents=map exp_rename contents,
	     condition=exp_rename condition}
    in
	((*(#eqs class) := (map (EqUtil.renameSym (orig_sym, new_sym)) eqs);*)
	 (#exps class) := (map (ExpProcess.renameSym (orig_sym, new_sym)) exps);
	 (#inputs class) := (map renameInput inputs);
	 (#outputs class) := (map renameOutput outputs))
    end

val commonPrefix = "mdlvar__"

fun removePrefix str = 
    if String.isPrefix commonPrefix str then
	String.extract (str, String.size commonPrefix, NONE)
    else
	str

fun fixname name = 
    let
	fun lbrack c = if c = "[" then "" else c
	fun rbrack c = if c = "]" then "" else c
	fun period c = if c = "." then "__" else c
	fun dash c = if c = "-" then "_" else c
	fun space c = if c = " " then "_" else c
	fun underscore c = if c = "_" then "_" else c
	fun lparen c = if c = "(" then "" else c
	fun rparen c = if c = ")" then "" else c
	fun plus c = if c = "+" then "" else c					       
    in
	(StdFun.stringmap (lbrack o rbrack o period o dash o space o underscore o lparen o rparen o plus) name)
    end


(* fix according to C rules *)
fun fixSymbolNames (class: DOF.class) =
    let
	val symbols = findSymbols class
	fun fixsym sym = if Symbol.symbol "t" = sym then
			     sym (* no need to fix 't' - it's reserved *)
			 else
			     Symbol.symbol (commonPrefix ^ (fixname (Symbol.name sym)))
    in
	app (fn(sym)=>
	       let
		   val sym' = fixsym sym
	       in
		   if sym = sym' then
		       ()
		   else
		       renameSym (sym, sym') class
	       end) 
	    symbols
    end

fun class2instances class = 
    let
	val exps = !(#exps class)
    in
	List.filter ExpProcess.isInstanceEq exps
    end

fun class2orig_name (class : DOF.class) =
    let
	val {properties={classtype,...},...} = class
    in
	case classtype of
	    DOF.MASTER c => c
	  | DOF.SLAVE c => c
    end

fun class2instnames (class : DOF.class) : (Symbol.symbol * Symbol.symbol) list =
    let
	val inst_eqs = class2instances class
	fun inst2orig_names inst =
	    (ExpProcess.instOrigClassName inst, ExpProcess.instOrigInstName inst)

	fun uniq_fun ((c1,i1),(c2,i2)) = i1 = i2
	val classes_insts = Util.uniquify_by_fun uniq_fun (map inst2orig_names inst_eqs)
	val all_classes = CurrentModel.classes()
	fun name2orig_name orig_name = 
	    case List.find (fn{name,...}=>name=orig_name) all_classes of
		SOME {properties={classtype,...},...} => (case classtype of 
							     DOF.MASTER c => c 
							   | DOF.SLAVE c => c)
	      | _ => orig_name
    in
	map (fn(c,i)=>(name2orig_name c, i)) classes_insts
    end

fun class2statesize (class: DOF.class) =
    let
	val {exps,...} = class
	val initial_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
	val instance_equations = List.filter ExpProcess.isInstanceEq (!exps)
    in
	Util.sum ((map ExpProcess.exp2size initial_conditions) @ 
		  (map (fn(exp)=> 
			  let
			      val {classname,...} = ExpProcess.deconstructInst exp
			  in
			      class2statesize (CurrentModel.classname2class classname)
			  end
		       ) instance_equations))
    end

fun class2statesizebyiterator (iter: Symbol.symbol) (class: DOF.class) =
    let	
	val {name,exps,...} = class
	val initial_conditions = List.filter (ExpProcess.doesEqHaveIterator iter) (List.filter ExpProcess.isInitialConditionEq (!exps))
	val instance_equations = List.filter ExpProcess.isInstanceEq (!exps)

	(*val _ = Util.log ("in class2statesizebyiterator for class '"^(Symbol.name name)^"', # of init conditions="^(i2s (List.length initial_conditions))^", # of instances=" ^ (i2s (List.length instance_equations)))*)
    in
	Util.sum ((map ExpProcess.exp2size initial_conditions) @ 
		  (map (fn(exp)=> 
			  let
			      val {classname,...} = ExpProcess.deconstructInst exp
			  in
			      class2statesizebyiterator iter (CurrentModel.classname2class classname)
			  end
		       ) instance_equations))
    end
	

fun makeSlaveClassProperties props = 
    let
	val {classtype, classform, sourcepos} = props
    in
	{classtype=case classtype of
		       DOF.MASTER classname => DOF.SLAVE classname
		     | _ => classtype,
	 classform=classform,
	 sourcepos=sourcepos}
    end


fun assignCorrectScope (class: DOF.class) =
    let
	val exps = !(#exps class)

	val state_equations = List.filter ExpProcess.isStateEq exps
	val state_terms = map ExpProcess.lhs state_equations
	val state_iterators_options = map (TermProcess.symbol2temporaliterator o ExpProcess.exp2term) state_terms
	val state_iterators = map (fn(exp, iter)=>
				     case iter of
					 SOME i => i
				       | NONE => DynException.stdException(("State '"^(e2s exp)^"' does not have temporal iterator associated with it"), "ClassProcess.assignCorrectScope", Logger.INTERNAL))
				  (ListPair.zip (state_terms, state_iterators_options))

	val symbols = map (Term.sym2curname o ExpProcess.exp2term) state_terms
	(*Util.flatmap ExpProcess.exp2symbols state_terms*)
	val actions = map (fn(sym, iter)=>{find=Match.asym sym, test=NONE, replace=Rewrite.ACTION (sym, (fn(exp)=>ExpProcess.assignCorrectScopeOnSymbol (ExpProcess.assignIteratorToSymbol iter exp)))}) (ListPair.zip (symbols, state_iterators))

	val exps' = map (fn(exp) => Match.applyRewritesExp actions exp) exps
	(*val exps' = map (fn(exp)=>ExpProcess.assignCorrectScope symbols exp) exps*)

	(* write back expression changes *)
	val _ = (#exps class) := exps'

	val outputs = !(#outputs class)
	val outputs' = map (fn{name, contents, condition}=>
			      let
				  (*val _ = Util.log ("Processing output '"^(e2s (Exp.TERM name))^"'")*)
				  val contents' = map (Match.applyRewritesExp actions) contents
				  val condition' = Match.applyRewritesExp actions condition

			      (* TODO: The iterator for the name should be defined in modeltranslate.  If it exists,
			       the iterator vector will automatically be added to the output trace.  If it doesn't exist, 
			       only the values will be output.  This can be controlled with the "withtime" and "notime" 
			       properties *)
				  val name' = 
				      case TermProcess.symbol2temporaliterator name of
					  SOME iter => name (* keep the same *)
					| NONE => (* we have to find the iterator *)
					  let
					      (*val _ = Util.log ("Searching for iterator for output '"^(e2s (Exp.TERM name))^"'")*)
					      val sym = Term.sym2curname name
					      val flat_equ = flattenEq class sym
					      (*val _ = Util.log ("Resulting flat equation: " ^ (e2s flat_equ)) *)
					      val symbols = ExpProcess.exp2termsymbols flat_equ
					      val iterators = 
						  Util.uniquify_by_fun (fn((a,_),(a',_)) => a = a')
								       (List.mapPartial 
									    TermProcess.symbol2temporaliterator 
									    symbols)
					  in
					      case iterators of
						  [] => (* no iterators present, just return name *)			
						  name
						| [iter] => ExpProcess.exp2term
								(ExpProcess.assignIteratorToSymbol iter (Exp.TERM name))
						| rest => (* this is an error *)
						  DynException.stdException(("Particular output '"^(e2s (Exp.TERM name))^"' has more than one iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym,_)=> Symbol.name sym) iterators))),
									    "ClassProcess.assignCorrectScope",
									    Logger.INTERNAL)
					  end
				  (*val _ = Util.log("Converting name from '"^(e2s (Exp.TERM name))^"' to '"^(e2s (Exp.TERM name'))^"'")*)

			      in
				  {name=name',
				   contents=contents',
				   condition=condition'}
			      end
			   ) outputs
		      
	(* write back output changes *)
	val _ = (#outputs class) := outputs'
    in
	()
    end


fun optimizeClass (class: DOF.class) =
    let
	val exps = !(#exps class)

	(* first, convert all subs to adds and divs to recip *)
	val exps' = map (fn(exp)=> Match.applyRewritesExp [Rules.replaceSubWithNeg,
							   Rules.replaceDivWithRecip] exp) exps

	(* next, aggregate all additions *)
	val exps'' = map (fn(exp)=> Match.repeatApplyRewritesExp [(*Rules.distributeNeg,*)
								  Rules.aggregateSums,
								  Rules.aggregateProds] exp) exps'

	val _ = (#exps class) := exps'
    in
	()
    end
    

end
