structure MathematicaWriter =
struct

(* Utility functions *)
fun exp2mexp exp =
    Mathematica.MEXP exp
fun sym2mexp sym =
    exp2mexp (ExpBuild.var (Symbol.name sym))
fun mexps2list mexps = 
    Mathematica.MBUILTIN 
	{funname=Symbol.symbol "List",
	 args=mexps}


fun class2mexp top_class (class: DOF.class) =
    let
	(* pull out the useful parts of the class *)
	val name = #name class
	val exps = !(#exps class)
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)

	(* partition expressions *)
	val (inst_eqs, rest_eqs) = List.partition ExpProcess.isInstanceEq exps
		       
	(* find all symbols used in instances *)
	val instance_symbols = 
	    let
		(* use a symbol set to uniquify *)
		val all_lhs_symbols = SymbolSet.flatmap 
					  (fn(exp)=>ExpProcess.exp2symbolset 
							(ExpProcess.lhs exp))
					  inst_eqs
		val all_rhs_symbols = SymbolSet.flatmap 
					  (fn(exp)=>ExpProcess.exp2symbolset 
							(ExpProcess.rhs exp))
					  inst_eqs
		val intersected_symbols = SymbolSet.intersection (all_lhs_symbols, all_rhs_symbols)
	    in
		SymbolSet.listItems intersected_symbols
	    end

	(* add prefix to symbols to make them unique *)
	fun addPrefix pre sym = Symbol.symbol (pre ^ (Symbol.name sym))
	val local_instance_symbols = map
					 (fn(sym)=>addPrefix "local" sym)
					 instance_symbols

	(* need to make each state unique *)
	val state_equs = List.filter ExpProcess.isStateEq rest_eqs
	val state_names = map ExpProcess.getLHSSymbol state_equs
	(* we also need to make each output unique *)
	val output_names = map (Term.sym2symname o #name) outputs

	(* if it's the top class, we're going to grab all the states *)
	val output_names = if top_class then
			       let 
				   (*val _ = Util.log("Before flattenEq")*)
				   val eqs = map (fn(sym)=>ClassProcess.flattenEq class sym) output_names
				   (*val _ = Util.log("After flattenEq")*)
				   val sym_list = SymbolSet.flatmap (fn(exp)=>
								       if ExpProcess.isEquation exp then
									   ExpProcess.exp2symbolset (ExpProcess.rhs exp)
								       else
									   SymbolSet.empty
								    ) eqs
				   (*val _ = Util.log ("Before: " ^ (Util.symlist2s output_names))
				   val _ = Util.log ("After: " ^ (SymbolSet.toStr sym_list))*)
			       in
				   SymbolSet.listItems sym_list
			       end
			   else
			       output_names

	val input_names = map (Term.sym2symname o #name) inputs
	(* designate every variable that will be made local *)
	val local_vars = List.filter (fn(sym)=>not(List.exists (fn(sym')=>sym=sym') input_names)) (Util.uniquify (state_names @ output_names @ local_instance_symbols))

	(* grab the output equations *)
	val output_equs = List.mapPartial
			      (fn{name,contents,...}=>
			      case contents of 
				  nil => NONE
				| [content] => SOME (ExpBuild.equals (ExpProcess.term2exp name, content))
				| _ => SOME (ExpBuild.equals (ExpProcess.term2exp name, ExpBuild.explist contents)))
			      outputs
	val output_mequs = map exp2mexp output_equs
			       
	(* all terms *)
	val all_terms = Util.flatmap ExpProcess.exp2termsymbols exps
	fun sym2term sym = valOf (List.find (fn(t)=> Term.sym2symname t = sym) all_terms)


	val local_output_mapping = map 
				       (fn(sym,sym')=>Mathematica.MASSIGN {lhs=sym2mexp sym, rhs=sym2mexp sym'})
				       (ListPair.zip (instance_symbols, local_instance_symbols))


	val instantiations = map exp2mexp inst_eqs

	(* create the return function *)
	fun inst2eqsvar exp = 
	    let 
		val sym = addPrefix "exps" (#instname (ExpProcess.deconstructInst exp))
	    in
		sym2mexp sym
	    end

	(* there may still be some local variables, if that's the case, use a rewrite to remove *)
	val rewrites = map
			   (fn(sym,sym')=> (sym2mexp sym', sym2mexp sym))
			   (ListPair.zip (instance_symbols, local_instance_symbols))

	(* create a variable 'fullEquList' that includes all the submodels and the equations *)
	val full_equ_list = 
	    Mathematica.MREWRITE {mexp=
				  Mathematica.MASSIGN {lhs=exp2mexp (ExpBuild.var "fullEquList"),
						       rhs=Mathematica.MBUILTIN {funname=Symbol.symbol "Join",
										 args=((mexps2list (map exp2mexp rest_eqs)):: 
										       (map inst2eqsvar inst_eqs)) @
										      (if top_class then
											   []
										       else
											   [mexps2list output_mequs])}},
				  rewrites=rewrites}

	val return_data = exp2mexp (ExpBuild.tuple [Term.sym2term (Symbol.symbol "fullEquList"),
								  Exp.TUPLE (if top_class then
										 (map sym2term output_names)
									     else
										 (map #name outputs))])


    in
	Mathematica.MDELAYASSIGN 
	    {lhs=Mathematica.MFUN 
		     {name=name,
		      args=map (fn{name,default}=>
				  (Term.sym2symname name, 
				   case default of 
				       SOME e => SOME (Mathematica.MEXP e)
				     | NONE => NONE)) inputs},
	     rhs=Mathematica.MODULE
		     {locals=local_vars,
		      exps=local_output_mapping @ instantiations @ [full_equ_list],
		      return=return_data}}
    end
    handle e => DynException.checkpoint "MathematicaWriter.class2mexp" e

fun model2progs (model:DOF.model) =
    let
	val (classes,{name,classname},_)=model
	val prog_list = map (fn(c)=>
			       if #name c = classname then
				   Mathematica.mexp2prog (class2mexp true c) (* boolean represents the top class *)
			       else
				   Mathematica.mexp2prog (class2mexp false c)
			    ) classes

	val additional_progs = [Printer.$(" "),
				Printer.$("m = "^(Util.mathematica_fixname (Symbol.name classname))^"[];"),
				Printer.$("equs = model2flatEqus[m[[1]]];"),
				Printer.$("outputs = m[[2]]/.a_[_]->a;"),
				Printer.$("{p,s}=EvalModel[equs, outputs, 100];"),
				Printer.$("p")]
    in
	prog_list @ additional_progs
    end
    handle e => DynException.checkpoint "MathematicaWriter.model2progs" e

end
