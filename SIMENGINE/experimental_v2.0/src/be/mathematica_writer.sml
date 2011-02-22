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

local
    open Mathematica
in
fun class2mexp top_class (class: DOF.class) =
    let
	(* pull out the useful parts of the class *)
	val name = #name class
	val exps = !(#exps class)
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)

	(* partition expressions *)
	val (inst_eqs, non_inst_eqs) = List.partition ExpProcess.isInstanceEq exps
	val (output_eqs, rest_eqs) = List.partition ExpProcess.isOutputEq non_inst_eqs
		       
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
	val output_names = map (Term.sym2symname o DOF.Output.name) outputs

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

	val input_names = map (Term.sym2symname o DOF.Input.name) inputs
	(* designate every variable that will be made local *)
	val local_vars = List.filter (fn(sym)=>not(List.exists (fn(sym')=>sym=sym') input_names)) (Util.uniquify (state_names @ output_names @ local_instance_symbols))

	(* grab the output equations *)
	val output_equs = List.mapPartial
			      (fn output =>
			      case DOF.Output.contents output of 
				  nil => NONE
				| [content as (Exp.TERM (Exp.SYMBOL (sym,_)))] =>
				   let val name = DOF.Output.name output
				   in if Term.sym2curname name = sym then
					 NONE (* no need to do var == var *)
				      else
					  SOME (ExpBuild.equals (ExpProcess.term2exp name, content))
				   end
				| contents => SOME (ExpBuild.equals (ExpProcess.term2exp (DOF.Output.name output), ExpBuild.explist contents)))
			      outputs
	val output_mequs = map exp2mexp output_equs
			       
	(* sym2term - find a particular term that matches a symbol - don't want derivatives *)
	fun sym2term sym = 
	    let
		val non_init_eqs = List.filter (not o ExpProcess.isInitialConditionEq) exps
		val all_terms = Util.flatmap ExpProcess.exp2termsymbols non_init_eqs
		val t = valOf (List.find (fn(t)=> Term.sym2symname t = sym) all_terms)
	    in
		case t of 
		    Exp.SYMBOL (sym, props) => Exp.SYMBOL (sym, Property.clearDerivative props)
		  | _ => t
	    end


	val local_output_mapping = map 
				       (fn(sym,sym')=>MASSIGN {lhs=sym2mexp sym, rhs=sym2mexp sym'})
				       (ListPair.zip (instance_symbols, local_instance_symbols))


	val instantiations = map exp2mexp inst_eqs

	(* create the return function *)
	fun inst2eqsvar exp = 
	    let 
		val sym = addPrefix "exps" (#instname (ExpProcess.deconstructInst exp))
	    in
		MSYMBOL sym
	    end

	(* there may still be some local variables, if that's the case, use a rewrite to remove *)
	val rewrites = MLIST (map
				  (fn(sym,sym')=> MRULE (sym2mexp sym', sym2mexp sym))
				  (ListPair.zip (instance_symbols, local_instance_symbols)))


	fun output2mexp equ =
	    let
		val {classname, instname, inpargs, outargs, props} = ExpProcess.deconstructInst equ
		val (outname, outarg) = List.hd (SymbolTable.listItemsi outargs)
	    in
		MINFIX ("==", (MEXP (Exp.TERM outarg), MBUILTIN {funname=fixname instname,
						      args=[MSYMBOL (fixname outname)]}))
	    end

	(* create a variable 'fullEquList' that includes all the submodels and the equations *)
	val full_equ_list = 
	    MREWRITE {mexp=
		      MASSIGN {lhs=MSYMBOL (Symbol.symbol "fullEquList"),
			       rhs=MBUILTIN {funname=Symbol.symbol "Join",
					     args=((MLIST (map MEXP rest_eqs))::
						   (MLIST (map output2mexp output_eqs))::
						   (map inst2eqsvar inst_eqs)) @
						  (if top_class then
						       []
						   else
						       [MLIST output_mequs])}},
		      rewrites=rewrites}

	val return_data = exp2mexp (ExpBuild.tuple [Term.sym2term (Symbol.symbol "fullEquList"),
								  Exp.TUPLE (if top_class then
										 (map sym2term output_names)
									     else
										 (map DOF.Output.name outputs))])

	val input_defaults = List.mapPartial 
				 (fn(inp)=>
				    case DOF.Input.default inp of
					SOME e => SOME (MRULE (MSYMBOL (Term.sym2symname (DOF.Input.name inp)),
									   MEXP e))
				      | NONE => NONE)
				 inputs

	val instances = map
			    (fn(equ)=>
			       let
				   val {classname, instname, inpargs, outargs, props} = ExpProcess.deconstructInst equ
			       in
				   MASSIGN {lhs= MSYMBOL instname,
					    rhs= 
					    MBUILTIN {funname=Symbol.symbol "SubModel",
						      args=
						      [MBUILTIN {funname=fixname classname,
								 args=
								 [MLIST (map 
									     (fn(sym, exp)=>
										MRULE (MSYMBOL sym,
										       MEXP exp))
									     (SymbolTable.listItemsi inpargs))]
								}
						      ]
						     }
					   }
			       end)
			    inst_eqs
    in
	MDELAYASSIGN 
	    {lhs=MFUN 
		     {name=name,
		      args=[(Symbol.symbol "rewrites_",
			     SOME (MLIST input_defaults))]
		     (* args=map (fn input =>
				     (Term.sym2symname (DOF.Input.name input), 
				      case DOF.Input.default input of 
					  SOME e => SOME (MEXP e)
					| NONE => NONE)) inputs*)
		     },
	     rhs=MODULE {locals=[Symbol.symbol "fullEquList"](*local_vars*),
			 exps=[],
			 return=
			 MWITH
			     {locals=map 
					 (fn(var)=>(var, MBUILTIN {funname=Symbol.symbol "Unique",
								   args=[MSYMBOL var]}))
					 local_vars,
			      exps=instances @ [full_equ_list],
			      return=MREWRITE {mexp=return_data, rewrites=MSYMBOL (Symbol.symbol "rewrites")}}}}
	     (*
	     MODULE
		     {locals=local_vars,
		      exps=local_output_mapping @ instantiations @ [full_equ_list],
		      return=return_data}}*)
    end
    handle e => DynException.checkpoint "MathematicaWriter.class2mexp" e
end

local
    open Layout
in
fun model2progs (model:DOF.model) =
    let
	val (classes,{name,classname},_)=model
	val iterators = CurrentModel.withModel model (fn()=> CurrentModel.iterators())
	val iter_names = map (fn(iter_name,_)=>iter_name) 
			     (List.filter (fn(_,iter_type)=> case iter_type of
								 DOF.CONTINUOUS _ => true
							       | _ => false) iterators)
	val rewrite = seq [str "/.",
			   curlyList (map (fn(sym)=>seq [str "a_",
							 bracket (str (Util.mathematica_fixname (Symbol.name sym))),
							 str "->",
							 str "a",
							 bracket (str "t")]) iter_names)]

	val prog_list = map (fn(c)=>
			       if #name c = classname then
				   Mathematica.mexp2prog (class2mexp true c) (* boolean represents the top class *)
			       else
				   Mathematica.mexp2prog (class2mexp false c)
			    ) classes

	val model_name = Util.mathematica_fixname (Symbol.name classname)
	val additional_progs = [str("m = "^model_name^"[];"),
				seq [str "equs = Modeling`model2flatEqus[m[[1]]", rewrite, str "];"],
				str("outputs = m[[2]]/.a_[_Symbol]->a;"),
				str("{p,s}=Modeling`EvalModel[equs, outputs, {t, 0, 100}];"),
				str("p")]

	fun comment msg = 
	    seq [str "(* ", str msg ,str " *)"]

	val program = 
	    align[str ("BeginPackage[\""^model_name^"`\"];"),
		  indent(align[newline,
			       comment "Class descriptions",
			       align prog_list,
			       newline,
			       comment "Model code",
			       align additional_progs]
		       ,3),
		  str ("EndPackage[];")]	    
    in
	[program]
    end
    handle e => DynException.checkpoint "MathematicaWriter.model2progs" e
end
end
