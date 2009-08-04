structure ClassProcess = 
struct

fun duplicate_class (class: DOF.class) new_name =
    let
	val {name, properties, inputs, outputs, exps} = class						       
    in
	{name=new_name,
	 properties=properties,
	 inputs=ref (!inputs),
	 outputs=ref (!outputs),
	 exps=ref (!exps)}
    end

(*
fun generateOffsets (class: DOF.class) = 
    let
	val eqs = (!(#eqs class))
	val iterators = CurrentModel.iterators()

		      
	 val blank_index_list = map (fn(i)=>0) iterators
	 val iter_names = map (fn(sym,_)=>sym) iterators
	 fun iter_index iter =
	     case List.find (fn((sym, _),i)=> sym=iter) (Util.addCount iterators) of
		 SOME (_,i)=>i
	       | NONE => DynException.stdException(("Iterator '"^(Symbol.name iter)^"' has not been defined"), "ClassProcess.generateOffsets", Logger.INTERNAL)
			 
	 fun increment_iter_by_amount iter_list iter amount = 
	     let
		 val i = iter_index iter
	     in
		 map (fn(count,i')=> if i=i' then count+amount else count) (Util.addCount iter_list)
	     end


	 fun increment_iter iter_list iter = 
	     increment_iter_by_amount iter_list iter 1

	 fun eq2iterator (eq as {lhs,...}:DOF.eq) = 
		 case lhs of
		     Exp.SYMBOL (sym, props) => 
		     (case Property.getIterator props of
			  SOME ((v,_)::rest) => v
			| _ => DynException.stdException(("No iterator defined for eq '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
							 "ClassProcess.generateOffsets.eq2iterator",
							 Logger.INTERNAL))
		   | _ => DynException.stdException(("Unexpected non-symbol for eq '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
						    "ClassProcess.generateOffsets.eq2iterator",
						    Logger.INTERNAL)

	 fun addOffset (eq as {eq_type,sourcepos,lhs,rhs}) pos =
	     case eq_type of
		 DOF.INITIAL_VALUE {offset} => {eq_type=DOF.INITIAL_VALUE {offset=pos},
						sourcepos=sourcepos,
						lhs=lhs, rhs=rhs}
	       | DOF.DIFFERENCE_EQ {offset} => {eq_type=DOF.DIFFERENCE_EQ {offset=pos},
						sourcepos=sourcepos,
						lhs=lhs, rhs=rhs}
	       | DOF.DERIVATIVE_EQ {offset} => {eq_type=DOF.DERIVATIVE_EQ {offset=pos},
						sourcepos=sourcepos,
						lhs=lhs, rhs=rhs}
	       | _ => eq

	 fun addOffsetToInst (eq as {eq_type,sourcepos,lhs,rhs}) offset_list = 
	     case eq_type of
		 DOF.INSTANCE {name,classname,offset} => {eq_type=DOF.INSTANCE {name=name, 
										classname=classname, 
										offset=offset_list},
							  sourcepos=sourcepos,
							  lhs=lhs, rhs=rhs}
	       | _ => eq

	 fun addOffsetToOneEq eqs index pos = 
	     (Util.take (eqs, index)) @
	     [addOffset (Util.nth (eqs, index)) pos] @
	     (Util.drop (eqs, index+1))

	 fun addOffsetListToOneEq eqs index pos = 
	     (Util.take (eqs, index)) @
	     [addOffsetToInst (Util.nth (eqs, index)) pos] @
	     (Util.drop (eqs, index+1))
								     
	val init_eqs = EqUtil.getInitialValueEqs eqs
	val (iter_counts, init_eqs_with_offsets) = 
	    foldl
		(fn((eq as {lhs,rhs,...},index),(offsets, init_eqs))=> 
		   let
		       val iter = eq2iterator eq
		   in
		       (increment_iter offsets iter, addOffsetToOneEq init_eqs index (Util.nth (offsets, (iter_index iter))))
		   end)
		((map (fn(i)=>0) iterators), init_eqs)
		(Util.addCount init_eqs)
		
	val difference_eqs_with_offsets = 
	    map
		(fn(eq as {lhs,...})=> 
		   let
		       val sym = Term.sym2symname lhs
		   in
		       case (List.find (fn{lhs,...}=> Term.sym2symname lhs = sym) init_eqs_with_offsets) of
			   SOME {eq_type=DOF.INITIAL_VALUE {offset},...} => addOffset eq offset
			 | _ => eq
		   end)
		(EqUtil.getDifferenceEqs eqs)
	    
	val derivative_eqs_with_offsets = 
	    map
		(fn(eq as {lhs,...})=> 
		   let
		       val sym = Term.sym2symname lhs
		   in
		       case (List.find (fn{lhs,...}=> Term.sym2symname lhs = sym) init_eqs_with_offsets) of
			   SOME {eq_type=DOF.INITIAL_VALUE {offset},...} => addOffset eq offset
			 | _ => eq
		   end)
		(EqUtil.getDerivativeEqs eqs)
	    
	val (iter_counts, instances_with_offsets) =
	    foldl
		(fn((eq as {eq_type, lhs,...}:DOF.eq,index),(iter_counts,eqs))=> 
		   case eq_type of
		       DOF.INSTANCE {offset,...} => 
		       (let
			    val size_per_iterator = map (fn(iter)=>EqUtil.eq2statesizeByIterator iter eq) iter_names
			    val iter_counts' = map (fn(a,b)=>a+b) (ListPair.zip (iter_counts, size_per_iterator))
			in
			    (iter_counts', addOffsetListToOneEq eqs index (ListPair.zip (iter_names, iter_counts)))
			end)
		     | _ => (iter_counts, eqs) (* not possible to reach this *)
		)
		(iter_counts, EqUtil.getInstances eqs)
		(Util.addCount (EqUtil.getInstances eqs))
	    
	val eqs' = init_eqs_with_offsets @ 
		   (EqUtil.getIntermediateEqs eqs) @
		   instances_with_offsets @
		   derivative_eqs_with_offsets @
		   difference_eqs_with_offsets

    in
	(#eqs class := eqs')
    end
*)

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
    

end
