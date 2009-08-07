structure ClassProcess = 
struct

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

fun assignCorrectScope (class: DOF.class) =
    let
	val exps = !(#exps class)

	val differential_equations = List.filter ExpProcess.isFirstOrderDifferentialEq exps
	val derivative_terms = map ExpProcess.lhs differential_equations

	val symbols = Util.flatmap ExpProcess.exp2symbols derivative_terms

	val exps' = map (fn(exp)=>ExpProcess.assignCorrectScope symbols exp) exps		    

	val outputs = !(#outputs class)
	val outputs' = map (fn{name, contents, condition}=>{name=name,
							    contents=map (ExpProcess.assignCorrectScope symbols) contents,
							    condition=ExpProcess.assignCorrectScope symbols condition}) outputs
		      
    in
	((#exps class) := exps';
	 (#outputs class) := outputs')
    end


fun optimizeClass (class: DOF.class) =
    let
	val exps = !(#exps class)

	(* first, convert all subs to adds and divs to recip *)
	val exps' = map (fn(exp)=> Match.applyRulesExp [Rules.replaceSubWithNeg,
							Rules.replaceDivWithRecip] exp) exps

	val _ = (#exps class) := exps'
    in
	()
    end
    

end
