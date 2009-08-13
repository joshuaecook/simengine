structure CParallelWriter =
struct

datatype status =
	 SUCCESS 
       | FAILURE of string

open Printer
exception InternalError

val i2s = Util.i2s
val r2s = Util.r2s

(* ====================  HEADER  ==================== *)

fun header (class_name, includes, defpairs) = 
    [$("// C Execution Engine for top-level model: " ^ class_name),
     $("// " ^ Globals.copyright),
     $(""),
     $("#include <simengine.h>"),
     $("#include <solvers.h>")] @
    (map (fn(inc)=> $("#include "^inc)) includes) @
    [$(""),
     $("")] @
    (map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs)

fun simengine_interface class =
    let
	val time = Symbol.symbol "t"
	fun findStatesInitValues basestr (class:DOF.class) = 
	    let
		val classname = ClassProcess.class2orig_name class
		val exps = #exps class
		val diff_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isFirstOrderDifferentialEq (!exps))
		val init_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
		fun exp2name exp = 
		    Term.sym2curname (ExpProcess.exp2term exp)
		    handle e => DynException.checkpoint ("CParallelWriter.simengine_interface.findStatesInitValues.exp2name ["^(ExpProcess.exp2str exp)^"]") e
				      
		val sorted_init_conditions = 
		    map 
			(fn(exp)=>
			   let
			       val name = exp2name exp
			   in
			       case List.find (fn(exp')=> 
						 name = exp2name (ExpProcess.lhs exp')) init_conditions of
				   SOME v => v
				 | NONE => DynException.stdException(("No initial condition found for differential equation: " ^ (ExpProcess.exp2str exp)), "CParallelWriter.simengine_interface.findStatesInitValues", Logger.INTERNAL)
			   end)
			diff_eqs_symbols
		val instances = List.filter ExpProcess.isInstanceEq (!exps)
		val class_inst_pairs = ClassProcess.class2instnames class
	    in
		(StdFun.flatmap (fn(exp)=>
				   let
				       val term = ExpProcess.exp2term (ExpProcess.lhs exp)
				       val rhs = ExpProcess.rhs exp
				   in
				       if Term.isInitialValue term time then
					   [((if basestr = "" then "" else basestr ^ "." ) ^ (Term.sym2name term), CWriterUtil.exp2c_str rhs)]
				       else 
					   []
				   end) init_conditions)
		@ (StdFun.flatmap
		       (fn(classname, instname)=>
			  let
			      val basestr' = Symbol.name instname
			  in
			      findStatesInitValues basestr' (CurrentModel.classname2class classname)
			  end)
		       class_inst_pairs)
	    end
	    handle e => DynException.checkpoint "CParallelWriter.simengine_interface.findStatesInitValues" e
	    
	val (state_names, state_defaults)  = ListPair.unzip (findStatesInitValues "" class)
	val (input_names, input_defaults) = ListPair.unzip (map (fn{name,default}=>(name,default)) (!(#inputs class)))
	val output_names = map #name (!(#outputs class))
    in
	[$(""),
	 $("const char *input_names[] = {" ^ (String.concatWith ", " (map (fn (name) => "\"" ^ (Term.sym2name name) ^ "\"") input_names)) ^ "};"),
	 $("const char *state_names[] = {" ^ (String.concatWith ", " (map (fn (name) => "\"" ^ name ^ "\"") state_names)) ^ "};"),
	 $("const char *output_names[] = {" ^ (String.concatWith ", " (map (fn (name) => "\"" ^ (Term.sym2name name) ^ "\"") output_names)) ^ "};"),
	 $("const double default_inputs[] = {" ^ (String.concatWith ", " (map (fn(default)=>
										 case default of
										     SOME v => CWriterUtil.exp2c_str v
										   | NONE => DynException.stdException("Unexpected non-default value for input", "CParallelWriter.simEngineInterface", Logger.INTERNAL))
									      input_defaults)) ^ "};"),
	 $("const double default_states[] = {" ^ (String.concatWith ", " state_defaults) ^ "};"),
	 $("const char model_name[] = \"MODEL NAME GOES HERE\";"),
	 $("const char solver[] = \"SOLVER GOES HERE\";"),
	 $(""),
	 $("const simengine_metadata semeta = {"),
	 SUB[$("0x0000000000000000ULL, // hashcode"),
	     $("NUM_MODELS,"),
	     $("solver,"),
	     $("sizeof(CDATAFORMAT)")
	    ],
	 $("};"),
	 $(""),
	 $("const simengine_interface seint = {"),
	 SUB[$("0, // Version,"),
	     $("INPUTSPACE, // Number of inputs"),
	     $("STATESPACE, // Number of states"),
	     $("OUTPUTSPACE, // Number of outputs"),
	     $("input_names,"),
	     $("state_names,"),
	     $("output_names,"),
	     $("default_inputs,"),
	     $("default_states,"),
	     $("model_name,"),
	     $("&semeta")],
	 $("};"),
	 $(""),
	 $("simengine_alloc se_alloc;"),
	 $("")]
    end
(*	
fun outputdatastruct_code class =
    let
	val outputs = #outputs class
	fun output2struct (out as {name, contents, condition}) = 
	    let
		val struct_name = "output_" ^ (CWriterUtil.exp2c_str (Exp.TERM name))
		val struct_inst = "outputdata_" ^ (CWriterUtil.exp2c_str (Exp.TERM name))
	    in
		[$(""),
		 $("struct " ^ struct_name ^ " {"),
		 SUB[$("int length;"),
		     $("int alloc_size;"),
		     $("CDATAFORMAT *time;"),
		     $("// output data: "),
		     SUB(map (fn(exp,i)=> $("CDATAFORMAT *vals" ^ (i2s i) ^ "; // " ^ (ExpProcess.exp2str exp))) (Util.addCount contents))],
		 $("};"),
		 $("struct " ^ struct_name ^ " " ^ struct_inst ^ ";")]
	    end
    in
	List.concat (map output2struct (!outputs))
    end
*)
fun outputstatestructbyclass_code (class : DOF.class) =
    let
	val classname = ClassProcess.class2orig_name class
	val exps = #exps class
	val diff_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isFirstOrderDifferentialEq (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
    in
	[$(""),
	 $("// define state structures"),
	 $("struct statedata_" ^ (Symbol.name classname) ^ " {"),	 
	 SUB($("// states (count="^(i2s (List.length diff_eqs_symbols))^")")::
	     (map (fn(sym)=>
		     let
			 val size = Term.symbolSpatialSize (ExpProcess.exp2term sym)
			 val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term sym))
		     in
			 if size = 1 then
			     $("CDATAFORMAT " ^ name ^ "[NUM_MODELS];")
			 else
			     $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"*NUM_MODELS];")
		     end) diff_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length class_inst_pairs)) ^")")::
	      (map 
		   (fn(classname, instname)=>
		      let			  
			  val size = 
			      case List.find (fn(inst)=> ExpProcess.instOrigInstName inst = instname) instances 
			       of SOME inst' => ExpProcess.instSpatialSize inst'
				| NONE => 1
		      in
			  if size = 1 then
			      $("struct statedata_" ^ (Symbol.name classname) ^ " "^(Symbol.name instname)^";")
			  else
			      $("struct statedata_" ^ (Symbol.name classname) ^ " "^(Symbol.name instname)^"["^(i2s size)^"];")
		      end)
		   class_inst_pairs))),
	 $("};")]
    end

fun outputstatestruct_code classes =
    let
	fun isMasterClass {properties={classtype,...},...} =
	    case classtype of
		DOF.MASTER _ => true
	      | _ => false
	val master_classes = List.filter isMasterClass classes

	val predeclare_statements = 
	    map
		(fn(class)=> $("struct " ^ (Symbol.name (ClassProcess.class2orig_name class)) ^ ";"))
		master_classes

    in
	($("")::($("// Pre-declare state structures"))::predeclare_statements) @
	List.concat (map outputstatestructbyclass_code master_classes)
    end

(*
fun outputinit_code class =
    let 
	val outputs = #outputs class
	fun output2progs (out as {name, contents, condition}) = 
	    let
		val var = "outputdata_" ^ (CWriterUtil.exp2c_str (Exp.TERM name))
	    in
		[$(var ^ ".length = 0;"),
		 $(var ^ ".alloc_size = START_SIZE;"),
		 $(var ^ ".time = MALLOCFUN(START_SIZE*sizeof(CDATAFORMAT));")] @
		(map (fn(c,i)=> $(var ^ ".vals" ^ (i2s i) ^ " = MALLOCFUN(START_SIZE*sizeof(CDATAFORMAT));")) (Util.addCount contents))
	    end

	val dependent_symbols = CWriterUtil.class2uniqueoutputsymbols class
	val sym_decls = map
			    (fn(term, sym)=> $("CDATAFORMAT outputsave_" ^ (Symbol.name sym) ^ ";"))
			    dependent_symbols
    in
	[$(""),
	 $("void output_init() {"),
	 SUB(List.concat (map output2progs (!outputs))),
	 $("}"),
	 $(""),
	 $("// declaring variables to store for computing outputs")] @
	 sym_decls

    end

fun initbyclass_code class =
    let
	val classname = ClassProcess.class2orig_name class
	val exps = #exps class
	val init_eqs = (List.filter ExpProcess.isInitialConditionEq (!exps))
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
    in
	[$(""),
	 $("// define state initialization functions"),
	 $("void init_" ^ (Symbol.name classname) ^ "(struct statedata_"^(Symbol.name classname)^" *states, int num_models) {"),
	 SUB([$("int modelid;"),
	      $("// states (count="^(i2s (List.length init_eqs))^")"),
	      $("for(modelid=0; modelid<num_models; modelid++){"),
	      SUB((map (fn(sym)=>
			  let
			      val size = Term.symbolSpatialSize (ExpProcess.exp2term (ExpProcess.lhs sym))
			      val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term (ExpProcess.lhs sym)))
			      val assigned_value = CWriterUtil.exp2c_str (ExpProcess.rhs sym)
			  in
			      if size = 1 then
				  $("states[AS_IDX]." ^ name ^ "[SA_IDX] = " ^ assigned_value ^ ";")
			      else (* might have to do something special here or in c_writer_util *)
				  $("#error FIXME - this path is not assigned code in c_parallel_writer.sml")
			  end) init_eqs)),
	      $("}"),
	      $("// instances (count=" ^ (i2s (List.length class_inst_pairs)) ^")")] @
	     (map 
		  (fn(classname, instname)=>
		     let			  
			 val size = 
			     case List.find (fn(inst)=> ExpProcess.instOrigInstName inst = instname) instances 
			      of SOME inst' => ExpProcess.instSpatialSize inst'
			       | NONE => 1
		     in
			 if size = 1 then
			     $("init_" ^ (Symbol.name classname) ^ "(&states[AS_IDX]."^(Symbol.name instname)^", num_models);")
			 else (* not sure what to do here *)
			     $("#error FIXME - this path is not assigned code in c_parallel_writer.sml")
		     end)
		  class_inst_pairs)),
	  $("};")]
    end
    

fun init_code classes =
    let
	fun isMasterClass {properties={classtype,...},...} =
	    case classtype of
		DOF.MASTER _ => true
	      | _ => false
	val master_classes = List.filter isMasterClass classes

	val predeclare_statements = 
	    map
		(fn(class)=> $("void init_" ^ (Symbol.name (ClassProcess.class2orig_name class)) ^ "(struct statedata_"^(Symbol.name (ClassProcess.class2orig_name class))^" *states, int num_models);"))
		master_classes

    in
	($("")::($("// Pre-declare state initialization functions"))::predeclare_statements) @
	List.concat (map initbyclass_code master_classes)
    end
*)
fun class2flow_code (class, top_class) =
    let
	val orig_name = ClassProcess.class2orig_name class

	val header_progs = 
	    [$(""),
	     $("__DEVICE__ int flow_" ^ (Symbol.name (#name class)) 
	       ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid) {")]

	val read_memory_progs = []

	val read_states_progs = []
	    
	val read_inputs_progs =
	    [$(""),
(*	     $("const struct statedata_"^(Symbol.name orig_name)^" *y = &py[modelid];"),
	     $("struct statedata_"^(Symbol.name orig_name)^" *dydt = &pdydt[modelid];"),
*)
	     $("// mapping inputs to variables")] @ 
	    (map
		 (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];"))
		 (Util.addCount (!(#inputs class))))


	(* filter out all the unneeded expressions *)
	val (initvalue_exps, rest_exps) = List.partition ExpProcess.isInitialConditionEq (!(#exps class))
	val (valid_exps, rest_exps) = List.partition (fn(exp)=>(ExpProcess.isIntermediateEq exp) orelse
							     (ExpProcess.isInstanceEq exp) orelse
							     (ExpProcess.isFirstOrderDifferentialEq exp)) rest_exps
	val _ = if (List.length rest_exps > 0) then
		    (Logger.log_error($("Invalid expressions reached in code writer while writing class " ^ (Symbol.name (ClassProcess.class2orig_name class))));
		     app (fn(exp)=> Util.log ("  Offending expression: " ^ (ExpProcess.exp2str exp))) rest_exps;
		     DynException.setErrored())
		else
		    ()

	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate, instance, and differential equation expressions")] @
	    (let
		 val progs =
		     Util.flatmap
			 (fn(exp)=>
			    if (ExpProcess.isIntermediateEq exp) then
 				[$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str exp) ^ ";")]
			    else if (ExpProcess.isFirstOrderDifferentialEq exp) then
 				[$((CWriterUtil.exp2c_str exp) ^ ";")]
			    else if (ExpProcess.isInstanceEq exp) then
				let
				    val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst exp
				    val orig_instname = case Fun.getRealInstName props of
							     SOME v => v
							   | NONE => instname

				    val class = CurrentModel.classname2class classname

				    val calling_name = "flow_" ^ (Symbol.name classname)

				    val inpvar = Unique.unique "inputdata"
				    val outvar = Unique.unique "outputdata"

				    val inps = "CDATAFORMAT " ^ inpvar ^ "[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str inpargs)) ^ "};"
				    val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (List.length outargs))^"];"
				in
				    [SUB([$("// Calling instance class " ^ (Symbol.name classname)),
					  $("// " ^ (CWriterUtil.exp2c_str exp)),
					  $(inps), $(outs_decl),
					  $(calling_name ^ "(t, &y[STRUCT_IDX]."^(Symbol.name orig_instname)^", &dydt[STRUCT_IDX]."^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration, 0);")] @
					 let
					     val symbols = map
							       (fn(outsym) => Term.sym2curname outsym)
							       outargs
					 in
					     map
						 (fn((sym, {name, contents, condition}),i')=> 
						    $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ outvar ^
						      "["^(i2s i')^"]; // Mapped to "^(Symbol.name classname)^": "^(ExpProcess.exp2str (List.hd (contents)))))
						 (Util.addCount (ListPair.zip (symbols, !(#outputs class))))
					 end)

				    ]
				end
			    else
				DynException.stdException(("Unexpected expression '"^(ExpProcess.exp2str exp)^"'"), "CParallelWriter.class2flow_code.equ_progs", Logger.INTERNAL)
			 )
			 valid_exps
	     in
		 progs
	     end)
	    
	val state_progs = []

	val output_progs = 
	    if top_class then
		[$(""),
		 $("// writing output variables"),
		 $("if (first_iteration) {"),
		 SUB(map
			 (fn(t,s)=> $("outputsave_" ^ (Symbol.name s) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM t)) ^ ";"))
			 (CWriterUtil.class2uniqueoutputsymbols class)),
		 $("}")]
	    else
		[$(""),
		 $("// writing output data "),
		 SUB(map 
			 (fn({name,contents,condition},i)=> 
			    let
				val _ = if length contents = 1 then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(ExpProcess.exp2str condition)^"' when used as a submodel"), "CParallelWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), 
							       "CParallelWriter.class2flow_code", 
							       Logger.INTERNAL)
			    end) (Util.addCount (!(#outputs class))))]

	val mapping_back_progs = []

    in
	header_progs @
	[SUB(read_states_progs @
	     read_inputs_progs @
	     equ_progs @
	     state_progs @
	     output_progs @
	     mapping_back_progs @
	     [$(""),
	      $("return 0;")]),
	 $("}"),
	 $("")]
    end

fun flow_code (classes: DOF.class list, topclass: DOF.class) = 
    let
	fun isInline (class: DOF.class) =
	    let
		val {properties={classform,...},...} = class
	    in
		case classform of 
		    DOF.FUNCTIONAL => true
		  | _ => false
	    end

	val fundecl_progs = map
				(fn(class) => 
				   let
				       val orig_name = ClassProcess.class2orig_name class
				   in
				       if isInline class then
					   $("CDATAFORMAT "^(Symbol.name (#name class))^"("^(String.concatWith ", " (map (fn{name,...}=> "CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name))) (!(#inputs class))))^");")
				       else
					   $("__DEVICE__ int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid);")
				   end)
				classes
	val iterators = CurrentModel.iterators()
				
	val flow_progs = List.concat (map (fn(c)=>
					     if isInline c then
						 (Logger.log_error ($("Functional classes like '"^(Symbol.name (#name c))^"' are not supported"));
						  DynException.setErrored();
						  [])
					     else
						 class2flow_code (c,#name c = #name topclass)) classes)

	val top_level_flow_progs = [$"",
				    $("__DEVICE__ int model_flows(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, unsigned int first_iteration, unsigned int modelid){"),
				    SUB[$("return flow_" ^ (Symbol.name (#name topclass)) ^ "(t, (const struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)y, (struct statedata_"^(Symbol.name (ClassProcess.class2orig_name topclass))^"*)dydt, inputs, outputs, first_iteration, modelid);")],
				    $("}")]
    in
	[$("// Flow code function declarations")] @
	fundecl_progs @
	flow_progs @
	top_level_flow_progs
    end

(*
fun input_code (class: DOF.class) =
    [$(""),
     $("void init_inputs(double *inputs) {"),
     SUB(map 
	     (fn({name,default},i)=> $("inputs["^(i2s i)^"] = " ^(case default 
								of SOME t => CWriterUtil.exp2c_str t
								 | NONE => "(0.0/0.0)")^ "; // " ^ (ExpProcess.exp2str (Exp.TERM name))))
	     (Util.addCount (!(#inputs class)))),
     $("}")]
*)

fun output_code (name, location, block) =
    let
      val filename = location ^ "/" ^ name ^ "_parallel.c"
      val _ = Logger.log_notice ($("Generating C source file '"^ filename ^"'"))
      val file = TextIO.openOut (filename)
    in
      Printer.printtexts (file, block, 0)
      before TextIO.closeOut file
    end

fun props2solver props =
    let
    	val iterators = #iterators props
	val solver = case List.find (fn(sym, itertype) => 
				       (case itertype of
					    DOF.CONTINUOUS solver => true
					  | DOF.DISCRETE => false)) iterators of
			 SOME (sym, DOF.CONTINUOUS solver) => solver
		       | _ => DynException.stdException ("Requiring at least one differential equation", "CParallelWriter.buildC", Logger.INTERNAL)
    in
	solver
    end

fun exec_code (class:DOF.class, props, statespace) =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$(""),
	 $("int exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, unsigned int num_models, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {"),
	 SUB[$("int modelid = 0;"),
	     $("solver_props props;"),
	     $("props.timestep = DT;"),
	     $("props.abstol = ABSTOL;"),
	     $("props.reltol = RELTOL;"),
	     $("props.starttime = *t;"),
	     $("props.stoptime = t1;"),
	     $("props.time = t;"),
	     $("props.model_states = model_states;"),
	     $("props.inputs = inputs;"),
	     $("props.outputs = NULL;"),
	     $("props.first_iteration = TRUE;"),
	     $("props.statesize = STATESPACE;"),
	     $("props.inputsize = INPUTSPACE;"),
	     $("props.num_models = num_models;"),
	     $(""),
	     $("INTEGRATION_MEM *mem = SOLVER(INTEGRATION_METHOD, init, TARGET, SIMENGINE_STORAGE, &props);"),
	     $("for(modelid=0; modelid<num_models; modelid++){"),
	     SUB[$("while (t[modelid] < t1) {"),
		 SUB[$("CDATAFORMAT prev_t = *t;"),
		     $("int status = SOLVER(INTEGRATION_METHOD, eval, TARGET, SIMENGINE_STORAGE, mem, modelid);"),
		     $("if (status != 0) {"),
		     SUB[$("return ERRCOMP;")],
		     $("}"),
		     $("if(modelid == 0)"),
		     $("if (log_outputs(prev_t, (struct statedata_"^orig_name^"*) model_states, outputs, modelid) != 0) {"),
		     SUB[$("return ERRMEM;")],
		     $("}")],
		 $("}")],
	     $("}"),
	     $("SOLVER(INTEGRATION_METHOD, free, TARGET, SIMENGINE_STORAGE, mem);")],
	 $("return SUCCESS;"),
	 $("}")]
    end

fun logoutput_code class =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
	val dependent_symbols = CWriterUtil.class2uniqueoutputsymbols class
	val sym_decls = map
			    (fn(term, sym)=> 
			       let
				   val local_scope = case term of
							 Exp.SYMBOL (_, props) => (case Property.getScope props of
										       Property.LOCAL => true
										     | _ => false)
						       | _ => DynException.stdException (("Unexpected non symbol"), "CParallelWriter.logoutput_code", Logger.INTERNAL)
			       in
				   if local_scope then
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = outputsave_" ^ (Symbol.name sym) ^ ";")
				   else
				       $("CDATAFORMAT " ^ (Symbol.name sym) ^ " = " ^ (CWriterUtil.exp2c_str (Exp.TERM term)) ^ ";")
			       end)
			    dependent_symbols
	val output_exps =Util.flatmap
			      (fn(out as {condition, contents, name})=> 
				 let
				     val var = "outputdata_" ^ (CWriterUtil.exp2c_str (Exp.TERM name))
				 in
				     [$("{ // Generating output for symbol " ^ (ExpProcess.exp2str (Exp.TERM name))),
				      SUB[$("int cond = " ^ (CWriterUtil.exp2c_str condition) ^ ";"),
					  $("if (cond) {"),
					  SUB([$("if ("^var^".length == "^var^".alloc_size) {"),
					       SUB([$("CDATAFORMAT *new_ptr = REALLOCFUN("^var^".time, "^var^".alloc_size*2*sizeof(CDATAFORMAT));"),
						   $("if (NULL == new_ptr) return 1;"),
						    $(var ^ ".time = new_ptr;")] @
						   (Util.flatmap 
							(fn(_, i)=> [$("new_ptr = REALLOCFUN("^var^".vals"^(i2s i)^", "^var^".alloc_size*2*sizeof(CDATAFORMAT));"),
								     $("if (NULL == new_ptr) return 1;"),
								     $(var^".vals"^(i2s i)^" = new_ptr;")])
							(Util.addCount contents)) @
						   [$(var^".alloc_size *= 2;")]),
					       $("}"),					      
					       $(var^".time["^var^".length] = t;"),
					       $("PRINTFUN(\"%g \", t);")] @
					      (Util.flatmap
						   (fn(exp,i)=> [$(var^".vals"^(i2s i)^"["^var^".length] = "^(CWriterUtil.exp2c_str exp)^";"), 
								 $("PRINTFUN(\"%g \", "^var^".vals"^(i2s i)^"["^var^".length]);")])
						   (Util.addCount contents)) @
					      [$(var^".length++;")]),
					  $("}")
					 ],
				      $("}")]
				 end)
			      (!(#outputs class))

    in
	[$(""),
	 $("int log_outputs(double t, const struct statedata_"^orig_name^" *y, simengine_output *outputs, int modelid) {"),
	 SUB(sym_decls @
	     [$("")] @
	     output_exps @
	     [$(""),
	      $("PRINTFUN(\"\\n\");"),
	      $("return 0;")]
	    ),
	 $("}")]
    end

fun main_code class =
    let
	val name = Symbol.name (#name class)
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$(""),
	 $("const simengine_interface *simengine_getinterface(){"),
	 SUB[$("return &seint;")],
	 $("}"),
	 $(""),
	 $("simengine_result *simengine_runmodel(double start_time, double stop_time, unsigned int num_models, double *inputs, double *states, simengine_alloc *alloc){"),
	 SUB[$("CDATAFORMAT model_states[NUM_MODELS*STATESPACE];"),
	     $("CDATAFORMAT parameters[NUM_MODELS*INPUTSPACE];"),
	     $("CDATAFORMAT t[NUM_MODELS];"),
	     $("CDATAFORMAT t1 = stop_time;"),
	     $("int stateid;"),
	     $("int modelid;"),
	     $("int inputid;"),
	     $("int i;"),
	     $(""),
	     $("// Set up allocation functions"),
	     $("if(alloc){"),
	     SUB[$("se_alloc.malloc = alloc->malloc;"),
		 $("se_alloc.realloc = alloc->realloc;"),
		 $("se_alloc.free = alloc->free;")],
	     $("}"),
	     $("else{"),
	     SUB[$("se_alloc.malloc = malloc;"),
		 $("se_alloc.realloc = realloc;"),
		 $("se_alloc.free = free;")],
	     $(" }"),
	     $(""),
	     $("// Create result structure"),
	     $("simengine_result *seresult = (se_alloc.malloc)(sizeof(simengine_result));"),
	     $(""),
	     $("// Couldn't allocate return structure, return NULL"),
	     $("if(!seresult) return NULL;"),
	     $(""),
	     $("// Check that the number of models matches"),
	     $("if(num_models != semeta.num_models){"),
	     SUB[$("seresult->status = ERRNUMMDL;"),
		 $("seresult->status_message = errors[ERRNUMMDL];"),
		 $("seresult->outputs = NULL;"),
		 $("return seresult;")],
	     $("}"),
	     $(""),
	     $("// Allocate return structures"),
	     $("seresult->outputs = (se_alloc.malloc)(semeta.num_models * seint.num_outputs * sizeof(simengine_output));"),
	     $("if(!seresult->outputs){"),
	     SUB[$("seresult->status = ERRMEM;"),
		 $("seresult->status_message = errors[ERRMEM];"),
		 $("seresult->outputs = NULL;"),
		 $("return seresult;")],
	     $("}"),
	     $(""),
	     $(""),
	     $("for(modelid=0; modelid<NUM_MODELS; modelid++){"),
	     SUB[$("t[modelid] = start_time;"),
		 $("for(stateid=0;stateid<STATESPACE;stateid++){"),
		 SUB[$("model_states[TARGET_IDX(seint.num_states, semeta.num_models, stateid, modelid)] = states[AS_IDX(seint.num_states, semeta.num_models, stateid, modelid)];")],
		 $("}"),
		 $("for(inputid=0;inputid<INPUTSPACE;inputid++){"),
		 SUB[$("parameters[TARGET_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)] = inputs[AS_IDX(seint.num_inputs, semeta.num_models, inputid, modelid)];")],
		 $("}")],
	     $("}"),
	     $(""),
	     $("// Initialization of output structures"),
	     $("for(i = 0; i<semeta.num_models*seint.num_outputs; i++){"),
	     SUB[$("seresult->outputs[i].alloc = START_SIZE;"),
		 $("seresult->outputs[i].num_quantities = seint.output_num_quantities[i];"),
		 $("seresult->outputs[i].num_samples = 0;"),
		 $("seresult->outputs[i].data = (se_alloc.malloc)(START_SIZE*seint.output_num_quantities[i]*sizeof(CDATAFORMAT));")],
	     $("}"),
	     $(""),
	     $("seresult->status = exec_loop(t, t1, semeta.num_models, parameters, model_states, seresult->outputs);"),
	     $("seresult->status_message = simengine_errors[seresult->status];"),
	     $("return seresult;")
	    ],
	 $("}")
	]
    end

fun buildC (model: DOF.model as (classes, inst, props)) =
    let
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val class_name = Symbol.name (#name inst_class)

	val statespace = ClassProcess.class2statesize inst_class

	val {iterators,precision,...} = props
	val solver = props2solver props

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val header_progs = header (class_name, 
				   [], (* no additional includes *)
				   ("ITERSPACE", i2s (length iterators))::
				   ("STATESPACE", i2s statespace)::
				   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
				   ("OUTPUTSPACE", i2s (length (!(#outputs inst_class))))::
				   ("INTEGRATION_METHOD", (Solver.solver2name solver))::
				   ("INTEGRATION_MEM", (Solver.solver2name solver) ^ "_mem")::
				   ("START_SIZE", "1000")::
				   ("MAX_ALLOC_SIZE", "65536000")::
				   (Solver.solver2params solver))

	val simengine_interface_progs = simengine_interface inst_class
(*	val input_progs = input_code inst_class*)
(*	val outputdatastruct_progs = outputdatastruct_code inst_class*)
	val outputstatestruct_progs = outputstatestruct_code classes
(*	val outputinit_progs = outputinit_code inst_class *)
(*	val init_progs = init_code classes *)
	val flow_progs = flow_code (classes, inst_class)
	val exec_progs = exec_code (inst_class, props, statespace)
	val main_progs = main_code inst_class
	val logoutput_progs = logoutput_code inst_class

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @ 
					      simengine_interface_progs @
(*					      outputdatastruct_progs @ *)
					      outputstatestruct_progs @
(*					      outputinit_progs @
					      input_progs @
					      init_progs @ *)
					      flow_progs @ 
					      logoutput_progs @
					      exec_progs @
					      main_progs))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CParallelWriter.buildC" e

end
