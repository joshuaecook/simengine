structure MexWriter =
struct

open Printer

val i2s = Util.i2s
val r2s = Util.r2s

fun outputstruct_code (class: DOF.class) =
    let
	val {outputs, ...} = class
	val output_names = map (fn{name,...}=> ClassProcess.removePrefix (CWriterUtil.exp2c_str (Exp.TERM name))) (!outputs)
    in
	[$(""),
	 $("void generate_struct(mxArray **structptr) {"),
	 SUB([$("const char *field_names[] = {" ^ (String.concatWith ", " (map (fn(out)=>"\"" ^ out ^ "\"") output_names)) ^ "};"),
	      $("double *memptr;"),
	      $("*structptr = mxCreateStructMatrix(1, 1, "^(i2s (length (!outputs)))^", field_names);")] @
	     (Util.flatmap
		  (fn({name=term, contents, ...},i)=>
		     let
			 val name = CWriterUtil.exp2c_str (Exp.TERM term)
			 val var = "outputdata_" ^ name
			 val count = length contents + 1 (* add one for time *)
		     in
			 [$("if ("^var^".length > 0) {"),
			  SUB[$("memptr = MALLOCFUN("^var^".length*"^(i2s count)^"*sizeof(double));"),
			      $("if (NULL == memptr) {"),
			      SUB[$("ERRORFUN(Simatra:outOfMemory, \"Ran out of memory allocating output buffer of %d bytes\", "^var^".length*"^(i2s count)^"*sizeof(double));"),
				  $("return;")],
			      $("}"),
			      $("else {"),
			      SUB($("memcpy(memptr, "^var^".time, "^var^".length*sizeof(CDATAFORMAT));")::
				  (map
				      (fn(c, j)=> $("memcpy(memptr+("^(i2s (j+1))^"*"^var^".length), "^var^".vals"^(i2s j)^", "^var^".length*sizeof(CDATAFORMAT));"))
				      (Util.addCount contents))),
			      $("}"),
			      $("mwSize dims[2];"),
			      $("dims[0] = "^var^".length; dims[1] = "^(i2s count)^";"),
			      $("mxArray *"^name^"_array = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);"),
			      $("mxSetData("^name^"_array, memptr);"),
			      $("mxSetFieldByNumber(*structptr, 0, "^(i2s i)^", "^name^"_array);")
			     ],			  
			  $("}")]
		     end)
		  (Util.addCount (!outputs)))
	    ),	 
	 $("}")]
    end

fun stateoverride_code () =
    [$(""),
     $("int parseStateInits(const mxArray *arraydata) {"),
     SUB[$("int m_size = mxGetM(arraydata);"),
	 $("int n_size = mxGetN(arraydata);"),
	 $(""),
	 $("if (m_size != 1 || n_size != STATESPACE) {"),
	 SUB[$("ERRORFUN(Simatra:inputTypeError, \"The input state array should have dimensions of 1x%d, not %dx%d.\", STATESPACE, m_size, n_size);"),
	     $("return 1;")],	     
	 $("}"),
	 $(""),
	 $("double *ptr = mxGetData(arraydata);"),
	 $("int i;"),
	 $("for (i = STATESPACE-1; i>=0; i--) {"),
	 SUB[$("model_states[i] = ptr[i];")],
	 $("}"),
	 $(""),
	 $("return 0;")],
     $("}")]

fun inputstruct_code (class: DOF.class) =
    let
	val {inputs, ...} = class
	val input_names = map (fn{name,...}=> ExpProcess.exp2str (Exp.TERM name)) (!inputs)
    in
	[$(""),
	 $("int validate_inputs(CDATAFORMAT *inputs) {"),
	 SUB(foldl 
		 (fn((name, i),progs)=> [$("if (mxIsNaN(inputs["^(i2s i)^"])) {"),
					 SUB[$("ERRORFUN(Simatra:undefinedInputError, \"The input "^name^" has not been defined or has been set to NaN\");"),
					     $("return 1;")],
					 $("} else {"),
					 SUB(progs),
					 $("}")])
		 [$("return 0;")]
		 (Util.addCount input_names)),
	 $("}"),
	 $(""),
	 $("int parseInputs(const mxArray *inpstruct, CDATAFORMAT *inputs) {"),
	 SUB[$("if (mxIsStruct(inpstruct)) {"),
	     SUB[$("int numfields = mxGetNumberOfFields(inpstruct);"),
		 $("int i;"),
		 $("const char *fieldname;"),
		 $("mxArray *field;"),
		 $("for (i=0;i<numfields;i++) {"),
		 SUB([$("fieldname = mxGetFieldNameByNumber(inpstruct, i);"),
		      $("field = mxGetFieldByNumber(inpstruct, 0, i);"),
		      $("if (1 != mxGetM(field) || 1 != mxGetN(field)) {"),
		      SUB[$("ERRORFUN(Simatra:inputTypeError, \"The value for field %s must be a scalar\", fieldname);"),
			  $("return 1;")],
		      $("}")] @
		     (foldl 
			  (fn((name, i),progs)=>[$("if (0 == strcmp(fieldname, \""^name^"\")) {"),
						 SUB[$("inputs["^(i2s i)^"] = mxGetScalar(field);"),
						     $("if (mxIsNaN(inputs["^(i2s i)^"])) {"),
						     SUB[$("ERRORFUN(Simatra:undefinedInputError, \"The input %s has not been defined or has been set to NaN\", fieldname);"),
							 $("return 1;")],
						     $("}")],
						 $("}"),
						 $("else {"),
						 SUB(progs),
						 $("}")])
			  ([$("ERRORFUN(Simatra:undefinedInputError, \"The input %s specified does not exist\", fieldname);"),
			    $("return 1;")])
			  (Util.addCount input_names))),
		 $("}")],
	     $("}"),
	     $("else {"),
	     SUB[$("ERRORFUN(Simatra:argumentError, \"The second argument must be a parameter structure\");"),
		 $("return 1;")],
	 $("}"),
	     $(""),
	     $("return 0;")],
	 $("}")]
    end


fun main_code name = 
    let
	val _ = ()
    in
	[$(""),
	 $("void mexFunction(int nlhs, mxArray *plhs[ ],int nrhs, const mxArray *prhs[ ]) {"),
	 SUB[$("int errno;"),
	     $("CDATAFORMAT t = 0;"),
	     $("CDATAFORMAT t1;"),
	     $("double *data;"),
	     $(""),
	     $("// Parse right-hand side arguments"),	     
	     $("if (nrhs >= 1) {"),
	     SUB[$("switch (mxGetNumberOfElements(prhs[0])) {"),
		 SUB[$("case 1:"),
		     SUB[$("t = 0;"),
			 $("t1 = mxGetScalar(prhs[0]);"),
			 $("break;")]],
		 SUB[$("case 2:"),
		     SUB[$("data = mxGetPr(prhs[0]);"),
			 $("t = data[0];"),
			 $("t1 = data[1];"),
			 $("break;")]],		     
		 SUB[$("default:"),
		     SUB[$("mexErrMsgIdAndTxt(\"Simatra:argumentError\", \"Time input must have length of 1 or 2.\");"),
			 $("return;")]],
		 $("}")],
	     $("} else {"),
	     SUB[$("ERRORFUN(Simatra:argumentError, \"At least one argument is required.  Type 'help "^name^"' for more information.\");"),
		 $("return;")],
	     $("}"),
	     $(""),
	     $("// model processing"),
	     $("output_init(); // initialize the outputs"),
	     $("init_"^name^"(0); // initialize the states"),
	     $("CDATAFORMAT inputs[INPUTSPACE];"),
	     $(""),
	     $("init_inputs(inputs);"),
	     $(""),
	     $("// Check if there is an input argument"),
	     $("if (nrhs >= 2) {"),
	     SUB[$("if (mxIsStruct(prhs[1])) {"),
		 SUB[$("if (parseInputs(prhs[1], inputs) != 0) {"),
		     SUB[$("return;")],
		     $("}")],
		 $("} else if (mxIsNumeric(prhs[1])) {"),
		 SUB[$("if (parseStateInits(prhs[1]) != 0) {"),
		     SUB[$("return;")],
		     $("}")],
		 $("} else {"),
		 SUB[$("ERRORFUN(Simatra:argumentError, \"Unknown second argument passed to function.   Type 'help "^name^"' for more information.\");"),
		     $("return;")],
		 $("}"),
		 $("if (nrhs >= 3) {"),
		 SUB[$("if (mxIsStruct(prhs[2])) {"),
		     SUB[$("if (parseInputs(prhs[2], inputs) != 0) {"),
			 SUB[$("return;")],
			 $("}")],
		     $("} else if (mxIsNumeric(prhs[2])) {"),
		     SUB[$("if (parseStateInits(prhs[2]) != 0) {"),
			 SUB[$("return;")],
			 $("}")],
		     $("} else {"),
		     SUB[$("ERRORFUN(Simatra:argumentError, \"Unknown third argument passed to function.   Type 'help "^name^"' for more information.\");"),
			 $("return;")],
		     $("}")],
		 $("}"),
		 $("if (nrhs >= 4) {"),
		 SUB[$("ERRORFUN(Simatra:argumentError, \"More than three arguments passed to function.   Type 'help "^name^"' for more information.\");"),
		     $("return;")],		 
		 $("}")],
	     $("}"),
  	     $(""),
  	     $("if (validate_inputs(inputs) != 0) {;"),
	     SUB[$("return;")],
	     $("}"),
  	     $(""),
	     $("exec_loop(&t, t1, inputs);"),
	     $(""),
	     $("generate_struct(&plhs[0]);"),
	     $("if (nlhs > 1) {"),
	     SUB[$("mwSize dims[2];"),
		 $("dims[0] = 1; dims[1] = STATESPACE;"),
		 $("plhs[1] = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);"),
		 $("double *state_ptr = MALLOCFUN(STATESPACE*sizeof(double));"),
		 $("int i;"),
		 $("for (i=STATESPACE-1;i>=0;i--) {"),
		 SUB[$("state_ptr[i] = model_states[i];")],
		 $("}"),
		 $("mxSetData(plhs[1], state_ptr);"),
		 $("if (nlhs > 2) {"),
		 SUB[$("dims[0] = 1; dims[1] = 1;"),
		     $("plhs[2] = mxCreateNumericArray(2, dims, mxDOUBLE_CLASS, mxREAL);"),
		     $("double *t_ptr = MALLOCFUN(sizeof(double)); // add more iterators as needed here"),
		     $("*t_ptr = t;"),
		     $("mxSetData(plhs[2], t_ptr);")],
		 $("}")],
	     $("}"),
	     $("")],
	 $("}")]
    end

fun createExternalStructure props (class: DOF.class) = 
    let
	val {inputs,outputs,...} = class

	val time = Symbol.symbol "t"

	val {precision,...} = props

	fun hasTimeIterator ({eq_type=DOF.INSTANCE {offset,...},...}) = EqUtil.hasInstanceIter offset time
	  | hasTimeIterator ({eq_type=DOF.INITIAL_VALUE {offset},lhs,...}) = Term.isInitialValue lhs time
	  | hasTimeIterator _ = false

	fun eq2offset ({eq_type=DOF.INSTANCE {offset,...},...}) = EqUtil.getInstanceIterOffset offset time
	  | eq2offset ({eq_type=DOF.INITIAL_VALUE {offset},...}) = offset
	  | eq2offset eq = DynException.stdException(("Can't determine offset from eq: " ^ (ExpProcess.exp2str (EqUtil.eq2exp eq))),
						      "MexWriter.createExternalStructure.eq2offset", Logger.INTERNAL)

	fun sort_eqs eqs = 
	    let
		val eqs' = List.filter hasTimeIterator eqs
	    in
		StdFun.sort_compare (fn(a,b)=> (eq2offset a) < (eq2offset b)) eqs'
	    end

	fun findStatesInitValues basestr (class:DOF.class) =
	    Util.flatmap 
		(fn(eq as {eq_type,sourcepos,lhs,rhs})=> 
		   case eq_type of
		       DOF.INSTANCE {name, classname, offset} => 
		       let 
			   val basename' = Symbol.name name
					   
		       in
			   findStatesInitValues basename' (CurrentModel.classname2class classname)
		       end
		     | DOF.INITIAL_VALUE {offset} => 
		       if Term.isInitialValue lhs (Symbol.symbol "t") then
			   [((if basestr = "" then "" else basestr ^ "." ) ^ (Term.sym2name lhs), CWriterUtil.exp2c_str rhs)]
		       else
			   []
		     | _ => []
		)
		(sort_eqs (!(#eqs class)))
	val states = findStatesInitValues "" class
    in
	[$("% Generated output data structure for Matlab"),
	 $("% " ^ Globals.copyright),
	 $(""),
	 $("dif = struct();"),
	 $("dif.precision = "^(case precision of DOF.SINGLE => "'float'" | DOF.DOUBLE => "'double'")^";"),
	 $("dif.inputs = struct();")] @
	(map
	     (fn{name,default}=> 
		$("dif.inputs." ^ (Term.sym2name name) ^ 
		  " = "^(case default of SOME v => CWriterUtil.exp2c_str v | NONE => "nan")^";")
		(*$("dif.inputs." ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ 
		  " = struct('name', '"^(ExpProcess.exp2str (Exp.TERM name))^
		  "', 'value', "^(case default of SOME v => CWriterUtil.exp2c_str v | NONE => "nan")^");")*))
	     (!inputs)) @
	(map 
	     (fn((name, init), i)=> $((if i = 0 then "dif.states" else ("dif.states("^(i2s (i+1))^")"))
				      ^" = struct('name', '"^name^"', 'init', "^init^");"))
	     (Util.addCount states))
	 
    end

fun buildMexHelp name = 
    let
	fun write_help (filename, block) =
	    let
		val _ = Logger.log_notice ($("Generating Matlab MEX help file '"^ filename ^"'"))
		val file = TextIO.openOut (filename)
	    in
		Printer.printtexts (file, block, 0)
		before TextIO.closeOut file
	    end

	val upper = StdFun.toUpper name

	val progs = 
	    [$("%"^upper^" Executes a high-performance simulation engine"),
	     $("%producing a simulation data structure of outputs."),
	     $("% "),
	     $("% OUT = "^upper^"(STOPTIME) executes the simulation from time=0"),
	     $("% to time=STOPTIME.  The output structure OUT includes each output"),
	     $("% organized in the output groups specified in the DSL model. Each"),
	     $("% output group is a matrix where the first column is a time vector."),
	     $("% "),
	     $("% OUT = "^upper^"([STARTTIME STOPTIME]) executes the simulation from"),
	     $("% time=STARTTIME to time=STOPTIME."),
	     $("% "),
	     $("% OUT = "^upper^"([STARTTIME STOPTIME], INPUTS) sets all scalar inputs"),
	     $("% to the values specified in the INPUTS structure.  The default input"),
	     $("% can be found in the inputs field of the data structure returned by"),
	     $("% buildEngine."),
	     $("% "),
	     $("% OUT = "^upper^"([STARTTIME STOPTIME], STATE_INITS) sets all state values"),
	     $("% to the values specified in the STATE_INITS array.  The default state init"),
	     $("% can be found in the state_init field of the data structure returned by"),
	     $("% buildEngine."),	  
	     $("% "),
	     $("% OUT = "^upper^"([STARTTIME STOPTIME], INPUTS, STATE_INITS) sets both the"),
	     $("% inputs and the state initial values."),
	     $("% "),
	     $("% [OUT FINAL_STATE] = "^upper^"([STARTTIME STOPTIME], INPUTS, STATE_INITS) returns"),
	     $("% the final state vector as the second return argument.  That vector can be passed in"),
	     $("% as an initial state vector on a subsequent execution of the simulation engine."),
	     $("% "),
	     $("% [OUT FINAL_STATE FINAL_TIME] = "^upper^"([STARTTIME STOPTIME], INPUTS, STATE_INITS) returns"),
	     $("% the final simulation time as an optional third return argument."),
	     $("% "),
	     $("%   m = buildEngine('myModel.dsl');"),
	     $("%   [out final_state tf] = myModel(100, m.inputs, m.state_inits);"),
	     $("% ")
	    ]
    in
	write_help(name ^ ".m", progs)
    end


fun buildMex (model: DOF.model as (classes, inst, props)) =
    let
	val {name=inst_name, classname=class_name} = inst
	val inst_class = CurrentModel.classname2class class_name
	val class_name = Symbol.name (#name inst_class)

	val statespace = EqUtil.class2statesize inst_class

	val {iterators,precision,...} = props
	val solver = CWriter.props2solver props

	val c_data_format = case precision 
			     of DOF.SINGLE => "float" 
			      | DOF.DOUBLE => "double"

	val header_progs = CWriter.header (class_name, 
					   ["<mex.h>"],
					   ("ITERSPACE", i2s (length iterators))::			   
					   ("STATESPACE", i2s statespace)::
					   ("CDATAFORMAT", c_data_format)::
					   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
					   ("INTEGRATION_METHOD(m)", (Solver.solver2name solver) ^ "_ ## m")::
					   ("START_SIZE", "1000")::
					   ("MAX_ALLOC_SIZE", "65536000")::
					   ("MALLOCFUN", "mxMalloc")::
					   ("REALLOCFUN", "mxRealloc")::
					   ("PRINTFUN", "//")::
					   ("FPRINTFUN", "fprintf")::
					   (*("ERRORFUN(id,txt)", "(mexErrMsgIdAndText(#id, txt))")*)
					   ("ERRORFUN(ID, MESSAGE, ...)", "(mexErrMsgIdAndTxt(#ID, MESSAGE, ## __VA_ARGS__))")::
					   (Solver.solver2params solver))

(*
#define ERRORFUN(ID, MESSAGE, ARGS...) (fprintf(stderr, "Error (%s): " message "\n", #ID, ARGS...))
#define ERRORFUN(ID, MESSAGE, ARGS...) (mexErrMsgIdAndText(#ID, MESSAGE, ARGS...))
*)

	val input_progs = CWriter.input_code inst_class
	val outputdatastruct_progs = CWriter.outputdatastruct_code inst_class
	val outputinit_progs = CWriter.outputinit_code inst_class
	val init_progs = CWriter.init_code classes
	val flow_progs = CWriter.flow_code (classes, inst_class)
	val exec_progs = CWriter.exec_code (inst_class, props, statespace)
	val outputstruct_progs = outputstruct_code inst_class
	val inputstruct_progs = inputstruct_code inst_class
	val statestruct_progs = stateoverride_code()
	val main_progs = main_code class_name
	val logoutput_progs = CWriter.logoutput_code inst_class

	(* write the code *)
	val _ = CWriter.output_code(class_name ^ "_mex", ".", (header_progs @ 
							       outputdatastruct_progs @ 
							       outputinit_progs @ 
							       input_progs @ 
							       init_progs @ 
							       flow_progs @ 
							       logoutput_progs @
							       exec_progs @
							       statestruct_progs @
							       outputstruct_progs @
							       inputstruct_progs @
							       main_progs))

	val externalstruct_progs = createExternalStructure props inst_class

	fun write_struct (filename, block) =
	    let
		val _ = Logger.log_notice ($("Generating Matlab structure file '"^ filename ^"'"))
		val file = TextIO.openOut (filename)
	    in
		Printer.printtexts (file, block, 0)
		before TextIO.closeOut file
	    end

	val _ = write_struct(class_name ^ "_struct.m", externalstruct_progs)

	val _ = buildMexHelp class_name
		
    in
	System.SUCCESS
    end



end
