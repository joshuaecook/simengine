structure CWriter =
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
     $("#include <stdio.h>"),
     $("#include <stdlib.h>"),
     $("#include <math.h>"),
     $("#include <string.h>"),
     $("#define CDATAFORMAT double"),
     $("#include <solvers.h>")] @
    (map (fn(inc)=> $("#include "^inc)) includes) @
    [$(""),
     $("int fun_invocations = 0, steps = 0;"),
     $("")] @
    (map (fn(name,value)=> $("#define " ^ name ^ " " ^ value)) defpairs) @
    [$(""),
     $("static CDATAFORMAT model_states[STATESPACE];"), (* we need to work on this to make it more flexible *)
     $("")]

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
			     $("CDATAFORMAT " ^ name ^ ";")
			 else
			     $("CDATAFORMAT " ^ name ^ "["^(i2s size)^"];")
		     end) diff_eqs_symbols) @
	     ($("// instances (count=" ^ (i2s (List.length instances)) ^")")::
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
	 $("void init_" ^ (Symbol.name classname) ^ "(struct statedata_"^(Symbol.name classname)^" *states) {"),	 
	 SUB($("// states (count="^(i2s (List.length init_eqs))^")")::
	     (map (fn(sym)=>
		     let
			 val size = Term.symbolSpatialSize (ExpProcess.exp2term (ExpProcess.lhs sym))
			 val name = Symbol.name (Term.sym2curname (ExpProcess.exp2term (ExpProcess.lhs sym)))
			 val assigned_value = CWriterUtil.exp2c_str (ExpProcess.rhs sym)
		     in
			 if size = 1 then
			     $("states->" ^ name ^ " = " ^ assigned_value ^ ";")
			 else (* might have to do something special here or in c_writer_util *)
			     $("states->" ^ name ^ " = " ^ assigned_value ^ ";")
		     end) init_eqs) @
	     ($("// instances (count=" ^ (i2s (List.length instances)) ^")")::
	      (map 
		   (fn(classname, instname)=>
		      let			  
			  val size = 
			      case List.find (fn(inst)=> ExpProcess.instOrigInstName inst = instname) instances 
			       of SOME inst' => ExpProcess.instSpatialSize inst'
				| NONE => 1
		      in
			  if size = 1 then
			      $("init_" ^ (Symbol.name classname) ^ "(&states->"^(Symbol.name instname)^");")
			  else (* not sure what to do here *)
			      $("init_" ^ (Symbol.name classname) ^ "(&states->"^(Symbol.name instname)^");")
		      end)
		   class_inst_pairs))),
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
		(fn(class)=> $("void init_" ^ (Symbol.name (ClassProcess.class2orig_name class)) ^ "(struct statedata_"^(Symbol.name (ClassProcess.class2orig_name class))^" *states);"))
		master_classes

    in
	($("")::($("// Pre-declare state initialization functions"))::predeclare_statements) @
	List.concat (map initbyclass_code master_classes)
    end

(*
fun class2init_code iterators class =
    [$(""),
     $("void init_" ^ (Symbol.name (#name class)) ^ "(unsigned int offset) {"),
     SUB(
     let
	 val blank_index_list = map (fn(i)=>0) iterators
	 val iter_names = map (fn(sym,_)=>sym) iterators
	 fun iter_index iter =
	     case List.find (fn((sym, _),i)=> sym=iter) (Util.addCount iterators) of
		 SOME (_,i)=>i
	       | NONE => DynException.stdException(("Iterator '"^(Symbol.name iter)^"' has not been defined"), "CWriter.class2flow_code", Logger.INTERNAL)
			 
	 fun increment_iter iter_list iter = 
	     let
		 val i = iter_index iter
	     in
		 map (fn(count,i')=> if i=i' then count+1 else count) (Util.addCount iter_list)
	     end
	 val progs = 
	     Util.flatmap
		 (fn(exp)=>
		    if ExpProcess.isInitialValueEq exp then
			(case ExpProcess.lhs exp of
			     Exp.SYMBOL (sym, props) =>
			     let
				 val iter as (itersym, itertype) =
				     case (Property.getIterator props) of
					 SOME (v::rest) => v
				       | _ => DynException.stdException(("No iterator defined for initial value '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
									"CWriter.class2init_code",
									Logger.INTERNAL)
			     in
				 case itertype of
				     Iterator.ABSOLUTE a => if a = 0 then
								[$("model_states[offset+"^(i2s offset)^"] = " ^ 
								   (CWriterUtil.exp2c_str rhs) ^
								   "; // " ^ (ExpProcess.exp2str (Exp.TERM lhs)))]
							    else
								[]
				   | _ => []
			     end
			   | _ => DynException.stdException(("Can't handle non-symbol lhs terms '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
							    "CWriter.class2init_code",
							    Logger.INTERNAL))
		    else if ExpProcess.isInstanceEq exp then
			[$("// put instance here")]
		    else
			[])
	(*	 (fn(eq as {eq_type, sourcepos, lhs, rhs})=>
		    case eq_type
		     of DOF.INITIAL_VALUE {offset} => 
			(case lhs of
			     Exp.SYMBOL (sym, props) =>
			     let
				 val iter as (itersym, itertype) =
				     case (Property.getIterator props) of
					 SOME (v::rest) => v
				       | _ => DynException.stdException(("No iterator defined for initial value '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
									"CWriter.class2init_code",
									Logger.INTERNAL)
			     in
				 case itertype of
				     Iterator.ABSOLUTE a => if a = 0 then
								[$("model_states[offset+"^(i2s offset)^"] = " ^ 
								   (CWriterUtil.exp2c_str rhs) ^
								   "; // " ^ (ExpProcess.exp2str (Exp.TERM lhs)))]
							    else
								[]
				   | _ => []
			     end
			   | _ => DynException.stdException(("Can't handle non-symbol lhs terms '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
							    "CWriter.class2init_code",
							    Logger.INTERNAL))
		      | DOF.INSTANCE {name, classname, offset} => 
			let
			    val i = if length offset = 0 then 
					DynException.stdException(("Instance of '"^(Symbol.name classname)^"' has no defined iterators"), "CWriter.class2init_code", Logger.INTERNAL)
				    else
					#2 (Util.hd offset) (* time iterator offset *)
			in
			    [$("init_" ^
			       (Symbol.name classname) ^ 
			       "(offset+"^(i2s i)^");" ^ 
			       ("// inst: " ^ (Symbol.name name)))]
			end
		      | _ => (*[$("// " ^ (ExpProcess.exp2str (EqUtil.eq2exp eq)))]*)[]
		 ) *)
		 (!((*#eqs*)#exps class))
     in
	 progs
     end
     ),
     $("}"),
     $("")]
*)
fun class2flow_code (class, top_class) =
    let
	val orig_name = ClassProcess.class2orig_name class

	val header_progs = 
	    [$(""),
	     $("int flow_" ^ (Symbol.name (#name class)) 
	       ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration) {")]

	val read_memory_progs = []

	val read_states_progs = []
	    
	val read_inputs_progs =
	    [$(""),
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
					  $(calling_name ^ "(t, &y->"^(Symbol.name orig_instname)^", &dydt->"^(Symbol.name orig_instname)^", "^inpvar^", "^outvar^", first_iteration);")] @
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
				DynException.stdException(("Unexpected expression '"^(ExpProcess.exp2str exp)^"'"), "CWriter.class2flow_code.equ_progs", Logger.INTERNAL)
			 )
			 valid_exps
	     in
		 progs
	     end)
	    
	val state_progs = []
	   (* [$(""),
	     $("// writing all state equations")] @
	    (Util.flatmap
		 (fn(eq)=>[$("// " ^ (ExpProcess.exp2str (EqUtil.eq2exp eq))),
			   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (EqUtil.eq2exp eq)) ^ ";")])
		 (List.filter EqUtil.isDerivative (!(#eqs class))))*)

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
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(ExpProcess.exp2str condition)^"' when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), 
							       "CWriter.class2flow_code", 
							       Logger.INTERNAL)
			    end) (Util.addCount (!(#outputs class))))]

	val mapping_back_progs = []
(*	    [$(""),
	     $("// mapping variables back")] @     
	    (let
		 val progs = 
		     Util.flatmap
			 (fn(eq as {eq_type,...}) => 
			    case eq_type of
				DOF.DERIVATIVE_EQ {offset} =>
				[$("dydt["^(i2s offset)^"] = "^(CWriterUtil.exp2c_str (Exp.TERM (#lhs eq)))^";")]
			      | _ => [])
			 (List.filter EqUtil.isDerivative (!(#eqs class)))
	     in
		 progs
	     end)*)


    in
	header_progs @
	[SUB((*read_memory_progs @*)
	     read_states_progs @
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
(*
fun class2flow_code (class, top_class, iterators) =
    let
	val header_progs = 
	    [$(""),
	     $("int flow_" ^ (Symbol.name (#name class)) 
	       ^ "(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration) {")]

	val read_memory_progs =
	    $("")::
	    $("// mapping y vector to separate vectors per iterator")::
	    (map (fn((sym, itertype), i)=> $("CDATAFORMAT *y_"^(Symbol.name sym)^" = y["^(i2s i)^"];")) (Util.addCount iterators))

	val read_states_progs = 
	    $("// mapping state variables first")::	    
	    let
		val blank_index_list = map (fn(i)=>0) iterators
		fun iter_index iter =
		    case List.find (fn((sym, _),i)=> sym=iter) (Util.addCount iterators) of
			SOME (_,i)=>i
		      | NONE => DynException.stdException(("Iterator '"^(Symbol.name iter)^"' has not been defined"), "CWriter.class2flow_code", Logger.INTERNAL)

		fun increment_iter iter_list iter = 
		    let
			val i = iter_index iter
		    in
			map (fn(count,i')=> if i=i' then count+1 else count) (Util.addCount iter_list)
		    end

		val progs =
		    Util.flatmap
			(fn(eq as {eq_type,lhs,...})=>
			   case eq_type 
			    of DOF.INITIAL_VALUE {offset} => 
			       (case lhs of
				    Exp.SYMBOL (sym, props) => 
				    let
					val iter as (itersym, itertype) = 
					    case (Property.getIterator props) of 
						SOME (v::rest) => v
					      | _ => DynException.stdException(("No iterator defined for initial value '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
									       "CWriter.class2flow_code",
									       Logger.INTERNAL)
				    in
					case itertype of
					    Iterator.ABSOLUTE a => if a = 0 then
								       [$("CDATAFORMAT " ^ (Symbol.name sym) ^ " = y["^(i2s offset)^"];")]
								   else
								       []
					  | Iterator.RELATIVE r => if r = 0 then
								       [$("CDATAFORMAT " ^ (Symbol.name sym) ^ " = y["^(i2s offset)^"];")]
								   else
								       []
					  | _ => []

				    end
				  | _ => DynException.stdException(("Can't handle non-symbol lhs terms '"^(ExpProcess.exp2str (EqUtil.eq2exp eq))^"'"),
								   "CWriter.class2flow_code",
								   Logger.INTERNAL))
			     | _ => []
			)
			(EqUtil.getInitialValueEqs (!(#eqs class)))
	    in
		progs
	    end
	    
	val read_inputs_progs =
	    [$(""),
	     $("// mapping inputs to variables")] @ 
	    (map
		 (fn({name,default},i)=> $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (Exp.TERM name)) ^ " = inputs[" ^ (i2s i) ^ "];"))
		 (Util.addCount (!(#inputs class))))


	val equ_progs = 
	    [$(""),
	     $("// writing all intermediate and instance expressions")] @
	    (let
		 val progs =
		     Util.flatmap
			 (fn(eq)=>
			    case #eq_type eq of
				DOF.INTERMEDIATE_EQ => [$("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (EqUtil.eq2exp eq)) ^ ";")]
			      | DOF.INSTANCE {name,classname,offset} => 
				let
				    val class = CurrentModel.classname2class classname
				    val {lhs, rhs, ...} = eq
				    val args = case rhs of 
						   Exp.FUN (name, args) => args
						 | _ => DynException.stdException("Unexpected term in instance expression", "CWriter.class2flow_code", Logger.INTERNAL)
				    val calling_name = "flow_" ^ (Symbol.name (#name class))
				    val i = if length offset = 0 then 
						DynException.stdException(("Instance of '"^(Symbol.name classname)^"' has no defined iterators"), "CWriter.class2flow_code", Logger.INTERNAL)
					    else
						#2 (Util.hd offset) (* time iterator offset *)

				    val inpvar = Unique.unique "inputdata"
				    val outvar = Unique.unique "outputdata"

				    val inps = "CDATAFORMAT " ^ inpvar ^ "[] = {" ^ (String.concatWith ", " (map CWriterUtil.exp2c_str args)) ^ "};"
				    val outs_decl = "CDATAFORMAT " ^ outvar ^ "["^(i2s (Term.termCount lhs))^"];"
				in
				    [SUB([$("// Calling instance class " ^ (Symbol.name classname)),
					  $("// " ^ (CWriterUtil.exp2c_str (EqUtil.eq2exp eq))),
					  $(inps), $(outs_decl),
					  $(calling_name ^ "(t, y+"^(i2s i)^", dydt+"^(i2s i)^", "^inpvar^", "^outvar^", first_iteration);")] @
					 let
					     val symbols = case lhs of
							       Exp.SYMBOL (sym, _) => [Symbol.name sym]
							     | Exp.TUPLE l => map (fn(t)=>case t of 
											      Exp.SYMBOL (sym, _)=> Symbol.name sym
											    | _ => DynException.stdException("Unexpected non symbol on lhs of instance expression",
															     "CWriter.class2flow_code", Logger.INTERNAL)) l
							     | _ => DynException.stdException("Unexpected non symbol on lhs of instance expression",
											      "CWriter.class2flow_code", Logger.INTERNAL)
					 in
					     map
						 (fn((sym, {name, contents, condition}),i')=> 
						    $("CDATAFORMAT " ^ sym ^ " = " ^ outvar ^
						      "["^(i2s i')^"]; // Mapped to "^(Symbol.name classname)^": "^(ExpProcess.exp2str (List.hd (contents)))))
						 (Util.addCount (ListPair.zip (symbols, !(#outputs class))))
					 end)

				    ]
				end
			      | _ => raise InternalError
			 )
			 (List.filter (fn(eq')=>EqUtil.isIntermediate(eq') orelse EqUtil.isInstance(eq')) (!(#eqs class)))
	     in
		 progs
	     end)
	    
	val state_progs = 
	    [$(""),
	     $("// writing all state equations")] @
	    (Util.flatmap
		 (fn(eq)=>[$("// " ^ (ExpProcess.exp2str (EqUtil.eq2exp eq))),
			   $("CDATAFORMAT " ^ (CWriterUtil.exp2c_str (EqUtil.eq2exp eq)) ^ ";")])
		 (List.filter EqUtil.isDerivative (!(#eqs class))))

	val output_progs = 
	    if top_class then
		[$(""),
		 $("// writing output variables"),
		 $("if (first_iteration) {"),
		 SUB(map
			 (fn(s)=> $("outputsave_" ^ (Symbol.name s) ^ " = " ^ (Symbol.name s) ^ ";"))
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
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
				val valid_condition = case condition 
						       of (Exp.TERM (Exp.BOOL v)) => v
							| _ => false
				val _ = if valid_condition then
					    ()
					else
					    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not have a condition '"^(ExpProcess.exp2str condition)^"' when used as a submodel"), "CWriter.class2flow_code", Logger.INTERNAL)
					    
			    in
				case contents of
				    [content] =>
				    $("outputs["^(i2s i)^"] = " ^ (CWriterUtil.exp2c_str (content)) ^ ";")
				  | _ => 
				    DynException.stdException (("Output '"^(ExpProcess.exp2str (Exp.TERM name))^"' in class '"^(Symbol.name (#name class))^"' can not be a grouping of {"^(String.concatWith ", " (map ExpProcess.exp2str contents))^"} when used as a submodel"), 
							       "CWriter.class2flow_code", 
							       Logger.INTERNAL)
			    end) (Util.addCount (!(#outputs class))))]

	val mapping_back_progs = 
	    [$(""),
	     $("// mapping variables back")] @     
	    (let
		 val progs = 
		     Util.flatmap
			 (fn(eq as {eq_type,...}) => 
			    case eq_type of
				DOF.DERIVATIVE_EQ {offset} =>
				[$("dydt["^(i2s offset)^"] = "^(CWriterUtil.exp2c_str (Exp.TERM (#lhs eq)))^";")]
			      | _ => [])
			 (List.filter EqUtil.isDerivative (!(#eqs class)))
	     in
		 progs
	     end)


    in
	header_progs @
	[SUB((*read_memory_progs @*)
	     read_states_progs @
	     read_inputs_progs @
	     equ_progs @
	     state_progs @
	     output_progs @
	     mapping_back_progs @
	     [$(""),
	      $("return 0;")]),
	 $("}"),
	 $("")]
    end*)

(*
fun init_code (classes: DOF.class list) = 
    let
	(* pre-declare all the init code *)
	val fundecl_progs = map
				(fn(class) => $("void init_" ^ (Symbol.name (#name class)) ^ "(unsigned int offset);"))
				classes

	val iterators = CurrentModel.iterators()
	val init_progs = List.concat (map (class2init_code iterators) classes)


    in
	[$("// Initialization code function declarations")] @
	fundecl_progs @ [$(""), $("// Initialization of flow functions")] @
	init_progs	
    end
*)
fun flow_code (classes: DOF.class list, topclass: DOF.class) = 
    let
	val fundecl_progs = map
				(fn(class) => 
				   let
				       val orig_name = ClassProcess.class2orig_name class
				   in
				       $("int flow_" ^ (Symbol.name (#name class)) ^ "(CDATAFORMAT t, const struct statedata_"^(Symbol.name orig_name)^" *y, struct statedata_"^(Symbol.name orig_name)^" *dydt, CDATAFORMAT inputs[], CDATAFORMAT outputs[], int first_iteration);")
				   end)
				classes
	val iterators = CurrentModel.iterators()
				
	val flow_progs = List.concat (map (fn(c)=>class2flow_code (c,#name c = #name topclass)) classes)
    in
	[$("// Flow code function declarations")] @
	fundecl_progs @
	flow_progs
    end

fun input_code (class: DOF.class) =
    [$(""),
     $("void init_inputs(double *inputs) {"),
     SUB(map 
	     (fn({name,default},i)=> $("inputs["^(i2s i)^"] = " ^(case default 
								of SOME t => CWriterUtil.exp2c_str t
								 | NONE => "(0.0/0.0)")^ "; // " ^ (ExpProcess.exp2str (Exp.TERM name))))
	     (Util.addCount (!(#inputs class)))),
     $("}")]

fun output_code (name, location, block) =
    let
      val filename = location ^ "/" ^ name ^ ".c"
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
		       | _ => DynException.stdException ("Requiring at least one differential equation", "CWriter.buildC", Logger.INTERNAL)
    in
	solver
    end

fun exec_code (class:DOF.class, props, statespace) =
    let
	val orig_name = Symbol.name (ClassProcess.class2orig_name class)
    in
	[$(""),
	 $("void exec_loop(CDATAFORMAT *t, CDATAFORMAT t1, CDATAFORMAT *inputs) {"),
	 SUB[$("solver_props props;"),
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
	     $("props.fun = &flow_"^(Symbol.name (#name class))^";"),
	     $(""),          
	     $("INTEGRATION_METHOD(mem) *mem = INTEGRATION_METHOD(init)(&props);"),
	     $("while (*t < t1) {"),
	     SUB[$("double prev_t = *t;"),
		 $("int status = INTEGRATION_METHOD(eval)(mem);"),
		 $("if (status != 0) {"),
		 SUB[(*$("sprintf(str, \"Flow calculated failed at time=%g\", *t);")*)
		     $("ERRORFUN(Simatra:flowError, \"Flow calculation failed at time=%g\", *t);"),
		     $("break;")],
		 $("}"),
		 $("if (log_outputs(prev_t, (struct statedata_"^orig_name^"*) model_states) != 0) {"),
		 SUB[$("ERRORFUN(Simatra:outOfMemory, \"Exceeded available memory\");"),
		     $("break;")],
		 $("}"),		 
		 $("steps++;")(*,*)
				 (*  $("PRINTFUN(\"%g,"^(String.concatWith "," (List.tabulate (statespace, fn(i)=>"%g")))^"\\n\", t, "^
				     (String.concatWith ", " (List.tabulate (statespace, fn(i)=>"model_states["^(i2s i)^"]")))^");")*)
		],
	     $("}"),
	     $("INTEGRATION_METHOD(free)(mem);")],
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
										       LOCAL => true
										     | _ => false)
						       | _ => DynException.stdException (("Unexpected non symbol"), "CWriter.logoutput_code", Logger.INTERNAL)
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
	 $("int log_outputs(double t, const struct statedata_"^orig_name^" *y) {"),
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
	[$("int main(int argc, char **argv) {"),
	 SUB[(*$("FPRINTFUN(stderr,\"Running '"^name^"' model ...\\n\");"),*)
	     $(""),
	     $("// Get the simulation time t from the command line"),
	     $("double t = 0;"),
	     $("double t1 = atof(argv[1]);"),
	     $("output_init(); // initialize the outputs"),
	     $("init_"^name^"((struct statedata_"^orig_name^"*) model_states); // initialize the states"),
	     $("CDATAFORMAT inputs[INPUTSPACE];"),
	     $(""),
	     $("init_inputs(inputs);"),
	     $(""),
	     $("exec_loop(&t, t1, inputs);"),
	     $(""),
	     $("return 0;")],
	 $("}")]
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
				   ("CDATAFORMAT", c_data_format)::
				   ("INPUTSPACE", i2s (length (!(#inputs inst_class))))::
				   ("INTEGRATION_METHOD(m)", (Solver.solver2name solver) ^ "_ ## m")::
				   ("START_SIZE", "1000")::
				   ("MAX_ALLOC_SIZE", "65536000")::
				   ("MALLOCFUN", "malloc")::
				   ("REALLOCFUN", "realloc")::
				   ("PRINTFUN", "printf")::
				   (*("ERRORFUN(id,txt)", "(fprintf(stderr, \"Error (%s): %s\\n\", #id, txt))")*)
				   ("ERRORFUN(ID, MESSAGE, ...)", "(fprintf(stderr, \"Error (%s): \" MESSAGE \"\\n\", #ID, ## __VA_ARGS__))")::
				   (Solver.solver2params solver))

	val input_progs = input_code inst_class
	val outputdatastruct_progs = outputdatastruct_code inst_class
	val outputstatestruct_progs = outputstatestruct_code classes
	val outputinit_progs = outputinit_code inst_class
	val init_progs = init_code classes
	val flow_progs = flow_code (classes, inst_class)
	val exec_progs = exec_code (inst_class, props, statespace)
	val main_progs = main_code inst_class
	val logoutput_progs = logoutput_code inst_class

	(* write the code *)
	val _ = output_code(class_name, ".", (header_progs @ 
					      outputdatastruct_progs @ 
					      outputstatestruct_progs @
					      outputinit_progs @ 
					      input_progs @ 
					      init_progs @ 
					      flow_progs @ 
					      logoutput_progs @
					      exec_progs @
					      main_progs))
    in
	SUCCESS
    end
    handle e => DynException.checkpoint "CWriter.buildC" e

end
