(* Implements a stream-oriented C language printer for DOF model data. 
 * Depends on the PP library from smlnj-lib.
 * See smlnj-lib/PP/src/pp-stream-sig.sml.
 *)
structure CWriter =
struct

(* See smlnj-lib/PP/src/pp-stream-sig.sml. *)
structure PP = PPStreamFn(structure Token = StringToken structure Device = SimpleTextIODev)

(* Constructs a PP.stream which writes to a given outstream. *)
fun withTextIOStream (outstream : TextIO.outstream) proc =
    let val device = SimpleTextIODev.openDev {dst=outstream, wid=100}
	val stream = PP.openStream device
    in
	proc stream before PP.closeStream stream
	handle e => (PP.closeStream stream; raise e)
    end


structure Emit : sig
    (* An emitter displays an item of some type 'a on the given print stream. *)
    type 'a emit = PP.stream -> 'a -> unit

    val modelEmit : DOF.model emit

    val systemEmit : {top_class: Symbol.symbol,
		      iter: DOF.systemiterator,
		      model: DOF.model} list emit

    val classEmit : (DOF.systemiterator * DOF.class) emit
    val expEmit : Exp.exp emit
    val termEmit : Exp.term emit

end = 
struct
(* Abbreviations
 * pps: "Pretty Print Stream" of type PP.stream
 *)

(* See smlnj-lib/PP/src/pp-stream-sig.sml. *)
open PP
open Printer

(* Most functions within this module conform to this type.
 * Many utility functions operate by wrapping another emitter, e.g.
 * 
 * line : 'a emit -> 'a emit
 * prefix : (string * 'a emit) -> 'a emit *)
type 'a emit = stream -> 'a -> unit

(* Emits a list of expressions delimited by a separator
 * E.g. (delimit (string,newline) pps strings) prints each string on a line.
 * delimit : ('a emit * (PP.stream -> unit)) -> stream -> 'a list -> unit *)
fun delimit (emit, separate) pps [] = ()
  | delimit (emit, separate) pps [x] = emit pps x
  | delimit (emit, separate) pps (x::xs) = 
    (emit pps x; separate pps; delimit (emit, separate) pps xs)

(* Emits a newline after displaying an item. *)
fun line emit pps item = 
    emit pps item before newline pps

(* Emits Printer.text data. *)
fun text pps ($ str) = line string pps str
  | text pps (SUB txts) =
    openHOVBox pps (Rel 2)
    before texts pps txts
    before closeBox pps
and texts pps txts = app (text pps) txts

fun archive pps filename =
    line string pps (Archive.getC filename)

(* Emits a preprocessor error. 
 * Writing may continue but the C compiler should fail;
 * output is no longer assured to be valid C code. *)
fun error pps message =
    newline pps
    before text pps ($("#error (" ^ message ^ ")"))
     
fun comma pps = string pps ", "

fun prefix (left, emit) pps item =
    string pps left before emit pps item

fun suffix (emit, right) pps item =
    emit pps item before string pps right

fun parentheses (left, emit, right) pps item =
    suffix (prefix (left, emit), right) pps item

val c_string =
    parentheses ("\"", string, "\"")

fun c_array emit =
    parentheses ("{", delimit (emit, comma), "}")

(* Emits a single-line comment followed by a newline. *)
fun comment' emit pps message = 
    line (prefix ("// ", emit)) pps message
fun comment pps message =
    comment' string pps message





fun expEmit pps (exp as Exp.FUN (typ, operands)) =
    emit_fun_expression pps (typ, operands)

  | expEmit pps (Exp.TERM term) =
    termEmit pps term

and emit_instance_call pps (iter_name, caller, equation) =
    let val {classname, instname, props={realinstname, ...}, ...} = ExpProcess.deconstructInst equation
	val instname = case realinstname of SOME name => name | NONE => instname

	val (state_read, state_write) =
	    if CurrentModel.isTopClass caller then
		("&rd_" ^ (Symbol.name iter_name) ^ "[STRUCT_IDX]." ^ (Symbol.name instname),
		 "&wr_" ^ (Symbol.name iter_name) ^ "[STRUCT_IDX]." ^ (Symbol.name instname))
	    else
		("rd_" ^ (Symbol.name iter_name) ^ "->" ^ (Symbol.name instname),
		 "wr_" ^ (Symbol.name iter_name) ^ "->" ^ (Symbol.name instname))

	val arguments = [(Symbol.name iter_name),
			 state_read,
			 state_write,
			 "subsys_rd",
			 "instInputs",
			 "instOuputs",
			 "firstIteration",
			 "modelid"]
    in prefix (("flow_"^(Symbol.name classname)),
	       parentheses ("(", delimit (string, comma), ")"))
	      pps arguments
    end


(* Emits the computation without a destination. *)
and emit_fun_expression pps (Fun.BUILTIN Fun.ASSIGN, [dst, src]) =
    expEmit pps src
  | emit_fun_expression pps (Fun.BUILTIN Fun.ASSIGN, operands) = 
    error pps ("Malformed assignment with "^(Util.i2s (List.length operands))^" operands.")
  | emit_fun_expression pps (f, operands) =
    (case FunProps.fun2cstrnotation f
      of (operation, FunProps.INFIX) => 
	 delimit (expEmit, (fn pps => string pps operation)) pps operands

       | (operation, FunProps.PREFIX) =>
	 prefix (operation, parentheses ("(",delimit (expEmit, comma),")")) 
		pps operands

       | (operation, FunProps.POSTFIX) => 
	 string pps "POSTFIX-FIXME"

       | (operation, FunProps.MATCH) => 
	 let fun emit_replacing substr =
		 if Substring.isEmpty substr then ()
		 else if Substring.isPrefix "$" substr then
		     case (Int.fromString o Substring.string o Substring.slice) (substr, 1, NONE)
		      of SOME z =>
			 let val zlen = (String.size (Int.toString z))
			     val () = if z < 1 orelse z > (List.length operands) 
				      then prefix ("$", string) pps (Int.toString z)
				      else parentheses ("(",expEmit,")") pps (List.nth (operands, (z-1)))
			 in 
			    emit_replacing (Substring.slice (substr, 1 + zlen, NONE))
			 end
		       | NONE => 
			 (string pps "$";
			  emit_replacing (Substring.slice (substr, 1, NONE)))

		 else let val (prefix, substr) = Substring.position "$" substr
		      in string pps (Substring.string prefix);
			 emit_replacing substr
		      end

	 in
	     emit_replacing (Substring.full operation)
	 end)

and termEmit pps (Exp.BOOL b) = 
    string pps (if b then "YES" else "NO")

  | termEmit pps (Exp.INT z) = 
    string pps (Util.i2s z)

  | termEmit pps (Exp.REAL r) = 
    emit_real pps r

  | termEmit pps Exp.NAN = 
    string pps "NAN"

  (* TODO negative infinity. *)
  | termEmit pps Exp.INFINITY = 
    string pps "INFINITY"

  | termEmit pps (Exp.RATIONAL (numer,denom)) = 
    emit_rational pps (numer, denom)

  | termEmit pps (Exp.SYMBOL (name, properties)) = 
    emit_symbol pps (name, properties)

  | termEmit pps term = 
    error pps "Unrecognized term."


(* Emits a rational number by converting to literal division. *)
and emit_rational pps (numer, denom) =
    expEmit pps (ExpBuild.divide (ExpBuild.int numer, ExpBuild.real (Real.fromInt denom)))

(* Emits a real number according to the precision of the expression.
 * Infinity and NaN are not allowed.
 * TODO support precision directly and eliminate the FLITERAL macro. *)
and emit_real pps r = 
    if Real.isFinite r then string pps ("FLITERAL("^(Util.real2exact_str r)^")")
    else error pps ("Number is infinite or nan.")

and emit_symbol pps (name, properties) =
    string pps (Term.sym2c_str (name, properties))

fun emit_term_name pps (Exp.SYMBOL (name, properties)) = 
    string pps (Symbol.name name)
  | emit_term_name pps _ = error pps "Not a name."

fun emit_term_declaration pps exp =
    prefix ("CDATAFORMAT ", emit_term_name) pps exp

(* Emits a name as the destination ('left' value) of an assignment.
 * Only symbol terminals are allowed. *)
fun emit_lvalue pps (exp as Exp.TERM (Exp.SYMBOL (name, properties))) =
    suffix (emit_symbol, " = ") pps (name, properties)

  | emit_lvalue pps _ = 
    error pps ("Left side of assignment must be a symbol.")




(* Emits an expression as a part of a class flow function. *)
fun emit_class_instruction pps (iterator, class, block as Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [dst, src])) =
    if ExpProcess.isInstanceEq block then
	emit_instance_equation pps (iterator, class, block)
    else if ExpProcess.isIntermediateEq block then
	prefix ("CDATAFORMAT ", emit_lvalue) pps dst
	before suffix (expEmit, ";") pps block
    else 
	emit_lvalue pps dst
	before suffix (expEmit, ";") pps block

  | emit_class_instruction pps _ = 
    error pps ("Not a class instruction.")

and emit_instance_equation pps (iterator, caller, equation) =
    let val {classname, instname, props, inpargs, outargs} = ExpProcess.deconstructInst equation
	val instance_class = CurrentModel.classname2class classname
	val iter_name = 
	    case iterator
	     of (_, DOF.UPDATE name) => name
	      | (_, DOF.POSTPROCESS name) => name
	      | (name, _) => name

	val iterators = List.filter (not o ModelProcess.isUpdateIterator) (CurrentModel.iterators ())

	fun declare_instance_input pps [] = ()
	  | declare_instance_input pps xs = 
	    let val term = (ExpProcess.exp2term o ExpBuild.var) ("instInputs["^(Int.toString (List.length xs))^"]")
	    in line (suffix (emit_term_declaration, ";")) pps term
	    end

	fun declare_instance_output pps [] = ()
	  | declare_instance_output pps ys = 
	    let val term = (ExpProcess.exp2term o ExpBuild.var) ("instOutputs["^(Int.toString (List.length ys))^"]")
	    in line (suffix (emit_term_declaration, ";")) pps term
	    end

	fun declare_subsystem pps [] = ()
	  | declare_subsystem pps iterators = 
	    let val term = (ExpProcess.exp2term o ExpBuild.var) ("system_"^(Symbol.name classname))
	    in line (suffix (emit_term_name, " subsys_rd;")) pps term
	    end

	fun assign_input pps (input, index) =
	    let val lvalue = ExpBuild.var ("instInputs["^(Int.toString index)^"]")
	    in emit_lvalue pps lvalue;
	       suffix (expEmit, ";") pps input
	    end

	fun assign_output pps (output, index) =
	    let val rvalue = ExpBuild.var ("instOutputs["^(Int.toString index)^"]")
	    in emit_lvalue pps (Exp.TERM output);
	       suffix (expEmit, ";") pps rvalue
	    end

	fun assign_subsystem pps (iterator as (iter_name, _)) =
	    let val subsys_iter = ExpBuild.var ("subsys_rd." ^  (Symbol.name iter_name))
		val subsys_states = ExpBuild.var ("subsys_rd.states_" ^  (Symbol.name iter_name))
	    in emit_lvalue pps subsys_iter;
	       string pps ("sys_rd->" ^ (Symbol.name iter_name));
	       string pps ";"; newline pps;
	       emit_lvalue pps subsys_states;
	       string pps ("sys_rd->" ^ (Symbol.name iter_name) ^ "->" ^ (Symbol.name instname));
	       string pps ";"
	    end

    in comment pps ("Calling "^(Symbol.name classname)^" instance "^(Symbol.name instname));
       comment' expEmit pps equation;
       delimit (suffix (emit_term_declaration, ";"), newline) pps outargs; newline pps;
       string pps "  {"; openHOVBox pps (Rel ~1); newline pps;

       declare_instance_input pps inpargs;
       declare_instance_output pps outargs;
       declare_subsystem pps iterators;
       newline pps;

       delimit (assign_input, newline) pps (Util.addCount inpargs); newline pps;
       newline pps;

       delimit (assign_subsystem, newline) pps iterators; newline pps;
       newline pps;

       suffix (emit_instance_call, ";") pps (iter_name, caller, equation); newline pps;
       newline pps;

       delimit (assign_output, newline) pps (Util.addCount outargs); newline pps;
       string pps "}"; closeBox pps; newline pps
    end
	
fun classEmit pps (iterator, class) =
    let val {exps, ...} = class
    in
	case !exps 
	 of [] => emit_class_prototype pps (iterator, class)
	  | _ => emit_class_definition pps (iterator, class)
    end

and emit_class_definition pps (iterator, class) =
    let val {name, properties, inputs, outputs, iterators, exps} = class
    in emit_class_signature pps (iterator, class); newline pps;
       string pps "  {"; openHOVBox pps (Rel ~1); newline pps;
       emit_class_locals pps class; newline pps;
       newline pps;

       emit_class_body pps (iterator, class); newline pps;
       newline pps;

       emit_class_outputs pps class; newline pps;
       newline pps;

       string pps "return 0;"; newline pps;
       string pps "}"; closeBox pps; newline pps
    end

and emit_class_signature pps (iterator, class : DOF.class) =
    let val {name, properties={basename, ...}, ...} = class
	val iter_name = 
	    case iterator
	     of (_, DOF.UPDATE name) => name
	      | (_, DOF.POSTPROCESS name) => name
	      | (name, _) => name

	val state_type = "state_" ^ (Symbol.name name)
	val system_type = "system_" ^ (Symbol.name basename)

	val params = ["CDATAFORMAT " ^ (Symbol.name iter_name),
		      "const " ^ state_type ^ " *rd_" ^ (Symbol.name iter_name),
		      state_type ^ " *wr_" ^ (Symbol.name iter_name),
		      system_type ^ " *sys_rd",
		      "CDATAFORMAT *inputs",
		      "CDATAFORMAT *outputs",
		      "const unsigned int firstIteration",
		      "const unsigned int modelid"]
    in
	line string pps "__HOST__ __DEVICE__";
	prefix (("int flow_"^(Symbol.name name)), 
		parentheses ("(", delimit (string, comma), ")"))
	       pps params
    end

and emit_class_prototype pps (iterator, class) =
    suffix (emit_class_signature, ";") pps (iterator, class)

and emit_class_locals pps class =
    let val {name, properties, inputs, outputs, iterators, exps} = class
	fun emit_input pps (input, index) =
	    let val {name, default} = input
		val index = 
		    if CurrentModel.isTopClass class then
			"TARGET_IDX(num_inputs, num_models, " ^ (Int.toString index) ^ ", modelid)"
		    else Int.toString index
	    in prefix ("CDATAFORMAT ", emit_lvalue) pps (Exp.TERM name);
	       prefix ("inputs", parentheses ("[", string, "]"))
		      pps index;
	       string pps ";"
	    end
    in comment pps "Local variables";
       delimit (emit_input, newline) pps (Util.addCount (!inputs))
    end


and emit_class_body pps (iterator, class) =
    let val {name, properties, inputs, outputs, iterators, exps} = class
	val equations = List.filter (fn exp => not (ExpProcess.isInitialConditionEq exp) andalso
					       (ExpProcess.isIntermediateEq exp orelse
						ExpProcess.isInstanceEq exp orelse
						ExpProcess.isStateEq exp)) (!exps)
    in comment pps "Computation of equations";
       delimit (fn pps => (fn exp => emit_class_instruction pps (iterator, class, exp)), newline) pps equations
    end

and emit_class_outputs pps class =
    if CurrentModel.isTopClass class then
	let val {outputs, ...} = class
	    fun output_terms {contents, condition, ...} =
		ExpProcess.exp2termsymbols condition @
		(Util.flatmap ExpProcess.exp2termsymbols contents)

	    val terms = Util.flatmap output_terms (!outputs)

	    fun assign_output pps exp =
		prefix ("od[modelid].", emit_term_name) pps exp
		before prefix (" = ", suffix (emit_term_name, ";")) pps exp

	in comment pps "Output data";
	   string pps "if (firstIteration) {"; newline pps;
	   string pps "output_data *od = (output_data *)outputs;"; newline pps;
	   openHOVBox pps (Rel 2);
	   delimit (assign_output, newline) pps terms; newline pps;
	   closeBox pps; newline pps;
	   string pps "}"; newline pps
	end
    else let val {outputs, ...} = class
	     fun emit_output pps (output, index) =
		 let val {contents, ...} = output
		     val lvalue = ExpBuild.var ("outputs["^(Int.toString index)^"]")
		 in case contents
		     of [item] => (emit_lvalue pps lvalue;
				   suffix (expEmit, ";") pps item)
		      | [] => error pps "Empty output."
		      | _ => error pps "Output contents too large."
		 end
	 in comment pps "Output data";
	    delimit (emit_output, newline) pps (Util.addCount (!outputs))
	 end

(*
fun emit_class_data_structures pps class =
    let val {name, properties, inputs, outputs, iterators, exps} = class
    in emit_class_state_structure pps class; newline pps;
       emit_class_output_structure pps class; newline pps
    end
*)

and emit_class_state_structure pps class = 
    let val {name, properties, inputs, outputs, iterators, exps} = class
	fun declare_member pps (exp as Exp.FUN _) =
	    if ExpProcess.isStateEq exp then 
		suffix (emit_term_declaration, ";") pps (ExpProcess.exp2term (ExpProcess.lhs exp))
	    else if ExpProcess.isInstanceEq exp then 
		suffix (declare_instance, ";") pps exp
	    else ()
	  | declare_member pps _ = ()

	and declare_instance pps exp =
	    let val {classname, instname, props={realclassname, ...}, ...} = ExpProcess.deconstructInst exp
		val name = case realclassname of SOME name => name | _ => classname
	    in prefix ("state_", string) pps (Symbol.name name);
	       string pps " ";
	       string pps (Symbol.name instname)
	    end

	val members = List.filter (fn exp => ExpProcess.isStateEq exp orelse ExpProcess.isInstanceEq exp) (!exps)

	val (states, rest) = List.partition ExpProcess.isStateEq (!exps)
	val (instances, rest) = List.partition ExpProcess.isInstanceEq rest
    in comment pps ("States of " ^ (Symbol.name name));
       string pps "typedef struct {"; newline pps;
       delimit (declare_member, newline) pps (states @ instances); newline pps;
       string pps ("} state_" ^ (Symbol.name name));
       string pps ";"; newline pps
    end

and emit_class_output_structure pps ({outputs=ref [], ...} : DOF.class) = ()
  | emit_class_output_structure pps (class as {name, outputs, ...}) = 
    let fun output_terms {contents, condition, ...} =
	    ExpProcess.exp2termsymbols condition @
	    (Util.flatmap ExpProcess.exp2termsymbols contents)
	val terms = Util.flatmap output_terms (!outputs)
    in comment pps ("Outputs of " ^ (Symbol.name name));
       string pps "typedef struct {"; newline pps;
       delimit (suffix (emit_term_declaration, ";"), newline) 
	       pps terms;
       newline pps;
       string pps "} output_data;"; newline pps;
       newline pps
    end
    

(* FIXME this was pasted directly from old writer; clean it up. *)
fun findStatesInitValues iter_sym basestr (class:DOF.class) = 
    let
	val classname = ClassProcess.class2orig_name class
	val exps = #exps class
	(*val state_eqs_symbols = map ExpProcess.lhs (List.filter ExpProcess.isStateEq (!exps))*)
	val init_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
	fun exp2name exp = 
	    Term.sym2curname (ExpProcess.exp2term exp)
	    handle e => DynException.checkpoint ("CParallelWriter.simengine_interface.findStatesInitValues.exp2name ["^(ExpPrinter.exp2str exp)^"]") e
			
	val instances = List.filter ExpProcess.isInstanceEq (!exps)
	val class_inst_pairs = ClassProcess.class2instnames class
    in
	(List.mapPartial (init_condition2pair iter_sym basestr) init_conditions)
	@ (StdFun.flatmap (findInstanceStatesInitValues iter_sym) class_inst_pairs)
    end
    handle e => DynException.checkpoint "CParallelWriter.simengine_interface.findStatesInitValues" e

and findInstanceStatesInitValues iter_sym (classname, instname) =
    findStatesInitValues iter_sym (Symbol.name instname) (CurrentModel.classname2class classname)

and init_condition2pair iter_sym basestr exp =
    let val term = ExpProcess.exp2term (ExpProcess.lhs exp)
	val rhs = ExpProcess.rhs exp
    in if Term.isInitialValue term iter_sym then
	   SOME ((if "" = basestr then "" else basestr ^ ".") ^ (Term.sym2name term), CWriterUtil.exp2c_str rhs)
       else NONE
    end





fun systemEmit pps [] = ()
  | systemEmit pps system = 
    let val solvers = 
	    List.mapPartial (fn {iter as (_, DOF.CONTINUOUS solver), ...} => 
				SOME solver
			      | _ => NONE) system
    in comment pps "System data structures";
       delimit (emit_system_data_structures, newline) pps system; newline pps;
       emit_subsystem_data_structures pps system; newline pps;
       newline pps;
       
       comment pps "System flow prototypes";
       delimit (emit_system_flow_prototypes, newline) pps system; newline pps;
       newline pps;

       comment pps "System solvers wrapper";
       emit_system_solvers pps solvers; newline pps;
       newline pps;

       (* TODO update and post_process wrappers *)

       comment pps "System flow definitions";
       delimit (emit_system_flow_definitions, newline) pps system; newline pps
    end

and emit_system_data_structures pps {top_class, iter, model} =
    let val (iter_name, _) = iter
	val (classes, instance, properties) = model
    in comment pps ("Data structures of iterator " ^ (Symbol.name iter_name));
       CurrentModel.withModel model (fn _ => delimit (emit_class_state_structure, newline) pps classes)
    end

and emit_subsystem_data_structures pps [] = ()
  | emit_subsystem_data_structures pps system = 
    let val storage_system = List.filter (not o ModelProcess.isUpdateIterator o #iter) system
	val class_iterator_pairs = map (fn {model=(_, {classname, ...}, _),
					    iter, ...} => (classname, iter)) 
				       storage_system

	fun declare_system_member pps (classname, iter) =
	    let val (iter_name, _) = iter
	    in prefix ("state_", string) pps (Symbol.name classname);
	       nbSpace pps 1;
	       prefix ("states_", string) pps (Symbol.name iter_name)
	    end

	fun declare_subsystem pps {top_class, iter, model} = 
	    let val (classes, _, _) = model
		val iterators = ModelProcess.independentIterators model

		fun iter_member pps (iter_name, DOF.CONTINUOUS _) =
		    prefix ("CDATAFORMAT *", suffix(string, ";")) pps (Symbol.name iter_name)
		  | iter_member pps (iter_name, DOF.DISCRETE _) = 
		    prefix ("unsigned int *", suffix(string, ";")) pps (Symbol.name iter_name)
		  | iter_member pps _ = 
		    error pps ("Dependent iterator not filtered.")

		fun iter_states class pps (iter_name, _) =
		    let val {name, ...} = class
		    in prefix ("state_", string) pps (Symbol.name name);
		       prefix (" *states_", suffix(string, ";")) pps (Symbol.name iter_name)
		    end

		fun class_subsystem pps class =
		    let val {name, ...} = class
			fun hasIterator iter = ClassProcess.hasIterator iter class
			val iterators = List.filter hasIterator iterators
		    in line string pps "typedef struct {";
		       delimit (iter_member, newline) pps iterators; newline pps;
		       delimit (iter_states class, newline) pps iterators; newline pps;
		       line string pps ("} system_" ^ (Symbol.name name) ^ ";")
		    end
	    in
		CurrentModel.withModel model (fn _ => delimit (class_subsystem, newline) pps classes)
	    end
    in comment pps ("Top-level system states");
       line string pps "typedef struct {";
       delimit (declare_system_member, newline) pps class_iterator_pairs; newline pps;
       line string pps ("} system;");
       newline pps;

       comment pps ("Per-class system states");
       (* FIXME this is generating structures per-iterator *)
       delimit (declare_subsystem, newline) pps storage_system; newline pps
    end
    

and emit_system_flow_prototypes pps {top_class, iter, model} =
    let val (iter_name, _) = iter
	val (classes, _, _) = model
	val pairs = map (fn c => (iter, c)) classes
    in comment pps ("Flow prototypes of iterator " ^ (Symbol.name iter_name));
       CurrentModel.withModel model (fn _ => delimit (emit_class_prototype, newline) pps pairs)
    end

and emit_system_solvers pps solvers =
    let val templates = [("init", nil, nil),
			 ("eval", ["unsigned int modelid"], ["modelid"]),
			 ("free", nil, nil)]
	fun solver_call (method, args) pps solver =
	    let val name = Solver.solver2name solver
	    in text pps ($("case " ^ (String.map Char.toUpper name) ^ ":"));
	       string pps "return ";
	       prefix (name, 
		       parentheses ("(", delimit (string, comma), ")"))
		      pps args;
	       string pps ";"
	    end
	fun solver_wrapper pps (method, params, args) =
	    let val params' = "solver_props *props" :: params
		val args' = "props" :: args
	    in prefix ("solver_" ^ method, 
		       parentheses ("(", delimit (string, comma), ")"))
		      pps params';
	       line string pps "{";
	       text pps ($"switch (props->solver)");
	       line string pps "{";
	       delimit (solver_call (method, args'), newline) pps solvers;
	       newline pps;
	       text pps ($"default: return 1");
	       line string pps "}";
	       line string pps "}"
	    end
    in delimit (solver_wrapper, newline) pps templates
    end

and emit_system_flow_definitions pps {top_class, iter, model} =
    let val (iter_name, _) = iter
	val (classes, _, _) = model
	val pairs = map (fn c => (iter, c)) classes
    in comment pps ("Flow definitions of iterator " ^ (Symbol.name iter_name));
       CurrentModel.withModel model (fn _ => delimit (classEmit, newline) pps pairs)
    end

fun emit_model_flows pps system =
    let val params = ["CDATAFORMAT iterval",
		      "const CDATAFORMAT *y",
		      "CDATAFORMAT *dydt",
		      "solver_props *props",
		      "const unsigned int firstIteration",
		      "const unsigned int modelid"]

	fun system_flow pps {top_class, iter, ...} =
	    let val (iter_name, _) = iter
		val flow_name = "flow_" ^ (Symbol.name top_class)
		val args = ["iterval",
			    "(const state_" ^ (Symbol.name top_class) ^ " * )y",
			    "(state_" ^ (Symbol.name top_class) ^ " * )dydt",
			    "(const system_" ^ (Symbol.name top_class) ^ " * )props->system_states",
			    "props->inputs",
			    "(CDATAFORMAT *)props->od",
			    "firstIteration",
			    "modelid"]
	    in string pps ("case ITERATOR_" ^ (Symbol.name iter_name) ^ ":"); newline pps;
	       string pps "return ";
	       prefix (flow_name,
		       parentheses ("(", delimit (string, comma), ")"))
		      pps args;
	       string pps ";"; newline pps
	    end
    in line string pps "__HOST__ __DEVICE__";
       prefix ("int model_flows", 
	       parentheses ("(", delimit (string, comma), ")"))
	      pps params;
       newline pps; 
       openHOVBox pps (Rel 2);
       line string pps "{";

       line string pps "switch (props->iterator) {";
       openHOVBox pps (Rel 2);
       delimit (system_flow, newline) pps system; newline pps;
       line string pps "default: return 1;";
       string pps "}"; closeBox pps; newline pps;

       string pps "}"; closeBox pps; newline pps
    end

fun emit_solver_init pps system =
    let val solver_system = List.filter (fn {iter, ...} => not (ModelProcess.isDependentIterator iter)) system
	val (_, {classname, ...}, _) = CurrentModel.getCurrentModel ()
    in comment pps "Initializes solver properties";
       text pps ($("solver_props *init_solver_props(CDATAFORMAT starttime, CDATAFORMAT stoptime, CDATAFORMAT *inputs, CDATAFORMAT *model_states, simengine_output *outputs) {"));

       text pps (SUB [$("system *system_states = (system * )model_states;"),
		      $("system_"^(Symbol.name classname)^" *system_ptr = (system_"^(Symbol.name classname)^" * )malloc(sizeof(system_"^(Symbol.name classname)^" ));"),
		      $("solver_props *props = (solver_props * )malloc(NUM_ITERATORS * sizeof(solver_props));"),
		      $("output_buffer *ob = (output_buffer * )malloc(sizeof(output_buffer));"),
		      $("output_data *od = (output_data *)malloc(num_models * sizeof(output_data));"),
		      $("unsigned int outputsize = sizeof(output_data) / sizeof(CDATAFORMAT);")]);

       openHOVBox pps (Rel 2);
       delimit (init_solver_props, newline) pps solver_system;
       closeBox pps; newline pps;

       text pps (SUB [$("// Initialize all time vectors"),
		      $("for(Iterator iter=0;iter<NUM_ITERATORS;iter++){"),
		      SUB[$("for(unsigned inti=0;i<NUM_MODELS;i++){"),
			  SUB[$("props[iter].time[i] = starttime;"),
			      $("props[iter].next_time[i] = starttime;")],
			  $("}")],
		      $("}")]);
       text pps ($("}"));
       newline pps;

       texts pps [$("void free_solver_props(solver_props* props){"),
		  SUB[$("for(Iterator iter=0;iter<NUM_ITERATORS;iter++){"),
		      SUB[$("free(props[iter].time);"),
			  $("free(props[iter].next_time);"),
			  $("free(props[iter].freeme);"),
			  $("free(props[iter].running);")],
		      $("}"),
		      $("free(props[0].ob);"),
		      $("if(props[0].od) free(props[0].od);"),
		      $("free(props);")],
		  $("}")]
    end

and init_solver_props pps {top_class, iter, model} =
    let val (iter_name, iter_typ) = iter
	val props_name = "props[ITERATOR_" ^ (Symbol.name iter_name) ^ "]"
	val props = [("starttime", "starttime"),
		     ("stoptime", "stoptime"),
		     ("system_states", "system_ptr"),
		     ("time", "(CDATAFORMAT * )malloc(num_models * sizeof(CDATAFORMAT))"),
		     ("next_time", "(CDATAFORMAT * )malloc(num_models * sizeof(CDATAFORMAT))"),
		     ("count", "NULL"),
		     ("model_states", "(CDATAFORMAT * )(&system_states->states_" ^ (Symbol.name iter_name)),
		     ("inputs", "inputs"),
		     ("outputs", "outputs"),
		     ("solver", "FIXME"),
		     ("iterator", "ITERATOR_" ^ (Symbol.name iter_name)),
		     ("inputsize", "num_inputs"),
		     ("statesize", "FIXME"(*Int.toString (ModelProcess.model2statesize model)*)),
		     ("next_states", "(CDATAFORMAT * )malloc(num_models * " ^ props_name ^ ".statesize * sizeof(CDATAFORMAT))"),
		     ("freeme", props_name ^ ".next_states"),
		     ("outputsize", "outputsize"),
		     ("num_models", "num_models"),
		     ("od", "od"),
		     ("ob_size", "sizeof(output_buffer)"),
		     ("ob", "ob"),
		     ("running", "(int * )malloc(num_models * sizeof(int))")]
	val params = case iter_typ
		      of DOF.CONTINUOUS solver => Solver.solver2params solver
		       | DOF.DISCRETE {sample_period} => [("timestep", Util.r2s sample_period)]
		       | _ => [("ERROR", "Unfiltered dependent iterator")]
	fun init_prop pps (prop, value) =
	    prefix (props_name ^ ".", string) pps prop
	    before prefix (" = ", suffix (string, ";")) pps value
    in delimit (init_prop, newline) pps (params @ props);
       text pps ($("memcpy(props[ITERATOR_" ^ (Symbol.name iter_name) ^ 
		   "].next_states, props[ITERATOR_" ^ (Symbol.name iter_name) ^ 
		   "].model_states, NUM_MODELS*props[ITERATOR_" ^ (Symbol.name iter_name) ^ 
		   "].statesize*sizeof(CDATAFORMAT));"));
       (case iter_typ
	 of DOF.CONTINUOUS _ =>
	    text pps ($("system_ptr->" ^ (Symbol.name iter_name) ^ " = " ^ props_name ^ ".time;"))
	  | DOF.DISCRETE _ => 
	    text pps ($("system_ptr->" ^ (Symbol.name iter_name) ^ " = " ^ props_name ^ ".time;"))
	  | _ => error pps "Unfiltered dependent iterator");
       text pps ($("system_ptr->states_" ^ (Symbol.name iter_name) ^ " = &(system_states->states_" ^ (Symbol.name iter_name) ^ ");"))
    end
    
fun enum_model_iterators pps [] = ()
  | enum_model_iterators pps iterators = 
    let fun member pps (iter_name, _) =
	    prefix ("ITERATOR_", string) pps (Symbol.name iter_name)
    in text pps ($("typedef enum {"));
       delimit (member, comma) pps iterators;
       comma pps; text pps ($("NUM_ITERATORS"));
       text pps ($("} Iterator;"))
    end
    
fun enum_model_solvers pps [] = ()
  | enum_model_solvers pps solvers = 
    let fun member pps solver =
	    string pps (String.map Char.toUpper (Solver.solver2name solver))
    in text pps ($("typedef enum {"));
       delimit (member, comma) pps solvers;
       comma pps; text pps ($("NUM_SOLVERS"));
       text pps ($("} Solver;"))
    end


fun modelEmit pps model =
    let val (_, {classname, ...}, properties) = model
	val {target, ...} = properties
	val system = ModelProcess.createIteratorForkedModels model
	val independent_system = List.filter (not o ModelProcess.isDependentIterator o #iter) system
	val independent_iterators = map #iter independent_system
	val solvers = List.mapPartial (fn (_, DOF.CONTINUOUS solver) => SOME solver
					| (_, DOF.DISCRETE _) => SOME Solver.DISCRETE
					| _ => NONE) independent_iterators
	val top_class = ModelProcess.classByName (model, classname)
    in comment pps "Copyright Simatra Modeling Technologies, LLC";
       newline pps;

       enum_model_solvers pps solvers;
       enum_model_iterators pps independent_iterators;

       archive pps "simengine/simengine_target.h";
       archive pps "simengine/simengine_api.h";
       archive pps "solvers/solvers.h";
       archive pps "simengine/defines.h";
       
       comment pps "Model interface description";
       CurrentModel.withModel model (fn _ => emit_model_interface pps system); newline pps;
       newline pps;

       archive pps "simengine/semeta_seint.h";

       (case top_class
	 of SOME class =>
	    CurrentModel.withModel model (fn _ => emit_class_output_structure pps class) before newline pps
	  | NONE => error pps "Top class not found.");
       newline pps;

       archive pps "simengine/output_buffer.h";
       archive pps "simengine/init_output_buffer.c";
       
       CurrentModel.withModel model (fn _ => emit_solver_init pps system); newline pps;
       newline pps;

       archive pps "simengine/simengine_api.c";
       archive pps "simengine/log_outputs.c";
       
       (case target
	 of Target.CPU =>
	    archive pps "simengine/exec_cpu.c"
	    before archive pps "simengine/exec_serial_cpu.c"
	  | Target.OPENMP => 
	    archive pps "simengine/exec_cpu.c"
	    before archive pps "simengine/exec_parallel_cpu.c"
	  | Target.CUDA _ => 
	    archive pps "simengine/exec_kernel_gpu.cu"
	    before archive pps "simengine/exec_parallel_gpu.cu");

       systemEmit pps system;

       CurrentModel.withModel model (fn _ => emit_model_flows pps independent_system); newline pps;

       archive pps "simengine/exec_loop.c"
    end

(* Displays the public interface for a model,
 * including its name, descriptions of its inputs, outputs, and states,
 * and any target-specific metadata. *)
and emit_model_interface pps system =
    let val (_, instance, systemproperties) = CurrentModel.getCurrentModel ()
	val {precision, target, num_models, debug, profile, ...} = systemproperties

	val top_class = CurrentModel.classname2class (#classname instance)

	val model_name = case instance
			  of {name as SOME iname, ...} => iname
			   | {classname, ...} => classname

	val target_name = case target
			   of Target.CPU => "cpu"
			    | Target.OPENMP => "openmp"
			    | Target.CUDA _ => "cuda"

	val solvers = 
	    List.mapPartial (fn {iter as (_, DOF.CONTINUOUS solver), ...} => 
				SOME solver
			      | {iter as (_, DOF.DISCRETE _), ...} => 
				SOME Solver.DISCRETE
			      | _ => NONE) system

	val iterators = 
	    map (fn {iter, ...} => iter) system

	val {inputs, ...} = top_class

	fun default_input pps (SOME exp) = expEmit pps exp
	  | default_input pps NONE = default_input pps (SOME (Exp.TERM (Exp.NAN)))

	val outputs = 
	    let fun subsystem_outputs {top_class, model, ...} =
		    case ModelProcess.classByName (model, top_class)
			of SOME {outputs, ...} => ! outputs
			 | NONE => nil
	    in Util.flatmap subsystem_outputs system
	    end

	val output_num_quantities =
	    map (fn {contents, ...} => 1 + (List.length contents)) outputs

	val (state_names, state_defaults) = 
	    let fun subsystem_states {top_class, model, iter} =
		    CurrentModel.withModel 
			model 
			(fn _ =>
			    let val (iter_sym,_) = iter
				val class = CurrentModel.classname2class top_class
			    in findStatesInitValues iter_sym "" class
			    end)
	    in ListPair.unzip (Util.flatmap subsystem_states system)
	    end
    in line (prefix ("const char model_name[] = ", suffix (c_string, ";"))) 
	    pps (Symbol.name model_name);
       line (prefix ("const char target[] = ", suffix (c_string, ";"))) 
	    pps target_name;
       line (prefix ("const char *solvers[] = ", suffix (c_array c_string, ";"))) 
	    pps (map Solver.solver2name solvers);
       line (prefix ("const char *iterator_names[] = ", suffix (c_array c_string, ";"))) 
	    pps (map (Symbol.name o #1) iterators);
       line (prefix ("const char *input_names[] = ", suffix (c_array c_string, ";"))) 
	    pps (map (Term.sym2name o #name) (!inputs));
       line (prefix ("const char *default_inputs[] = ", suffix (c_array default_input, ";")))
	    pps (map #default (!inputs));
       line (prefix ("const char *output_names[] = ", suffix (c_array c_string, ";")))
	    pps (map (Term.sym2name o #name) outputs);
       line (prefix ("const unsigned int output_num_quantities[] = ", suffix (c_array string, ";")))
	    pps (map Int.toString output_num_quantities);
       line (prefix ("const char *state_names[] = ", suffix (c_array c_string, ";"))) 
	    pps state_names;
       line (prefix ("const char *default_states[] = ", suffix (c_array string, ";"))) 
	    pps state_defaults;

       texts pps [$("#define NUM_INPUTS " ^ (Int.toString (List.length (!inputs)))),
		  $("const unsigned int num_inputs " ^ (Int.toString (List.length (!inputs)))),
		  $("#define NUM_STATES " ^ (Int.toString (List.length state_names))),
		  $("const unsigned int num_states " ^ (Int.toString (List.length state_names))),
		  $("#define NUM_OUTPUTS " ^ (Int.toString (List.length outputs))),
		  $("const unsigned int num_outputs " ^ (Int.toString (List.length outputs))),
		  $("#define HASHCODE (0x0000000000000000ULL)"),
		  $("#define VERSION (0))")]
    end

end (* structure Emit *)


end
