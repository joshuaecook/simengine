signature ASTDOFTRANS =
sig

    val ast_to_dof : Ast.stm list -> DOF.model
    exception TranslationError

end
structure AstDOFTrans : ASTDOFTRANS =
struct

exception TranslationError

open Ast
open Printer

(* perform basic error checking through these common commands *)
val error_trace = true

fun except str =
    DynException.stdException ("Unexpected exception: " ^ str, "AstDOFTrans", Logger.INTERNAL)

fun error str =     
    (if error_trace then
	 except str
     else	   
	 (Logger.log_error ($("Unsupported syntax: " ^ str));
	  DynException.setErrored()))
    handle e => DynException.checkpoint "AstDOFTrans.error" e

fun error_exp str = 
    (error str;
     Exp.TERM (Exp.TUPLE []))
    handle e => DynException.checkpoint "AstDOFTrans.error_exp" e

fun error_unit str = 
    (error str;
     ())
    handle e => DynException.checkpoint "AstDOFTrans.error_unit" e

fun failure str =
    (Logger.log_failure ($("Unexpected failure: " ^ str));
     DynException.setErrored();
     DynException.checkToProceed())


fun log_progs progs = Printer.printtexts (TextIO.stdOut, progs, 0)

(* Predicates: Create a set of predicates to find certain structures in the AST *)
fun isModel (DEFINITION (DEFMODEL _, _)) = true | isModel _ = false
(* these find all the parts of a model *)
fun isIterator (ITERATORDEF _) = true | isIterator _ = false
fun isInput (INPUTDEF _) = true | isInput _ = false
fun isOutput (OUTPUTDEF _) = true | isOutput _ = false
fun isEquation (STM (ACTION (EQUATIONS _, _))) = true | isEquation _ = false
fun isSubModel (SUBMODELINST _) = true | isSubModel _ = false
fun isState (QUANTITYDEF {basetype=STATE_QUANTITY,...}) = true | isState _ = false
fun isRandom (QUANTITYDEF {basetype=RANDOM_QUANTITY,...}) = true | isRandom _ = false
fun isSubModelAssign (STM (ACTION (ASSIGN (SEND _, _), _))) = true | isSubModelAssign _ = false


fun typepattern_to_str (TYPE sym) = "Type '"^(Symbol.name sym)^"'"
  | typepattern_to_str (COMPOUNDTYPE (sym, typepattern)) = "CompoundType '"^(Symbol.name sym)^"' ("^(typepattern_to_str typepattern)^")"
  | typepattern_to_str (ARROW (tp1, tp2)) = "Arrow ("^(typepattern_to_str tp1)^") -> ("^(typepattern_to_str tp2)^")"
  | typepattern_to_str (TUPLETYPE tplist) = "Tuple ["^(String.concatWith ", " (map typepattern_to_str tplist))^"]"
  | typepattern_to_str (UNITTYPE) = "Unit"
  | typepattern_to_str (DONTCARE) = "DontCare"

fun builtin (fcn,args) = Exp.FUN (Fun.BUILTIN fcn, map astexp_to_Exp args)
			 handle e => DynException.checkpoint "AstDOFTrans.builtin" e

(* astexp_to_Iterator - pulls out iterator references *)
and astexp_to_Iterator (POS (APPLY {func=(SYMBOL itersym), args=(TUPLE [VECTOR [offsetexp]])}, _)) =
    (* case #1 - handles iterator references that look like x[t[-1]] or x[t[0]] *)
    (case offsetexp of
	 LITERAL (CONSTREAL offset) => 
	 (* case #1a - x[t[1]] *)
	 (itersym, Iterator.RELATIVE (Real.round offset))
       | APPLY {func=(SYMBOL opsym), args=(TUPLE [LITERAL (CONSTREAL offset)])} =>
	 if opsym = (Symbol.symbol "operator_neg") then
	     (* case #1b - x[t[-1]] *)
	     (itersym, Iterator.RELATIVE (~(Real.round offset)))
	 else if opsym = (Symbol.symbol "operator_add") then
	     (* case #1c - x[t[+1]] *)
	     (itersym, Iterator.RELATIVE ((Real.round offset)))
	 else
	     (error ("unexpected operation '"^(Symbol.name opsym)^"' iterator '"^(Symbol.name itersym)^"' reference");
	      (itersym, Iterator.RELATIVE 0))
       | _ => (error ("invalid referencing of iterator '"^(Symbol.name itersym)^"'");
	       (itersym, Iterator.RELATIVE 0)))
  | astexp_to_Iterator (APPLY {func=(SYMBOL opsym), args=(TUPLE [SYMBOL itersym, LITERAL (CONSTREAL offset)])}) = 
    (* case #2 - handles iterator references that look like x[n-5] *)
    (case (Symbol.name opsym) of
	 "operator_add" => (itersym, Iterator.RELATIVE (Real.round offset))
       | "operator_subtract" => (itersym, Iterator.RELATIVE (~(Real.round offset)))
       | _ => (error ("Unexpected iterator operation <"^(Symbol.name opsym)^">");
	       (itersym, Iterator.RELATIVE 0)))
  | astexp_to_Iterator (SYMBOL itersym) = 
    (* case #3 - handles iterator references that look like y[t] or x[n] *)
    (itersym, Iterator.RELATIVE 0)
  | astexp_to_Iterator exp = (log_progs (exp_to_printer exp);
			      error "invalid iterator";
			      (Symbol.symbol "undefined", Iterator.RELATIVE 0))

and apply_to_Exp {func=(SYMBOL sym), args=(TUPLE [VECTOR [arg]])}= 
    ExpBuild.var_with_iter (sym, astexp_to_Iterator arg)
  | apply_to_Exp {func=(SYMBOL sym), args=(TUPLE args)} =
    ((case (Symbol.name sym) of
	"operator_add" => builtin (Fun.ADD, args)
      | "operator_subtract" => builtin (Fun.SUB, args)
      | "operator_multiply" => builtin (Fun.MUL, args)
      | "operator_divide" => builtin (Fun.DIVIDE, args)
      | "operator_modulus" => builtin (Fun.MODULUS, args)
      | "operator_neg" => builtin (Fun.NEG, args)
      | "operator_ge" => builtin (Fun.GE, args)
      | "operator_le" => builtin (Fun.LE, args)
      | "operator_gt" => builtin (Fun.GT, args)
      | "operator_lt" => builtin (Fun.LT, args)
      | "operator_eq" => builtin (Fun.EQ, args)
      | "operator_ne" => builtin (Fun.NEQ, args)
      | "operator_deriv" => builtin (Fun.DERIV, args)
      | "power" => builtin (Fun.POW, args)
      | "ln" => builtin (Fun.LOG, args)
      | _ => builtin (FunProps.name2op sym, args))
     (*
      | "exp" => builtin (Fun.EXP, args)
      | "log10" => builtin (Fun.LOG10, args)
      | "logn" => builtin (Fun.LOGN, args)
      | "sin" => builtin (Fun.SIN, args)
      | "cos" => builtin (Fun.COS, args)
      | "tan" => builtin (Fun.TAN, args)
      | "csc" => builtin (Fun.CSC, args)
      | "sec" => builtin (Fun.SEC, args)
      | "cot" => builtin (Fun.COT, args)
      | "asin" => builtin (Fun.ASIN, args)
      | "acos" => builtin (Fun.ACOS, args)
      | "atan" => builtin (Fun.ATAN, args)
      | "atan2" => builtin (Fun.ATAN2, args)
      | "acsc" => builtin (Fun.ACSC, args)
      | "asec" => builtin (Fun.ASEC, args)
      | "acot" => builtin (Fun.ACOT, args)
      | "sinh" => builtin (Fun.SINH, args)
      | "cosh" => builtin (Fun.COSH, args)
      | "tanh" => builtin (Fun.TANH, args)
      | "csch" => builtin (Fun.CSCH, args)
      | "sech" => builtin (Fun.SECH, args)
      | "coth" => builtin (Fun.COTH, args)
      | "asinh" => builtin (Fun.ASINH, args)
      | "acosh" => builtin (Fun.ACOSH, args)
      | "atanh" => builtin (Fun.ATANH, args)
      | "acsch" => builtin (Fun.ACSCH, args)
      | "asech" => builtin (Fun.ASECH, args)
      | "acoth" => builtin (Fun.ACOTH, args)
      | "not" => builtin (Fun.NOT, args)
      | "abs" => builtin (Fun.ABS, args)
      | "sqrt" => builtin (Fun.SQRT, args)
      | "floor" => builtin (Fun.FLOOR, args)
      | "ceil" => builtin (Fun.CEILING, args)
      | "round" => builtin (Fun.ROUND, args) 
      | _ => error_exp ("APPLY:" ^ (Symbol.name sym) ^ " with args = " ^ (Util.list2str ExpPrinter.exp2str (map astexp_to_Exp args)))*)
     handle e => DynException.checkpoint ("AstDOFTrans.apply_to_Exp ["^(Symbol.name sym)^"]") e)
  | apply_to_Exp {func=(SYMBOL sym),...} = error_exp ("APPLY ["^(Symbol.name sym)^"]")
  | apply_to_Exp {func,...} = error_exp ("APPLY")

and send_to_Exp {message, object=(SYMBOL sym)} = ExpBuild.var (Symbol.name sym ^ "." ^ (Symbol.name message))
  | send_to_Exp {message, object} = ExpBuild.var ("SEND:" ^ (Symbol.name message))

and astexp_to_Exp (LITERAL (CONSTREAL r)) = Exp.TERM (Exp.REAL r)
  | astexp_to_Exp (LITERAL (CONSTBOOL b)) = Exp.TERM (Exp.BOOL b)
  | astexp_to_Exp (LITERAL (CONSTSTR str)) = Exp.TERM (Exp.STRING str)
  | astexp_to_Exp (LITERAL (CONSTBINARY _)) = error_exp "CONSTBINARY"
  | astexp_to_Exp (STRINGEXP _) = error_exp "STRINGEXP"
  | astexp_to_Exp (SYMBOL sym) = ExpBuild.svar sym
  | astexp_to_Exp (LIBFUN _) = error_exp "LIBFUN"
  | astexp_to_Exp (LAMBDA _) = error_exp "LAMBDA"
  | astexp_to_Exp (APPLY {func, args}) = apply_to_Exp {func=func, args=args}
  | astexp_to_Exp (IFEXP {cond, ift, iff}) = builtin (Fun.IF, [cond, ift, iff])
  | astexp_to_Exp (VECTOR _) = error_exp "VECTOR"
  | astexp_to_Exp (TUPLE l) = Exp.CONTAINER (Exp.EXPLIST (map astexp_to_Exp l))
  | astexp_to_Exp (ASSERTION _) = error_exp "ASSERTION"
  | astexp_to_Exp (UNIT) = error_exp "UNIT"
  | astexp_to_Exp (UNDEFINED) = error_exp "UNDEFINED"
  | astexp_to_Exp (SEND {message, object}) = send_to_Exp {message=message, object=object}
  | astexp_to_Exp (SATISFIES _) = error_exp "SATISFIES"
  | astexp_to_Exp (ERROR _) = error_exp "ERROR"
  | astexp_to_Exp (POS (exp,_)) = astexp_to_Exp exp
  | astexp_to_Exp (TYPEEXP _) = error_exp "TYPEEXP"
  | astexp_to_Exp (AND explist) = builtin (Fun.AND, explist)
  | astexp_to_Exp (OR explist) = builtin (Fun.OR, explist)
  | astexp_to_Exp (FORGENERATOR _) = error_exp "FORGENERATOR"
  | astexp_to_Exp (FORALL _) = error_exp "FORALL"
  | astexp_to_Exp (EXISTS _) = error_exp "EXISTS"
  | astexp_to_Exp (TABLE _) = error_exp "TABLE"
  | astexp_to_Exp (LET _) = error_exp "LET"
  | astexp_to_Exp (NAMEDPATTERN _) = error_exp "NAMEDPATTERN"
  | astexp_to_Exp (WILDCARD) = error_exp "NAMEDPATTERN"
  | astexp_to_Exp (RULEMATCH _) = error_exp "RULEMATCH"
				      


and stm_to_printer (DEFINITION (def, log)) = ([$("Definition"),
					       SUB(def_to_printer def)]
					      handle e => DynException.checkpoint "AstDOFTrans.stm_to_printer [DEFINITION]" e)
  | stm_to_printer (ACTION (act, log)) = ([$("Action"),
					   SUB(act_to_printer act)]
					  handle e => DynException.checkpoint "AstDOFTrans.stm_to_printer [ACTION]" e)

and def_to_printer (DEFFUN _) = [$("Deffun")]
  | def_to_printer (DEFPROTOCOL _) = [$("Defprotocol")]
  | def_to_printer (DEFCLASS {name, classheader, methods}) = [$("Defclass"),
						     SUB[$("name: " ^ (Symbol.name name))]]
  | def_to_printer (DEFNAMESPACE {name, stms}) = [$("Defnamespace")]
  | def_to_printer (DEFINTERFACE {name, headers}) = [$("Definterface")]
  | def_to_printer (DEFGLOBAL (sym, optpat, optexp)) = [$("Defglobal: " ^ (Symbol.name sym))]
  | def_to_printer (DEFLOCAL (sym, optpat, optexp)) = [$("Deflocal: " ^ (Symbol.name sym))]
  | def_to_printer (DEFENUM {name, parent, args}) = [$("Defenum: " ^ (Symbol.name name))]
  | def_to_printer (DEFCONST (sym, optpat, exp)) = [$("Defconst: " ^ (Symbol.name sym))]
  | def_to_printer (DEFMODEL {header as {name, args, returns}, parts}) = 
    [$("Defmodel: " ^ (Symbol.name name)), 
     SUB(modelheader_to_printer header),
     $("Parts:"),
     SUB(Util.flatmap modelpart_to_printer parts)]
								      
  | def_to_printer (INSTMODEL {name, exp}) = [$("Instmodel: " ^ (Symbol.name name))]
  | def_to_printer (DEFPROPERTY {name, io={read,write}}) = [$("Defproperty: " ^ (Symbol.name name))]

and modelheader_to_printer {name, args, returns} = 
    [$("Model Header"),
     SUB([$("Args"),
	 SUB(map (fn(sym, symlist)=> $("Sym: '"^(Symbol.name sym)^"'" ^ 
				       (case symlist of 
					    SOME symlist => " ["^(Util.symlist2s symlist)^"]"
					  | NONE => ""))) args)] @
	 [SUB(case returns of 
		  SOME symlist => [$("Returns: "^(Util.symlist2s symlist))]
		| NONE => [])])]
    
and basetype_to_printer GENERIC_QUANTITY = "Generic"
  | basetype_to_printer STATE_QUANTITY = "State"
  | basetype_to_printer RANDOM_QUANTITY = "Random"
  | basetype_to_printer PARAMETER_QUANTITY = "Parameter"

and modelpart_to_printer (STM stm) = [$("Stm:"),
				      SUB(stm_to_printer stm)]
  | modelpart_to_printer (QUANTITYDEF {modifiers, basetype, name, precision, exp, settingstable, dimensions}) = 
    [$("Quantitydef ("^(basetype_to_printer basetype)^"): " ^ (Symbol.name name)),
     SUB(case exp of 
	     SOME exp => [$("exp"),
			  SUB(exp_to_printer exp)]
	   | NONE => []),
    SUB(case settingstable of
	    SOME exp => [$("settingstable"),
			 SUB(exp_to_printer exp)]
	  | NONE => [])]
  | modelpart_to_printer (OUTPUTDEF {name, quantity, dimensions, settings, condition}) = [$("Outputdef: " ^ (Symbol.name name)),
											  SUB([$("Quantity: " ^ (ExpPrinter.exp2str (astexp_to_Exp quantity)))] @ 
											       (case dimensions of
												    SOME dimlist => [$("Dimensions: " ^ (Util.symlist2s dimlist))]
												  | NONE => []) @
											       (case settings of
												    SOME e => [$("Settings: " ^ (ExpPrinter.exp2str (astexp_to_Exp e)))]
												  | NONE => []) @
											      (case condition of
												   SOME c => [$("Condition: " ^ (ExpPrinter.exp2str (astexp_to_Exp c)))]
												 | NONE => []))]
												    
  | modelpart_to_printer (INPUTDEF {name, settings}) = [$("Inputdef: " ^ (Symbol.name name)),
							SUB(case settings of
								SOME e => [$("Settings"),
									   SUB(exp_to_printer e)]
							      | NONE => [])]
  | modelpart_to_printer (ITERATORDEF {name, value, settings}) = [$("Iteratordef: " ^ (Symbol.name name)),
								  SUB(case value of 
									  SOME e => [$("Value: " ^ (ExpPrinter.exp2str (astexp_to_Exp e)))]
									| NONE => []),
								  SUB(case settings of
									  SOME e => [$("Settings"),
										     SUB(exp_to_printer e)]
									| NONE => [])]
  | modelpart_to_printer (SUBMODELDEF definition) = [$("Submodeldef:"),
						     SUB(def_to_printer definition)]
  | modelpart_to_printer (SUBMODELINST {class, name, opttable, optdimensions}) = 
    [$("Submodelinst: " ^ (Symbol.name name) ^ " of " ^ (Symbol.name class)),
     SUB(case opttable of 
	     SOME e => [$("Opttable: " ^ (ExpPrinter.exp2str (astexp_to_Exp e)))]
	   | NONE => []),
     SUB(case optdimensions of 
	     SOME dims => [$("Dimensions: " ^ (Util.symlist2s dims))]
	   | NONE => [])]

and typedname_to_printer (sym, typeopt) = 
    [$("Typedname"),
     SUB([$("Sym: " ^ (Symbol.name sym))] @
	 (case typeopt of 
	      SOME typ => [$("TypePat: " ^ typepattern_to_str typ)]
	    | NONW => []))]

and act_to_printer (EXP exp) = [$("Expression"),
				SUB(exp_to_printer exp)]
  | act_to_printer (IMPORT s) = [$("Import: " ^ s)]
  | act_to_printer (OPEN exp) = [$("Open")]
  | act_to_printer (ASSIGN (lhs,rhs)) = [$("Assign"),
					 SUB[$("lhs"),
					     SUB(exp_to_printer lhs)],
					 SUB[$("rhs"),
					     SUB(exp_to_printer rhs)]]
  | act_to_printer (COND {cond, ift, iff}) = [$("Conditional")]
  | act_to_printer (WHILE {cond, stms}) = [$("Conditional")]
  | act_to_printer (FOR {var, collection, stms}) = [$("Conditional")]
  | act_to_printer (EQUATIONS equlist) = [$("Equations:"),
					  SUB(Util.flatmap equation_to_printer equlist)]

and equation_to_printer (EQUATION (e1, e2, e3opt)) = 
    ((case e3opt of
	 SOME e3 =>
	 [$("Equation: "),
	  SUB[$("e1"),
	      SUB(exp_to_printer e1)],
	  SUB[$("e2"),
	      SUB(exp_to_printer e2)],
	  SUB[$("e3"),
	      SUB(exp_to_printer e3)]]
       | NONE =>
(*	 [$("Equ: " ^ (ExpPrinter.exp2str (ExpBuild.equals (astexp_to_Exp e1, 
							    astexp_to_Exp e2))))]*)
	 [$("Equation: "),
	  SUB[$("e1"),
	      SUB(exp_to_printer e1)],
	  SUB[$("e2"),
	      SUB(exp_to_printer e2)]])
     handle e => DynException.checkpoint "AstDOFTrans.equation_to_printer [EQUATION]" e)

  | equation_to_printer (MATHFUNCTION (e1, e2)) = [$("MathFunction: "),
						   SUB[$("e1"),
						       SUB(exp_to_printer e1)],
						   SUB[$("e2"),
						       SUB(exp_to_printer e2)]]
  | equation_to_printer (EVENT (sym, e)) = [$("Event: " ^ (Symbol.name sym)),
					    SUB[$("e"),
						SUB(exp_to_printer e)]]

and literal_to_string (CONSTSTR s) = s
  | literal_to_string (CONSTREAL r) =  Util.r2s r
  | literal_to_string (CONSTBOOL b) = Util.b2s b
  | literal_to_string (CONSTBINARY b) = "Binary"

and exp_to_printer (LITERAL lit) = [$("Literal: " ^ (literal_to_string lit))]
  | exp_to_printer (STRINGEXP explist) = [$("Stringexp"),
					  SUB(map (SUB o exp_to_printer) explist)]
  | exp_to_printer (SYMBOL sym) = [$("Symbol: " ^ (Symbol.name sym))]
  | exp_to_printer (LIBFUN (sym, exp)) = [$("Libfun: " ^ (Symbol.name sym)),
					  SUB(exp_to_printer exp)]
  | exp_to_printer (LAMBDA {args, body}) = [$("Lambda"),
					    SUB[$("args"),
						SUB(map ($ o Symbol.name) args)],
					    SUB[$("body"),
						SUB(exp_to_printer body)]]
  | exp_to_printer (APPLY {func, args}) = [$("Apply"),
					    SUB[$("func"),
						SUB(exp_to_printer func)],
					    SUB[$("args"),
						SUB(exp_to_printer args)]]
  | exp_to_printer (IFEXP {cond, ift, iff}) = [$("Ifexp"),
					       SUB[$("cond"),
						   SUB(exp_to_printer cond)],
					       SUB[$("ift"),
						   SUB(exp_to_printer ift)],
					       SUB[$("iff"),
						   SUB(exp_to_printer iff)]]
  | exp_to_printer (VECTOR explist) = [$("Vector"),
				       SUB(map (SUB o exp_to_printer) explist)]
  | exp_to_printer (TUPLE explist) = [$("Tuple: "),
				      SUB(Util.flatmap exp_to_printer explist)]
  | exp_to_printer (ASSERTION exp) = [$("Assertion")]
  | exp_to_printer (UNIT) = [$("Unit")]
  | exp_to_printer (UNDEFINED) = [$("Undefined")]
  | exp_to_printer (SEND {message, object as (SYMBOL _)}) = 
    [$("Send: " ^ 
       (ExpPrinter.exp2str (send_to_Exp {message=message, object=object})))]
  | exp_to_printer (SEND {message, object}) = 
    [$("Send"),
     SUB[$("Message: " ^ (Symbol.name message)),
	 $("object"),
	 SUB(exp_to_printer object)]]
  | exp_to_printer (SATISFIES {class, interface}) = [$("Satisfies")]
  | exp_to_printer (ERROR exp) = [$("Error")]
  | exp_to_printer (POS (exp, pos)) = [$("With Pos"),
				       SUB(exp_to_printer exp)]
  | exp_to_printer (TYPEEXP pat) = [$("Typeexp")]
  | exp_to_printer (AND explist) = [$("And"),
				    SUB(map (SUB o exp_to_printer) explist)] 
  | exp_to_printer (OR explist) = [$("Or"),
				   SUB(map (SUB o exp_to_printer) explist)]
  | exp_to_printer (FORGENERATOR _) = [$("Forgenerator")]
  | exp_to_printer (FORALL {var, collection, test}) = [$("Forall")]
  | exp_to_printer (EXISTS {var, collection, test}) = [$("Exists")]
  | exp_to_printer (TABLE symexplist) = [$("Table"),
					 SUB(Util.flatmap (fn(sym, exp)=> [$(Symbol.name sym),
									   SUB(exp_to_printer exp)]) symexplist)]
  | exp_to_printer (LET (sym, exp1, exp2)) = [$("Let")]
  | exp_to_printer (NAMEDPATTERN (sym, exp)) = [$("NamedPattern")]
  | exp_to_printer (WILDCARD) = [$("Wildcard")]
  | exp_to_printer (RULEMATCH {find, conds, replace}) = [$("Rulematch")]

(* modeldef_to_class - convert an AST model definition into a class *)
local
    fun lookupTable (TABLE symexplist, sym) = 
	(case List.find (fn(sym',exp)=> sym=sym') symexplist of
	     SOME (_, exp) => SOME exp
	   | NONE => NONE)
      | lookupTable _ = NONE

    fun translate_input (INPUTDEF {name, settings}) = 
	let
	    val default = case settings of 
			      SOME settings => 
			      (case lookupTable (settings, Symbol.symbol "default") of
				   SOME exp => SOME (astexp_to_Exp exp)
				 | NONE => NONE)
			    | NONE => NONE
	    val behaviour = case settings of
				SOME settings => 
				(case lookupTable (settings, Symbol.symbol "cycle_when_exhausted") of
				     SOME _ => DOF.Input.CYCLE
				   | NONE => (case lookupTable (settings, Symbol.symbol "halt_when_exhausted") of
						  SOME _ => DOF.Input.HALT
						| NONE => ((* don't need to check for hold since we don't use it anyway *)
							   (*case lookupTable (settings, Symbol.symbol "hold_when_exhausted") of
							       SOME _ => DOF.Input.HOLD
							     | NONE =>*) DOF.Input.HOLD)))
			      | NONE => DOF.Input.HOLD
						  
	    val iterator = case settings of
			       SOME settings => 
			       (case lookupTable (settings, Symbol.symbol "iter") of
				    SOME (SYMBOL sym) => SOME (sym, Iterator.RELATIVE 0)
				  | SOME _ => (error ("unexpected non symbol parameter for iter property for input " ^ (Symbol.name name));
					       NONE)
				  | NONE => NONE)
			     | NONE => NONE
	in
	    DOF.Input.make {name=case iterator of
				     SOME iter => ExpProcess.exp2term (ExpBuild.var_with_iter (name, iter))
				   | NONE => ExpProcess.exp2term (ExpBuild.svar name),
			    default=default, 
			    behaviour=behaviour}
	end
      | translate_input _ = except "non-input"
    fun translate_output inputNames (OUTPUTDEF {name, quantity, dimensions, settings, condition}) = 
	(DOF.Output.make {name=ExpProcess.exp2term (ExpBuild.svar name),
			  inputs=ref inputNames,
			  contents=case quantity of 
				       TUPLE t => map astexp_to_Exp t
				     | UNIT => []
				     | q => [astexp_to_Exp q],
			  condition=case condition of
					SOME c => astexp_to_Exp c
				      | NONE => ExpBuild.bool true})
      | translate_output _ _ = except "non-output"
    fun translate_iterator (ITERATORDEF {name, value, settings=(SOME (TABLE settings))}) =
	let
	    (* check for the continuous flag - if it is not continuous, assume that it is 
	     * discrete.  Only a good assumption when we are parsing machine generated code *)
	    val isContinuous = case List.find (fn(sym, _)=> sym = (Symbol.symbol "continuous")) settings of
				   SOME (_, LITERAL (CONSTBOOL true)) => true
				 | _ => false

	    fun translate_table ((sym, exp)::rest) = (sym, astexp_to_Exp exp)::(translate_table rest)
	      | translate_table [] = []
	in
	    if isContinuous then
		let
		    val solver=case List.find (fn(sym, exp)=>sym = (Symbol.symbol "solver")) settings of
				   SOME (_, POS (APPLY {func=(SYMBOL solver_sym), args=(TUPLE [TABLE solver_settings])}, _)) => 
				   Solver.name2solver (solver_sym, translate_table solver_settings)
				 | SOME (_, APPLY {func=(SYMBOL solver_sym), args=(TUPLE [TABLE solver_settings])}) => 
				   Solver.name2solver (solver_sym, translate_table solver_settings)
				 | SOME (_, APPLY {func=(SYMBOL solver_sym), args}) => 
				   (error ("unexpected args with table in solver");
				    Solver.default)
				 | SOME (_, APPLY {func, args}) => 
				   (error ("unexpected apply with strange syntax in solver");
				    Solver.default)
				 | SOME (_, exp) => 
				   (error ("unexpected non apply in solver");
				    log_progs (exp_to_printer exp);
				    Solver.default)
				 | _ => (error ("no valid solver specified");
					 Solver.default)
		in
		    (name, DOF.CONTINUOUS solver)
		end
	    else
		let
		    val settings' = translate_table settings
		in
		case List.find (fn(sym, exp)=>sym = (Symbol.symbol "sample_period")) settings' of
		    SOME (_, Exp.TERM (Exp.REAL r)) => (name, DOF.DISCRETE {sample_period=r})
		  | _ => (case List.find (fn(sym, exp)=>sym = (Symbol.symbol "sample_frequency")) settings' of
			      SOME (_, Exp.TERM (Exp.REAL r)) => (name, DOF.DISCRETE {sample_period=1.0/r})
			    | _ => (Logger.log_warning ($("No sample period or frequency specified for discrete iterator, default to one"));
				    (name, DOF.DISCRETE {sample_period=1.0})))
		end
	end
      | translate_iterator _ = except "non-iterator"
    fun expand_equations (STM (ACTION (EQUATIONS equs,_))) = equs
      | expand_equations _ = []
    fun translate_equation statetable (EQUATION (lhs, rhs, optcond)) = 
	let
	    val lhs = case lhs of 
			  APPLY {func=(SYMBOL sym), args=(TUPLE arglist)} => 
			  if sym = (Symbol.symbol "operator_deriv") then
			      let
				  val (id) = case arglist of
						 [LITERAL (CONSTREAL order), SYMBOL sym] => 
						 if Real.== (order, 1.0) then
						     case SymbolTable.look (statetable, sym) of
							 SOME iter_id => ExpBuild.diff_state_var (sym, iter_id)
						       | NONE => (error ("State '"^(Symbol.name sym)^"' has not been properly defined");
								  ExpBuild.svar sym)
						 else
						     (error ("Derivative on variable '"^(Symbol.name sym)^"' on left-hand-side must be of order one");
						      ExpBuild.svar sym)
					       | _ => (error "Derivatives can only be defined against a variable, not a general expression";
						       ExpBuild.var "unknown")
			      in
				  id
			      end
			  else
			      (error ("unsupported function '"^(Symbol.name sym)^"'");
			       ExpBuild.var "unknown")
			| _ => astexp_to_Exp lhs
	    val rhs = astexp_to_Exp rhs
	    val rhs = case optcond of
			  SOME cond => ExpBuild.cond (astexp_to_Exp cond, rhs, lhs)
			| NONE => rhs
	in
	    ExpBuild.equals (lhs, rhs)
	end
      | translate_equation _ _ = except "non-equation"
    fun translate_state ((QUANTITYDEF {modifiers, basetype=STATE_QUANTITY, name, precision, exp=SOME (init), settingstable=(SOME (TABLE settings)), dimensions}), (statelist, statetable)) = 
	let
	    val iter_id = case List.find (fn(sym, exp)=> sym = (Symbol.symbol "iter")) settings of
			      SOME (_, SYMBOL iter_id) => iter_id
			    | _ => (error ("no valid iterator found for state '"^(Symbol.name name)^"'");
				    Symbol.symbol "unknown_iterator")
	    val init = astexp_to_Exp init
	    val state_init_equ = ExpBuild.equals (ExpBuild.state_init_var (name, iter_id), init)
	    (* construct this state<->iterator table so that we know which iterators match up to the states when 
	     * we see them again as differential equations *)
	    val statetable = SymbolTable.enter (statetable, name, iter_id)
	in
	    (state_init_equ::statelist, statetable) 
	end
      | translate_state _ = except "non-state"

    fun translate_random (QUANTITYDEF {modifiers, basetype=RANDOM_QUANTITY, name, precision, exp=NONE, settingstable=(SOME (TABLE settings)), dimensions}) = 
	let
	    fun find_setting setting =
		let val sym = Symbol.symbol setting
		in case (List.find (fn(sym', _)=>sym=sym') settings) of
		       SOME (_, exp) => SOME exp
		     | NONE => NONE
		end

	    (* start searching for settings *)
	    val iter = case find_setting "iter" of
			   SOME (SYMBOL iter_sym) => iter_sym
			 | _ => (error ("No iterator defined with random number generator definition");
				 Symbol.symbol "unknown_iterator")
				
	    val isUniform = isSome (find_setting "uniform")
	    fun get setting = case find_setting setting of
				  SOME (exp) => astexp_to_Exp exp
				| _ => (error("Random number option '' does not exist");
					Exp.TERM (Exp.NAN))
	    val r = if isUniform then
			ExpBuild.uniform_rand()
		    else
			ExpBuild.normal_rand()

	    val rhs = if isUniform then
			  let 
			      val high = get "high"
			      val low = get "low"
			  in
			      ExpBuild.plus [ExpBuild.times [ExpBuild.sub (high, low), r], low]
			  end
		      else
			  let
			      val mean = get "mean"
			      val stddev = get "stddev"
			  in
			      ExpBuild.plus [ExpBuild.times [stddev, r], mean]
			  end

	    val in_process_iterator_sym = Iterator.inProcessOf (Symbol.name iter)
	    val equations = [(* first the initialization equation *)
			     ExpBuild.equals (ExpBuild.state_init_var (name, in_process_iterator_sym), rhs),
			    (* second the algebraic equation to define the next iteration's random number *)
			     ExpBuild.equals (ExpBuild.state_next_var (name, in_process_iterator_sym), rhs)]
	in
	    equations
	end
      | translate_random _ = except "non-random"

    fun collect_submodels (modeltable, submodels, submodelassigns) = 
	let
	    fun submodel_to_equation (SUBMODELINST {class, name, opttable, optdimensions}) = 
		(let
		     val {args, returns, inputs, parts} = case (SymbolTable.look (modeltable, class)) of
							      SOME entry => entry
							    | NONE => (error ("Class with name '"^(Symbol.name class)^"' has not been defined");
								       raise TranslationError)

		     val instprops = InstProps.setRealClassName InstProps.emptyinstprops class
		     fun submodel_to_funtype () =
			 Fun.INST {classname=class, 
				   instname=name, 
				   props=instprops}

		     (* TODO - add this support, despite the matlab code generator not currently supporting this *)
		     val _ = case opttable of
				 SOME _ => error ("ignoring all arguments attached to submodel '"^(Symbol.name name)^"':'"^(Symbol.name class)^"' instantiation")
			       | NONE => ()

		     val assignmentlist = List.mapPartial 
					      (fn(sma)=>
						 case sma of
						     STM (ACTION (ASSIGN (SEND {message=inputname, object as (SYMBOL instname)}, rhs),_)) => 
						     if instname = name then
							 SOME (inputname, astexp_to_Exp rhs)
						     else
							 NONE (* for another instance *)
						   | _ => except "non-submodelassign")
					      submodelassigns
		     (* val instargs = map *)
		     (* 			(fn(sym, optdefault)=>  *)
		     (* 			   case (List.find (fn(sym', rhs)=>sym=sym') assignmentlist, optdefault) of *)
		     (* 			       (SOME (_, rhs), _) => rhs *)
		     (* 			     | (NONE, SOME default) => default *)
		     (* 			     | (NONE, NONE) =>  *)
		     (* 			       (error("No value (default or explicit) was set for input '"^(Symbol.name sym)^"'"); *)
		     (* 				Exp.TERM (Exp.NAN))) *)
		     (* 			args *)
		     val instanceInputs =
			 Exp.CONTAINER
			     (Exp.ASSOC
				  (foldl (fn ((sym, optdefault), tab) => 
					     let 
						 val value = 
						     case (List.find (fn(sym', rhs)=>sym=sym') assignmentlist, optdefault) of
							 (SOME (_, rhs), _) => rhs
						       | (NONE, SOME default) => default
						       | (NONE, NONE) => 
							 (error("No value (default or explicit) was set for input '"^(Symbol.name sym)^"'");
							  Exp.TERM (Exp.NAN))
					     in
						 SymbolTable.enter (tab, sym, value)
					     end)
					 SymbolTable.empty args))

		     val instreturns = map
					   (fn(sym)=> ExpProcess.exp2term (ExpBuild.var ((Symbol.name name) ^ "." ^ (Symbol.name sym))))
					   returns

		     val exp = ExpBuild.equals (ExpBuild.tuple nil, Exp.FUN (submodel_to_funtype(), [instanceInputs]))

		     val output_exps = 
			 map (fn outname =>
				 let
				     val lhs = ExpBuild.tuple [ExpProcess.exp2term (ExpBuild.var ((Symbol.name name) ^ "." ^ (Symbol.name outname)))]
				     val rhs = Exp.FUN (Fun.OUTPUT {classname=class,
								    instname=name,
								    outname=outname,
								    props=InstProps.emptyinstprops},
							[instanceInputs])
				 in
				     ExpBuild.equals (lhs, rhs)
				 end
				 ) returns
		 in
		     exp :: output_exps
		 end
		 handle e => DynException.checkpoint "AstDOFTrans.collect_submodels.submodel_to_equation" e)
	      | submodel_to_equation _ = except "non-submodel"
		handle e => DynException.checkpoint "AstDOFTrans.collect_submodels.submodel_to_equation" e
	in
	    Util.flatmap submodel_to_equation submodels
	end
	handle e => DynException.checkpoint "AstDOFTrans.collect_submodels" e
    fun create_classproperties name = 
	{sourcepos=PosLog.NOPOS, 
	 preshardname=name, 
	 classform=DOF.INSTANTIATION {readstates=[], writestates=[]}  (* FIXME!!! *)
	}

fun remove_duplicate_iterators iterators = 
    let
	val itertable = SymbolTable.empty
	val itertable = foldl 
			    (fn(iter as (iter_sym, _),itertable')=>
			       case SymbolTable.look (itertable', iter_sym) of
				   SOME _ => itertable' (* already exists - should do an equality check here *)
				 | NONE => SymbolTable.enter (itertable', iter_sym, iter))
			    itertable
			    iterators
    in
	SymbolTable.listItems itertable
    end
    handle e => DynException.checkpoint "AstDOFTrans.remove_duplicate_iterators" e

fun build_system_properties iterators =
    let
	(* grab settings from the registry *)
	val precision = DynamoOptions.getStringSetting "precision"
	val target = DynamoOptions.getStringSetting "target"
	val parallel_models = DynamoOptions.getIntegerSetting "parallel_models"
	val debug = DynamoOptions.isFlagSet "debug"
	val profile = DynamoOptions.isFlagSet "profile"
			
	(* update to licensing default if set to default *)
	val target = if StdFun.toLower target = "default" then
			 Features.defaultTarget()
		     else
			 target			 
		      
	(* only support openmp right now *)
	val target = if StdFun.toLower target = "parallelcpu" then
			 "openmp"
		     else
			 target

	(* only support cuda right now *)
	val target = if StdFun.toLower target = "gpu" then
			 "cuda"
		     else
			 target
    in
	{iterators=iterators,
	 precision= case (StdFun.toLower precision)
		     of "single" => DOF.SINGLE
		      | "float" => DOF.SINGLE
		      | "double" => DOF.DOUBLE
		      | _ => (error ("unsupported precision '"^precision^"'");
			      DOF.DOUBLE),
	 target=case (StdFun.toLower target)
		  of "cpu" => Target.CPU
		   | "openmp" => Target.OPENMP
		   | "cuda" => Target.CUDA (*{compute=deviceCapability, 
					      multiprocessors=numMPs, 
					      globalMemory=globalMemory} *)
		   | _ => (error ("unsupported target '"^target^"'");
			   Target.CPU),
	 parallel_models=parallel_models,
	 debug=debug,
	 profile=profile}
    end
    handle e => DynException.checkpoint "AstDOFTrans.build_system_properties" e


in
fun modeldef_to_class modeltable name =
    let
	(* grab the pieces of the model out of the modeltable *)
	val {args, returns, inputs, parts} = case SymbolTable.look (modeltable, name) of
					 SOME r => r
				       | NONE => except "Can't access model by name"

	(* for the parts, we need to sort them and make sure there's nothing that
	 * we're not expecting, for example, some type of syntax that we can't
	 * support *)
	val (outputs, rest) = List.partition isOutput parts
	val (iterators, rest) = List.partition isIterator rest
	val (states, rest) = List.partition isState rest
	val (randoms, rest) = List.partition isRandom rest
	val (equations, rest) = List.partition isEquation rest
	val (submodels, rest) = List.partition isSubModel rest
	val (submodelassigns, rest) = List.partition isSubModelAssign rest

	(* rest should now be empty*)
	val _ = if List.length rest > 0 then
		    (error_unit ("Additional parts not accounted for in " ^ (Symbol.name name));
		     log_progs (Util.flatmap modelpart_to_printer parts))
		else
		    ()

	(* we're going to need a state - iterator mapping so that we can use the 
	 * iterator when we define the differential equations *)
	val statetable = SymbolTable.empty
	val (state_equations, statetable) = foldl translate_state ([], statetable) states
	val random_equations = Util.flatmap translate_random randoms

	(* go through each of the elements and process them one by one *)
	val class = {name=name,
		     properties=create_classproperties name,
		     inputs=ref inputs, (* we translated this earlier when we analyzed the model headers *)
		     outputs=ref (map (translate_output (map DOF.Input.name inputs)) outputs),
		     exps=ref (state_equations @
			       random_equations @
			       (collect_submodels (modeltable, submodels, submodelassigns)) @
			       (map (translate_equation statetable) (Util.flatmap expand_equations equations)))}

	(*val _ = DOFPrinter.printClass class*)
    in
	(class, map translate_iterator iterators)
    end
    handle e => DynException.checkpoint ("AstDOFTrans.modeldef_to_class ["^(Symbol.name name)^"]") e

fun create_model_table astlist = 
    let
	val (models, other) = List.partition isModel astlist
	val _ = if List.length other > 0 then
		    let
			val progs = Util.flatmap stm_to_printer other
			val _ = Util.log "PRINTING NON RECOGNIZED SYNTAX"
			val _ = log_progs progs
		    in
			error_unit "non-model syntax"
		    end
		else
		    ()

	val _ = DynException.checkToProceed()


	(* create a symbol table for the models - there could be many models *)
	val modeltable = SymbolTable.empty
	fun reduceArgs (arg, NONE) = arg
	  | reduceArgs (arg, SOME _) = (error ("Type on arg '"^(Symbol.name arg)^"'"); arg)
	fun reduceReturns (SOME symlist) = symlist
	  | reduceReturns (NONE) = []

	(* modeltable includes all the models that were loaded in the one dsl file.  By giving a name, you can instantly look up the interface, where the inputs are a symbol list in args, and the outputs are a symbol list in returns.  The parts are the final field in the record, and they are of type (Ast.modelpart list).*)
	val (modeltable, top_level_model) = 
	    foldl (fn(ast, (modeltable', _))=>
		     case ast of
			 DEFINITION (DEFMODEL {header as {name, args, returns},
					       parts}, _) =>
			 let
			     (* we need to pull out the inputs here, since we'll need to include the input default values as part of the argument list *)
			     val (input_stms, parts') = List.partition isInput parts
			     val inputs = map translate_input input_stms

			     (* add the default if included to the args *)
			     val args : (Symbol.symbol * Exp.exp option) list
			       = map 
				     (fn(sym)=> case List.find (fn(inp)=>(ExpProcess.exp2symbol o ExpProcess.term2exp o DOF.Input.name) inp = sym) inputs of
						    SOME inp => (sym, DOF.Input.default inp)
						  | NONE => (sym, NONE))
				     (map reduceArgs args)

			     val entry = {args=args, returns=(reduceReturns returns), inputs=inputs, parts=parts'}
			 in 
			     (SymbolTable.enter (modeltable', name, entry), 
			      name) (* return the name everytime, so on the last model loaded, we'll have the name of the top-level model *)
			 end
		       | _ => except "non-model"
		  ) 
		  (modeltable, Symbol.symbol "undefined")
		  models
    in
	(modeltable, top_level_model)
    end
    handle e => DynException.checkpoint "AstDOFTrans.create_model_table" e

fun ast_to_dof astlist = 
    let
	val _ = if DynamoOptions.isFlagSet "logast" then
		    let
			val progs = Util.flatmap stm_to_printer astlist
		    in
			(Util.log "PRINTING AST";
			 log_progs progs)
		    end
		else
		    ()
	val _ = DynException.checkToProceed()

	(* first, go through the AST and search for the models.  When we find a model, we're going to put together a signature list for each of
	 * the models.  Afterwards, we'll look at the parts of each model and begin translating them into classes.  *)
	val (modeltable, top_level_model) = create_model_table astlist

	val modelnames = SymbolTable.listKeys modeltable
	(* now we can convert each of the model definitions directly into classes.  We need to pass in the model table so that we can properly interpret submodels *)
	val (classes, iterators) = 
	    ListPair.unzip
		(map (modeldef_to_class modeltable) modelnames)

	(* remove duplicate iterators *)
	val reduced_iterators = remove_duplicate_iterators (Util.flatten iterators)
	(* add in process variants *)
	val all_iterators = (Symbol.symbol "always", DOF.IMMEDIATE)::
			    (Util.flatmap 
				 (fn(orig_iter as (iter_sym, _)) => [orig_iter, 
								     (Iterator.inProcessOf (Symbol.name iter_sym), DOF.ALGEBRAIC (DOF.INPROCESS, iter_sym)),
								     (Iterator.updateOf (Symbol.name iter_sym), DOF.UPDATE iter_sym)])
				 reduced_iterators)
	    
	(* put together the system properties *)
	val systemproperties = build_system_properties all_iterators
	val instance = {name=NONE, classname=top_level_model}

	val dof = (classes, instance, systemproperties)
    in
	dof
    end
    (*handle TranslationError => raise TranslationError*)
    handle e => DynException.checkpoint "AstDOFTrans.ast_to_dof" e
end

end
