signature RULES = sig

type rule = Rewrite.rewrite

val addRules : string * rule list -> unit
val getRules : string -> rule list

end

structure Rules : RULES=
struct

type rule = Rewrite.rewrite

val expansionRules : Rewrite.rewrite list = 
    [
(* Distribute Rules *)
(* -(a+b) -> (-a)+(-b) *)
     {find=ExpBuild.neg (ExpBuild.plus [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.plus[ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.neg (ExpBuild.var "#x")),
							ExpBuild.var "a")])},
           
(* a*(b+c) -> a*b+a*c *) 
     {find=ExpBuild.times [Match.any "d1", ExpBuild.plus [Match.any "a"], Match.any "d2"],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.plus[ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.times [ExpBuild.var "d1", ExpBuild.var "d2", ExpBuild.var "#x"]),
							ExpBuild.var "a")])},

(* De Morgan's Expansion Laws *)
(* ~(a & b) -> ~a | ~b *) 
     {find=ExpBuild.not (ExpBuild.land [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.lor [ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.not (ExpBuild.var "#x")),
						       ExpBuild.var "a")])},
     
(* ~(a | b) -> ~a & ~b *) 
     {find=ExpBuild.not (ExpBuild.lor [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.land [ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.not (ExpBuild.var "#x")),
						       ExpBuild.var "a")])}
    ]

val simplificationRules =
    [
     (* add() -> sequence() *)
     {find=ExpBuild.plus [],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.sequence [])},
     (* mul() -> sequence() *)
     {find=ExpBuild.times [],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.sequence [])}

    ]								 

val restorationRules =
    [(* COPIED *)
     (* add() -> sequence() *)
     {find=ExpBuild.plus [],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.sequence [])},
     (* mul() -> sequence() *)
     {find=ExpBuild.times [],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.sequence [])}

    ]								 

fun op2name (oper) =
    FunProps.op2name (Fun.BUILTIN oper)

val e2s = ExpPrinter.exp2str

fun lookup assigned_patterns name : Exp.exp =
    case SymbolTable.look(assigned_patterns, Symbol.symbol name) of
	SOME (exp) => exp
      | NONE => DynException.stdException ("Encountered unknown pattern " ^ name, 
					   "Rules.funop2rule.lookup", 
					   Logger.INTERNAL)


fun funop2rule oper =
    let
	fun exp2term (Exp.TERM t) = t
	  | exp2term _ = Exp.NAN

	fun execBinaryOp execs (b, a) =
	    let
		fun condapply NONE _ = 
		    Exp.FUN (Fun.BUILTIN oper, [a,b])
		  | condapply (SOME exec) (a,b) =
		    exec (a,b)

		val exp = 
		    case (exp2term a, exp2term b) of
			(Exp.BOOL a, Exp.BOOL b) => condapply (#bool execs) (a,b)
		      | (Exp.INT a, Exp.INT b) => condapply (#int execs) (a,b)
		      | (Exp.REAL a, Exp.REAL b) => condapply (#real execs) (a,b)
		      | (Exp.COMPLEX a, Exp.COMPLEX b) => condapply (#complex execs) (a,b)
		      (*| (Exp.LIST (a, _), Exp.LIST (b, _)) => condapply (#collection execs) (a,b)*)
		      | (Exp.RATIONAL a, Exp.RATIONAL b) => condapply (#rational execs) (a,b)
		      | _ => DynException.stdException ("Unexpected expression pair to binary operation: (" ^ (e2s a) ^ ", " ^ (e2s b) ^ ")"  , "Rules.execBinaryOp", Logger.INTERNAL)
	    in
		exp
	    end
	
	fun execUnaryOp execs (a: Exp.exp) : Exp.exp =
	    let
		fun condapply NONE _ = 
		    Exp.FUN (Fun.BUILTIN oper, [a])
		  | condapply (SOME exec) a =
		    exec a

		val exp = 
		    case exp2term a of
			Exp.BOOL a => condapply (#bool execs) a
		      | Exp.INT a => condapply (#int execs) a
		      | Exp.REAL a => condapply (#real execs) a
		      | Exp.COMPLEX a => condapply (#complex execs) a
		    (*  | Exp.LIST (a,_) => condapply (#collection execs) a*)
		      | Exp.RATIONAL a => condapply (#rational execs) a
		      | _ => DynException.stdException ("Unexpected expression pair to binary operation", "Rules.execBinaryOp", Logger.INTERNAL)
	    in
		exp
	    end

	fun runlist nil : Exp.exp = 
	    (case #operands (FunProps.op2props oper) of
		 FunProps.VARIABLE exp => Exp.TERM(exp)
	       | _ => DynException.stdException ("Expected variable operands, received fixed", "Rules.funop2rule.runlist", Logger.INTERNAL))
	  | runlist (list : Exp.exp list as (first: Exp.exp)::rest) =
	    case #eval (FunProps.op2props oper) of
		FunProps.UNARY execs => 
		if null rest then
		    execUnaryOp execs first
		else
		    DynException.stdException ("Encountered Unary operation " ^ (op2name oper) ^ " with multiple args, received fixed", "Rules.funop2rule.runlist", Logger.INTERNAL)
	      | FunProps.BINARY execs => foldl (execBinaryOp execs) first rest
					 
	      | FunProps.IF_FUN execs => Exp.FUN (Fun.BUILTIN oper, list)
	      | FunProps.INSTANCE => Exp.FUN (Fun.BUILTIN oper, list)
					
	    
	    
	fun run (a, b) =
	    let
		val (a', b') = 
		    Term.makeCommensurable (exp2term a, exp2term b)
	    in
		runlist [Exp.TERM a', Exp.TERM b']
	    end

	val (argpattern, evalExp) = 
	case #operands (FunProps.op2props oper) of
	    FunProps.FIXED (x) => (List.tabulate (x, (fn(i)=>Match.anyconst ("#x" ^ (Int.toString i)))),
				   fn(exp, assigned_patterns) =>
				     if x = 1 then
					 runlist[lookup assigned_patterns "#x0"]
				     else if x = 2 then
					 run(lookup assigned_patterns "#x0",
					     lookup assigned_patterns "#x1")
				     else
					 runlist (List.tabulate (x, 
								 (fn(i)=> lookup assigned_patterns 
										 ("#x" ^ (Int.toString i))))))
				  
	  | FunProps.VARIABLE _ => 
	    if (#commutative (FunProps.op2props oper)) then
		([Match.any "#d1",
		  Match.anyconst "#c1",
		  Match.any "#d2",
		  Match.anyconst "#c2",
		  Match.any "#d3"],
		 fn(exp, assigned_patterns) =>
		   Exp.FUN (Fun.BUILTIN oper, [lookup assigned_patterns ("#d1"), 
					       lookup assigned_patterns ("#d2"), 
					       lookup assigned_patterns ("#d3"), 
					       run(lookup assigned_patterns ("#c1"), lookup assigned_patterns ("#c2"))])
		)
	    else
		if (#associative (FunProps.op2props oper)) then
		    ([Match.any "#d1",
		      Match.anyconst "#c1",
		      Match.anyconst "#c2",
		      Match.any "#d2"],
		  fn(exp, assigned_patterns) =>
		    Exp.FUN (Fun.BUILTIN oper, [lookup assigned_patterns ("#d1"), 
						run(lookup assigned_patterns ("#c1"), lookup assigned_patterns ("#c2")),
						lookup assigned_patterns ("#d2")])
		    )
		else
		    ([Match.anyconst "#c1",
		      Match.anyconst "#c2",
		      Match.any "#d1"],
		  fn(exp, assigned_patterns) =>
		    Exp.FUN (Fun.BUILTIN oper, [run(lookup assigned_patterns ("#c1"), lookup assigned_patterns ("#c2")),
						lookup assigned_patterns ("#d1")])
		    )

    in
	    {find = Exp.FUN (Fun.BUILTIN oper, argpattern),
	     test=NONE,
	     replace=Rewrite.MATCHEDACTION (("constant folding " ^ (#name (FunProps.op2props oper))),
					    evalExp)}
    end

val ifRules = [{find = Exp.FUN (Fun.BUILTIN Fun.IF, [Match.anyconst "#b", Match.one "#t", Match.one "#f"]),
		test=NONE,
		replace=Rewrite.MATCHEDACTION ("constant folding if", fn(exp, assigned_patterns) => case lookup assigned_patterns ("#b") of
													Exp.TERM (Exp.BOOL true) => lookup assigned_patterns ("#t")
												      | Exp.TERM (Exp.BOOL false) => lookup assigned_patterns ("#f")
												      | _ => exp)}]

val evalRules = (map funop2rule Fun.op_list)
		@ ifRules


val ruleTable = ref SymbolTable.empty

val _ = ruleTable := SymbolTable.enter (!ruleTable,
					Symbol.symbol "expansion",
					expansionRules)


val _ = ruleTable := SymbolTable.enter (!ruleTable,
					Symbol.symbol "simplification",
					simplificationRules)

val _ = ruleTable := SymbolTable.enter (!ruleTable,
					Symbol.symbol "restoration",
					restorationRules)

val _ = ruleTable := SymbolTable.enter (!ruleTable,
					Symbol.symbol "eval",
					evalRules)

val _ = ruleTable := SymbolTable.enter (!ruleTable,
					Symbol.symbol "simplification",
					evalRules)


fun addRules (categoryname, rules) =
    ruleTable := SymbolTable.enter (!ruleTable, 
				    Symbol.symbol categoryname, 
				    (case SymbolTable.look(!ruleTable, Symbol.symbol categoryname) of
					 SOME rules => rules
				       | NONE => nil)
				    @ rules)
    

fun getRules categoryname = 
    case SymbolTable.look(!ruleTable, Symbol.symbol categoryname) of
	SOME rules => rules
      | NONE => nil

end
