structure Rules =
struct

val expansionRules : Rewrite.rewrite list = 
    [
(* Distribute Rules *)
(* -(a+b) -> (-a)+(-b) *)
     {find=ExpBuild.neg (ExpBuild.plus [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.plus[ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.neg (ExpBuild.var "#x")),
							ExpBuild.pvar "a")])},
           
(* a*(b+c) -> a*b+a*c *) 
     {find=ExpBuild.times [Match.any "d1", ExpBuild.plus [Match.any "a"], Match.any "d2"],
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.plus[ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.times [ExpBuild.pvar "d1", ExpBuild.pvar "d2", ExpBuild.var "#x"]),
							ExpBuild.pvar "a")])},

(* De Morgan's Expansion Laws *)
(* ~(a & b) -> ~a | ~b *) 
     {find=ExpBuild.not (ExpBuild.land [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.lor [ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.not (ExpBuild.var "#x")),
						       ExpBuild.pvar "a")])},
     
(* ~(a | b) -> ~a & ~b *) 
     {find=ExpBuild.not (ExpBuild.lor [Match.any "a"]),
      test=NONE,
      replace=Rewrite.RULE (ExpBuild.land [ExpBuild.map (ExpBuild.lambda ("#x", ExpBuild.not (ExpBuild.var "#x")),
						       ExpBuild.pvar "a")])}
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
