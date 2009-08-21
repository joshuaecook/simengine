structure Rules =
struct

val replaceSubWithNeg : Rewrite.rewrite = 
    {find=ExpBuild.sub (Match.any "a", Match.any "b"),
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.var "a", ExpBuild.neg (ExpBuild.var "b")])}

val replaceDivWithRecip : Rewrite.rewrite = 
    {find=ExpBuild.divide (Match.any "a", Match.any "b"),
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.times [ExpBuild.var "a", ExpBuild.power (ExpBuild.var "b", ExpBuild.int (~1))])}

val distributeNeg : Rewrite.rewrite =
    {find=ExpBuild.neg (ExpBuild.plus[Match.some "a"]),
     test=SOME (fn(exp, assigned_patterns)=>not (List.exists (fn(sym, _)=>sym = (Symbol.symbol "a")) assigned_patterns)),
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.neg (ExpBuild.var "a")])}

val factorNegAddition : Rewrite.rewrite =
    {find=ExpBuild.plus [ExpBuild.neg (Match.any "a"), ExpBuild.neg (Match.any "b")],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.neg (ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b"]))}

val aggregateSums1 : Rewrite.rewrite =
    {find=ExpBuild.plus [Match.any "a", ExpBuild.plus [Match.any "b", Match.any "c"]],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])}

val aggregateSums2 : Rewrite.rewrite =
    {find=ExpBuild.plus [ExpBuild.plus [Match.any "a", Match.any "b"], Match.any "c"],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])}

val aggregateProds1 : Rewrite.rewrite =
    {find=ExpBuild.times [Match.any "a", ExpBuild.times [Match.any "b", Match.any "c"]],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.times [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])}

val aggregateProds2 : Rewrite.rewrite =
    {find=ExpBuild.times [ExpBuild.times [Match.any "a", Match.any "b"], Match.any "c"],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.times [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])}

val aggregateSums : Rewrite.rewrite =
    {find=ExpBuild.plus [Match.any "a", ExpBuild.plus [Match.some "b"], Match.any "c"],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])}
(* a + (b + c) + d -> a + b + c + d*)
val aggregateProds : Rewrite.rewrite =
    {find=ExpBuild.times [Match.any "a", ExpBuild.times [Match.some "b"], Match.any "c"],
     test=NONE,

     replace=Rewrite.RULE (ExpBuild.times [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])};

end
