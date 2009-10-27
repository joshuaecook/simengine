structure Rules =
struct

(* a-b -> a+(-b) *)
val replaceSubWithNeg : Rewrite.rewrite = 
    {find=ExpBuild.sub (Match.any "a", Match.any "b"),
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.var "a", ExpBuild.neg (ExpBuild.var "b")])}

(* a/b -> a*b^(-1) *)
val replaceDivWithRecip : Rewrite.rewrite = 
    {find=ExpBuild.divide (Match.any "a", Match.any "b"),
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.times [ExpBuild.var "a", ExpBuild.power (ExpBuild.var "b", ExpBuild.int (~1))])}

(* -(a+b) -> (-a)+(-b) *)
val distributeNeg : Rewrite.rewrite =
    {find=ExpBuild.neg (ExpBuild.plus[Match.some "a"]),
     test=SOME (fn(exp, assigned_patterns)=>not (List.exists (fn(sym, _)=>sym = (Symbol.symbol "a")) assigned_patterns)),
     replace=Rewrite.RULE (ExpBuild.plus [ExpBuild.neg (ExpBuild.var "a")])}

(* (-a)+(-b) -> -(a+b) *)
val factorNegAddition : Rewrite.rewrite =
    {find=ExpBuild.plus [ExpBuild.neg (Match.any "a"), ExpBuild.neg (Match.any "b")],
     test=NONE,
     replace=Rewrite.RULE (ExpBuild.neg (ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b"]))}

(* Operator Simplification *)
(* a-b -> a+(-b) *)
(* a/b -> a*b^(-1) *)
(* a*(-1) -> -a *)
(* -0 -> 0 *)
(* a^0 -> 1 *)

(* Power Rules *)
(* a^b*a^c -> a^(b+c) *)
(* a^c*b^c -> (a*b)^c *)
(* (a^b)^c -> a^(b*c) *)
(* a*a -> a^2 *)
(* a*a^b -> a^(1+b) *)

(* Identity Rules *)
(* a+0 -> a *)
(* a*1 -> a *)
(* a^1 -> a *)
(* -(-a) -> a *)

(* Inverse Rules *)
(* a+(-a) -> 0 *)
(* a*a^(-1) -> 1 *)

(* Distribute Rules *)
(* -(a+b) -> (-a)+(-b) *)
(* a*(b+c) -> a*b+a*c *)

(* Factor Rules *)
(* a*b+a*c -> a*(b+c) *)

(* Logical Rules *)
(* a & true -> a *)
(* a | true -> true *)
(* a & false -> false *)
(* a | false -> a *)
(* a == a -> true *)
(* ~true -> false *)
(* ~false -> true *)
(* ~(~a) -> a *)

(* De Morgan's Expansion Laws *)
(* ~(a & b) -> ~a | ~b *)
(* ~(a | b) -> ~a & ~b *)

(* De Morgan's Factoring Laws *)
(* ~a | ~b -> ~(a & b) *)
(* ~a & ~b -> ~(a | b) *)

(* the following six rewrites are attempts at creating a rule to aggregate multiplications and additions *)
(* a + (b + c) -> a + b + c *)
(* (a*b)*c -> a*b*c *)

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
