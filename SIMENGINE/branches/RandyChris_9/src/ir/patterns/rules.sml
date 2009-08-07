structure Rules =
struct

type rule = (Exp.exp * Exp.exp) (* from and to *)
type action = (Exp.exp * (Exp.exp -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)

val replaceSubWithNeg : rule = 
    (ExpBuild.sub (Match.any "a", Match.any "b"),
     ExpBuild.plus [ExpBuild.var "a", ExpBuild.neg (ExpBuild.var "b")])

val replaceDivWithRecip : rule = 
    (ExpBuild.divide (Match.any "a", Match.any "b"),
     ExpBuild.times [ExpBuild.var "a", ExpBuild.power (ExpBuild.var "b", ExpBuild.int (~1))])

val aggregateSums : rule =
    (ExpBuild.plus [Match.any "a", ExpBuild.plus [Match.some "b"], Match.any "c"],
     ExpBuild.plus [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])
(* a + (b + c) + d -> a + b + c + d*)
val aggregateProds : rule =
    (ExpBuild.times [Match.any "a", ExpBuild.times [Match.some "b"], Match.any "c"],
     ExpBuild.times [ExpBuild.var "a", ExpBuild.var "b", ExpBuild.var "c"])

end
