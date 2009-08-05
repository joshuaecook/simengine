structure Rules =
struct

type rule = (Exp.exp * Exp.exp) (* from and to *)

val replaceSubWithNeg : rule = 
    (ExpBuild.sub (Match.any "a", Match.any "b"),
     ExpBuild.plus [ExpBuild.var "a", ExpBuild.neg (ExpBuild.var "b")])

val replaceDivWithRecip : rule = 
    (ExpBuild.divide (Match.any "a", Match.any "b"),
     ExpBuild.times [ExpBuild.var "a", ExpBuild.power (ExpBuild.var "b", ExpBuild.int (~1))])


end
