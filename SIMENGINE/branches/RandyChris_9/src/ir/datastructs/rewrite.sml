structure Rewrite =
struct

datatype rewritetype = RULE of Exp.exp (* from and to *)
		     | ACTION of (Symbol.symbol * (Exp.exp -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)
type rewrite = {find:Exp.exp, replace:rewritetype}

val e2s = ExpPrinter.exp2str
fun rewrite2str {find,replace} = 
    (e2s find) ^ " -> " ^ (case replace of 
			       RULE exp => e2s exp
			     | ACTION (sym,_) => "ACTION:"^(Symbol.name sym))

end
