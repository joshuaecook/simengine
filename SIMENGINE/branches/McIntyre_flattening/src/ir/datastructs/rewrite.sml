structure Rewrite =
struct

type assigned_pattern_type = Exp.exp SymbolTable.table
type test_type = ((Exp.exp * assigned_pattern_type) -> bool) option

datatype rewrite_type = RULE of Exp.exp (* from and to *)
		     | ACTION of (Symbol.symbol * (Exp.exp -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)
type rewrite = {find:Exp.exp, test:test_type, replace:rewrite_type}

val e2s = ExpPrinter.exp2str
fun rewrite2str {find,test,replace} = 
    (e2s find) ^ " -> " ^ (case replace of 
			       RULE exp => e2s exp
			     | ACTION (sym,_) => "ACTION:"^(Symbol.name sym))

end
