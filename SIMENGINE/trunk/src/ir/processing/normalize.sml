signature NORMALIZE = 
sig
val normalize: Exp.exp -> Exp.exp (* flatten sequences and normalize lambdas with applications *)

end

structure Normalize : NORMALIZE =
struct

fun normalize_with_env table exp =
    let
	fun flatten exps =
	    let
		fun combine (Exp.META(Exp.SEQUENCE s), exps) =
		    exps @ s
		  | combine (exp, exps) =
		    exps @ [exp]
	    in
		foldl combine nil exps
	    end
    
    in
	case exp of
	    Exp.FUN (f, args) => 
	    Exp.FUN (f, flatten(map (normalize_with_env table) args))
	  | Exp.TERM t => 
	    (case t of 
		 Exp.SYMBOL (s, _) =>
		 (case SymbolTable.look (table, s) of
		      NONE => exp
		    | SOME e => e)
	       | _ => exp)
	  | Exp.META m =>
	    let
		fun run argsym exp arg =
		    let
			val table' = SymbolTable.enter (table, argsym, arg)
		    in 
			normalize_with_env table' exp
		    end
	    in
		(case m of
		     Exp.APPLY {func, arg} =>
		     (case normalize_with_env table func of
			  Exp.META(Exp.LAMBDA {arg=argsym, body}) 
			  => 
			  run argsym body arg 
			| _ => exp)

		   | Exp.MAP {func, args} =>
		     (case normalize_with_env table func of
			  Exp.META(Exp.LAMBDA {arg=argsym, body}) 
			  => 
			  (case normalize_with_env table args of
			       Exp.META(Exp.SEQUENCE exps) =>
			       Exp.META(Exp.SEQUENCE (map (run argsym body) exps))
			     | arg => run argsym body arg)
			| _ => exp)				
     
		   | Exp.LAMBDA _ =>
		     exp
		   | Exp.SEQUENCE exps => Exp.META(Exp.SEQUENCE(flatten(map (normalize_with_env table) exps))))
	    end
    end

fun normalize exp = normalize_with_env SymbolTable.empty exp


end
