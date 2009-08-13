structure Match =
struct

val i2s = Util.i2s
val r2s = Util.r2s
val b2s = Util.b2s
val e2s = ExpPrinter.exp2str

(* define common patterns *)
fun one sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_any, Pattern.ONE))
fun any sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_any, Pattern.ZERO_OR_MORE))
fun some sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_any, Pattern.ONE_OR_MORE))
fun anyfun sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anyfun, Pattern.ONE))
fun anyterm sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anyterm, Pattern.ONE))
fun anynum sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anynumeric, Pattern.ONE))
(*fun anysym sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anysymbol, Pattern.ONE))*)
fun onesym sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anysymbol, Pattern.ONE))
fun anysym sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anysymbol, Pattern.ZERO_OR_MORE))
fun somesym sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anysymbol, Pattern.ONE_OR_MORE))
fun anydiff sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anydiffterm, Pattern.ONE))
(* this matches a symbol of a particular name *)
fun anysym_with_predlist preds sym = Exp.TERM (Exp.PATTERN (sym, PatternProcess.combine_preds preds, Pattern.ONE))
val anysymnotdiff = anysym_with_predlist [PatternProcess.predicate_anysymbol, PatternProcess.notpred PatternProcess.predicate_anydiffterm]
(* This will match a particular symbol name *)
fun asym sym = Exp.TERM (Exp.PATTERN (sym, PatternProcess.gen_predicate_from_symbol sym, Pattern.ONE))


(* utility function *)
fun exp2term (Exp.TERM t) = t
  | exp2term exp = DynException.stdException(("Unexpected non-term: " ^ (e2s exp)),"Match.exp2term", Logger.INTERNAL)

(* common term rewriting commands *)
(* level - grab the next level of arguments *)
fun level (exp) =
    case exp of 
	Exp.FUN (_,args) => args
      | Exp.TERM (Exp.LIST (termlist, _)) => map Exp.TERM termlist
      | Exp.TERM (Exp.TUPLE termlist) => map Exp.TERM termlist
      | Exp.TERM (Exp.COMPLEX (a, b)) => map Exp.TERM [a,b]
      | _ => []

(* this will return a function to rebuild the head *)
fun head (exp) =
    case exp of
	Exp.FUN (funtype, args) => (fn(args')=> Exp.FUN (funtype, args'))
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => (fn(args')=> Exp.TERM (Exp.LIST (map exp2term args', dimlist)))
      | Exp.TERM (Exp.TUPLE (termlist)) => (fn(args') => Exp.TERM (Exp.TUPLE (map exp2term args')))
      | Exp.TERM (Exp.COMPLEX (a, b)) => (fn(args') => Exp.TERM (Exp.COMPLEX (exp2term (List.nth (args', 0)), exp2term (List.nth (args', 1)))))
      | _ => (fn(args') => exp)

(* level and head are identity functions *)
(* exp == (head exp) (level exp)*)

fun equiv (exp1, exp2) = 
    let
	val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (exp1, exp2)
	(*val _ = Util.log ("Comparing '"^(e2s exp1)^"' with '"^(e2s exp2)^"': result=" ^ (b2s result))*)
    in
	result
    end

fun findOnce (pattern, target) =
    if equiv (pattern, target) then
	SOME target
    else 
	foldl (fn(a,b)=> case b of 
			     SOME v => SOME v
			   | NONE => findOnce (pattern, a)) 
	      NONE
	      (level target)

fun findOnceWithPatterns (pattern, target) =
    let
	val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pattern, target)
    in
	if result then
	    SOME assigned_patterns
	else
	    foldl (fn(a,b)=> case b of 
				 SOME v => SOME v
			       | NONE => findOnceWithPatterns (pattern, a)) 
		  NONE
		  (level target)
    end
    
fun findRecursive (pattern, target) = 
    (if equiv (pattern, target) then
	 [target]
     else
	 [])
    @ 
    (foldl 
	 (fn(a,b)=> b @ (findRecursive (pattern, a)))
	 []
	 (level target))

fun replaceSymbol (sym,repl_exp) exp =
    case exp of
	Exp.TERM (Exp.SYMBOL (sym',_)) => 
	      if sym=sym' then
		  case repl_exp of
		      Exp.FUN (Fun.BUILTIN Fun.GROUP, args) => args
		    | _ => [repl_exp]
	      else
		  [exp]
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => [Exp.TERM (Exp.LIST (Util.flatmap (map exp2term o (replaceSymbol (sym, repl_exp)) o Exp.TERM) termlist, dimlist))]
      | Exp.TERM (Exp.TUPLE termlist) => [Exp.TERM (Exp.TUPLE (Util.flatmap (map exp2term o (replaceSymbol (sym, repl_exp)) o Exp.TERM) termlist))]
      | Exp.FUN (funtype, args) => [Exp.FUN (funtype, Util.flatmap (replaceSymbol (sym, repl_exp)) args)]
      | _ => [exp]

fun replacePattern assigned_patterns exp =
    foldl 
	(fn(pattern,exp) => Util.hd (replaceSymbol pattern exp))
	exp
	assigned_patterns

fun rule2str (exp1, exp2) =
    (e2s exp1) ^ " -> " ^ (e2s exp2)
fun rules2str rules =
    "{" ^ (String.concatWith ", " (map (fn(rule)=> rule2str rule) rules)) ^ "}"

(* replaces the pat_exp with repl_exp in the expression, returning the new expression.  This function will operate recursively through the expression data structure. *)
fun applyRewriteExp (rewrite as {find,test,replace} : Rewrite.rewrite) exp =
    let
	val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (find, exp)
	val run_test = case test of SOME v => true | NONE => false

	(* Test the expression only if an additional predicate test is included in the rule (run_test is true) *)
	fun test_exp() = 
	    let
		val test_fun = valOf test
	    in
		test_fun (exp, assigned_patterns)
	    end

	val exp' = if result andalso (not run_test orelse (test_exp())) then
		       let
			   (* convert the repl_exp by removing all the pattern variables that have been assigned *)	    
			   val repl_exp' = case replace of
					       Rewrite.RULE repl_exp => replacePattern assigned_patterns repl_exp
					     | Rewrite.ACTION (sym, action_fun) => action_fun exp

			   (* log if desired *)
			   val _ = if DynamoOptions.isFlagSet "logrewrites" then
				       Util.log ("Rewriting Rule '"^(Rewrite.rewrite2str rewrite)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   else
				       ()
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyRewriteExp rewrite arg) (level repl_exp'))
		       in
			   exp'
		       end
		   else
		       (head exp) (map (fn(arg)=> applyRewriteExp rewrite arg) (level exp))
		       
    in
	exp'
    end

fun applyRewritesExp (rewritelist:Rewrite.rewrite list) exp = 
    if List.length rewritelist > 0 then
	let
	    val ret = foldl
			  (fn({find, test, replace},ret : ((Symbol.symbol * Exp.exp) list * Rewrite.rewrite) option) => 
			     case ret of 
				 SOME v => SOME v (* already found a match here so skip the rest*)
			       | NONE => 
				 let
				     val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (find, exp)
				     val run_test = case test of SOME v => true | NONE => false
											  
				     (* Test the expression only if an additional predicate test is included in the rule (run_test is true) *)
				     fun test_exp() = 
					 let
					     val test_fun = valOf test
					 in
					     test_fun (exp, assigned_patterns)
					 end
				 in
				     if result andalso (not run_test orelse (test_exp())) then
					 SOME (assigned_patterns, {find=find, test=test, replace=replace})
				     else
					 NONE
				 end)
			  NONE
			  rewritelist
			  
	    (*val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)*)
	    val exp' = case ret of
			   SOME (assigned_patterns, rewrite as {find,test,replace}) =>
			   let
			       (* convert the repl_exp by removing all the pattern variables that have been assigned *)	    
			       val repl_exp' = case replace of
						   Rewrite.RULE repl_exp => replacePattern assigned_patterns repl_exp
						 | Rewrite.ACTION (sym, action_fun) => action_fun exp
										       
			       (* log if desired *)
			       val _ = if DynamoOptions.isFlagSet "logrewrites" then
					   Util.log ("Rewriting Rule '"^(Rewrite.rewrite2str rewrite)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				       else
					   ()
					   
			       (* substitute it back in, but call replaceExp on its arguments *)
			       val exp' = (head repl_exp') (map (fn(arg)=> applyRewritesExp rewritelist arg) (level repl_exp'))
			   in
			       exp'
			   end
			 | NONE => 
			   (head exp) (map (fn(arg)=> applyRewritesExp rewritelist arg) (level exp))
		       
	in
	    exp'
	end
    else
	exp

(* apply rules and repeat *)
fun repeatApplyRewriteExp rewrite exp =
    let
	val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"

	fun repeatApplyRewriteExp_helper limit rewrite exp =
	    if limit = 0 then
		(Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		 exp)
	    else
		let
		    val exp' = applyRewriteExp rewrite exp
		in
		    if ExpEquality.equiv (exp, exp') then
			exp
		    else
			repeatApplyRewriteExp_helper (limit-1) rewrite exp'
		end

    in
	repeatApplyRewriteExp_helper iter_limit rewrite exp
    end

fun repeatApplyRewritesExp rewrites exp =
    if List.length rewrites > 0 then
	let
	    val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"
			     
	    fun repeatApplyRewritesExp_helper limit rewrites exp =
		if limit = 0 then
		    (Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		     exp)
		else
		    let
			val exp' = applyRewritesExp rewrites exp
		    in
			if ExpEquality.equiv (exp, exp') then
			    exp
			else
			    repeatApplyRewritesExp_helper (limit-1) rewrites exp'
		    end
		    
	in
	    repeatApplyRewritesExp_helper iter_limit rewrites exp
	end
    else
	exp

end
