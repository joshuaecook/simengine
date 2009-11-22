signature MATCH =
sig

(* Primary rule rewrite commands *)
val applyRewriteExp : Rewrite.rewrite -> Exp.exp -> Exp.exp
val applyRewritesExp : Rewrite.rewrite list -> Exp.exp -> Exp.exp
(* The following ones will repeat after finding a rewrite - useful for recursive rewrites *)
val repeatApplyRewriteExp : Rewrite.rewrite -> Exp.exp -> Exp.exp
val repeatApplyRewritesExp : Rewrite.rewrite list -> Exp.exp -> Exp.exp

(* Helper functions to build up pattern expressions - pull out more defined below as necessary *)
val any : string -> Exp.exp (* zero or more of anything *)
val some : string -> Exp.exp (* one or more of anything *)
val one : string -> Exp.exp (* just match one only *)
val anyterm : string -> Exp.exp (* match one or more terms *)
val anyfun : string -> Exp.exp (* match one or more functions *)
val anysym_with_predlist : PatternProcess.predicate list -> Symbol.symbol -> Exp.exp (* if you want to specify a particular set of predicates for the pattern *)
val asym : Symbol.symbol -> Exp.exp (* match a particular symbol by name - ex. I want to find 'Vm' - asym (Symbol.name "Vm") *)

(* Matching functions *)
val findOnce : (Exp.exp * Exp.exp) -> Exp.exp option (* Try to find just one match, return the first one found as an option *)
val findRecursive : (Exp.exp * Exp.exp) -> Exp.exp list (* Recursively search and find all matching expressions (search for the 1st exp in the 2nd exp) *)

(* Utility functions - similar to Mathematica's Head[] and Level[] *)
val head : Exp.exp -> (Exp.exp list -> Exp.exp) (* returns a function that will reapply the top function of the expression *)
val level : Exp.exp -> Exp.exp list (* returns the argument of the function *)
(* These can be combined together to quickly traverse through an expression - notice that exp == (head exp)(level exp) *)

end


structure Match : MATCH =
struct

fun print (s) = ()


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

fun findOnce (pattern, target) =
    if ExpEquality.equiv (pattern, target) then
	SOME target
    else 
	foldl (fn(a,b)=> case b of 
			     SOME v => SOME v
			   | NONE => findOnce (pattern, a)) 
	      NONE
	      (level target)

fun findOnceWithPatterns (pattern, target) =
    let
	val assigned_patterns = ExpEquality.findMatches (pattern, target)
    in
	if not (null assigned_patterns) then
	    SOME (hd assigned_patterns)
	else
	    foldl (fn(a,b)=> case b of 
				 SOME v => SOME v
			       | NONE => findOnceWithPatterns (pattern, a)) 
		  NONE
		  (level target)
    end
    
fun findRecursive (pattern, target) = 
    (if ExpEquality.equiv (pattern, target) then
	 [target]
     else
	 [])
    @ 
    (foldl 
	 (fn(a,b)=> b @ (findRecursive (pattern, a)))
	 []
	 (level target))

fun replaceSymbol (sym,repl_exp) exp : Exp.exp=
    case exp of
	Exp.TERM (Exp.SYMBOL (sym',_)) => 
	      if sym=sym' then
		  repl_exp
	      else
		  exp
      | Exp.TERM (Exp.TUPLE termlist) 
	=> Exp.TERM (Exp.TUPLE (map exp2term 
				    (map (replaceSymbol(sym, repl_exp))
					 (map Exp.TERM 
					      termlist))))
      | Exp.FUN (funtype, args) 
	=> Exp.FUN (funtype, map (replaceSymbol (sym, repl_exp)) args)
      | Exp.META (Exp.SEQUENCE s) 
	=> Exp.META(Exp.SEQUENCE (map (replaceSymbol (sym, repl_exp)) s))
      | Exp.META (Exp.MAP {func, args} )
	=> Exp.META (Exp.MAP {func= replaceSymbol (sym, repl_exp) func,
			      args= replaceSymbol (sym, repl_exp) args})
      | Exp.META (Exp.APPLY {func, arg}) 
	=> Exp.META (Exp.APPLY {func= replaceSymbol (sym, repl_exp) func,
				arg= replaceSymbol (sym, repl_exp) arg})
      | Exp.META (Exp.LAMBDA {arg, body})
	=> if arg = sym then
	       exp
	   else
	       Exp.META (Exp.LAMBDA {arg=arg, body= replaceSymbol (sym, repl_exp) body})
      | _ => exp

fun replacePattern (assigned_patterns: Exp.exp SymbolTable.table ) exp =
    foldl 
	(fn(sym, exp) => replaceSymbol (sym, valOf (SymbolTable.look (assigned_patterns, sym))) exp)
	exp
	(SymbolTable.listKeys assigned_patterns)

fun rule2str (exp1, exp2) =
    (e2s exp1) ^ " -> " ^ (e2s exp2)
fun rules2str rules =
    "{" ^ (String.concatWith ", " (map (fn(rule)=> rule2str rule) rules)) ^ "}"

(* replaces the pat_exp with repl_exp in the expression, returning the new expression.  This function will operate recursively through the expression data structure. *)
fun applyRewriteExp (rewrite as {find,test,replace} : Rewrite.rewrite) exp =
    let
	val assigned_patterns = (ExpEquality.findMatches (find, exp))

	val _ = print ("  # matches = " ^ (Int.toString (length assigned_patterns)) ^ "\n")

	val _ = print ("Assigned patterns: \n")

	val run_test = case test of SOME v => true | NONE => false

	(* Test the expression only if an additional predicate test is included in the rule (run_test is true) *)
	fun test_exp() = 
	    let
		val test_fun = valOf test
		val match = hd assigned_patterns
	    in
		test_fun (exp, match)
	    end

	val exp' = if not (null assigned_patterns) andalso (not run_test orelse (test_exp())) then
		       let
			   val match = hd assigned_patterns
			   (* convert the repl_exp by removing all the pattern variables that have been assigned *)	    
			   val repl_exp' = Normalize.normalize(case replace of
								    Rewrite.RULE repl_exp => replacePattern (match) repl_exp
								  | Rewrite.ACTION (sym, action_fun) => action_fun exp)

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

	val _ = print ("new exp = " ^ (e2s (Normalize.normalize exp')) ^ "\n")
		       
    in
	exp'
    end

fun applyRewritesExp (rewritelist:Rewrite.rewrite list) exp = 
    if List.length rewritelist > 0 then
	let
	    val ret = foldl
			  (fn({find, test, replace},ret : ((Exp.exp)SymbolTable.table * Rewrite.rewrite) option) => 
			     case ret of 
				 SOME v => SOME v (* already found a match here so skip the rest*)
			       | NONE => 
				 let
				     val assigned_patterns = ExpEquality.findMatches (find, exp)
				     val run_test = case test of SOME v => true | NONE => false
											  
				     (* Test the expression only if an additional predicate test is included in the rule (run_test is true) *)
				     fun test_exp() = 
					 let
					     val test_fun = valOf test
					 in
					     test_fun (exp, hd assigned_patterns)
					 end
				 in
				     if not (null assigned_patterns) andalso (not run_test orelse (test_exp())) then
					 SOME (hd assigned_patterns, {find=find, test=test, replace=replace})
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
			       val repl_exp' = Normalize.normalize(case replace of
								       Rewrite.RULE repl_exp => replacePattern (assigned_patterns) repl_exp
								     | Rewrite.ACTION (sym, action_fun) => action_fun exp)
													   
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
		     Logger.log_warning(Printer.$("  exp = " ^ (ExpPrinter.exp2str exp)));
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
