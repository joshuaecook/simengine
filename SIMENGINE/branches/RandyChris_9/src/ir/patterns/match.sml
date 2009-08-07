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
	Exp.TERM (Exp.SYMBOL (sym',_)) => if sym=sym' then
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

type rule = (Exp.exp * Exp.exp)
type action = (Exp.exp * (Exp.exp -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)

fun rule2str (exp1, exp2) =
    (e2s exp1) ^ " -> " ^ (e2s exp2)
fun rules2str rules =
    "{" ^ (String.concatWith ", " (map (fn(rule)=> rule2str rule) rules)) ^ "}"

(* replaces the pat_exp with repl_exp in the expression, returning the new expression.  This function will operate recursively through the expression data structure. *)
fun applyRuleExp (rule as (pat_exp, repl_exp) : rule) exp =
    let
	val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)
	val exp' = if result then
		       let
			   (* convert the repl_exp by removing all the pattern variables that have been assigned *)	    
			   val repl_exp' = replacePattern assigned_patterns repl_exp

			   (* log if desired *)
			   val _ = if DynamoOptions.isFlagSet "logrewrites" then
				       Util.log ("Rewriting Rule '"^(e2s pat_exp)^"' -> '"^(e2s repl_exp)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   else
				       ()
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyRuleExp rule arg) (level repl_exp'))
		       in
			   exp'
		       end
		   else
		       (head exp) (map (fn(arg)=> applyRuleExp rule arg) (level exp))
		       
    in
	exp'
    end

fun applyRulesExp (rulelist:rule list) exp = 
    let
	val ret = foldl
		      (fn((pat_exp, repl_exp),ret : ((Symbol.symbol * Exp.exp) list * (Exp.exp * Exp.exp)) option) => 
			 case ret of 
			     SOME v => SOME v (* already found a match here so skip the rest*)
			   | NONE => 
			     let
				 val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)
			     in
				 if result then
				     SOME (assigned_patterns, (pat_exp, repl_exp))
				 else
				     NONE
			     end)
		      NONE
		      rulelist

	(*val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)*)
	val exp' = case ret of
		       SOME (assigned_patterns, rule as (pat_exp, repl_exp)) =>
		       let
			   (* convert the repl_exp by removing all the pattern variables that have been assigned *)	    
			   val repl_exp' = replacePattern assigned_patterns repl_exp

			   (* log if desired *)
			   val _ = if DynamoOptions.isFlagSet "logrewrites" then
				       Util.log ("Rewriting Rule '"^(e2s pat_exp)^"' -> '"^(e2s repl_exp)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   else
				       ()
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyRuleExp rule arg) (level repl_exp'))
		       in
			   exp'
		       end
		     | NONE => 
		       (head exp) (map (fn(arg)=> applyRulesExp rulelist arg) (level exp))
		       
    in
	exp'
    end

(* applyActionExp: Search for a pattern, apply an action to it, and return back to the expression *)
fun applyActionExp (action as (pat_exp, action_fun) : action) exp = 
    let
	val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)
	val exp' = if result then
		       let
			   (* apply the matching expression to the action function *)
			   val repl_exp' = action_fun exp

			   (* log if desired *)
			   val _ = if DynamoOptions.isFlagSet "logrewrites" then
				       Util.log ("Rewriting action on pattern '"^(e2s pat_exp)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   else
				       ()
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyActionExp action arg) (level repl_exp'))
		       in
			   exp'
		       end
		   else
		       (head exp) (map (fn(arg)=> applyActionExp action arg) (level exp))
		       
    in
	exp'
    end

(* applyActionsExp: Applies a series of actions on expressions *)
fun applyActionsExp (actionlist : action list) exp =
    let
	val ret = foldl
		      (fn((pat_exp, action_fun),ret : ((Symbol.symbol * Exp.exp) list * (Exp.exp * (Exp.exp -> Exp.exp))) option) => 
			 case ret of 
			     SOME v => SOME v (* already found a match here so skip the rest*)
			   | NONE => 
			     let
				 val (assigned_patterns, result) = ExpEquality.exp_equivalent [] (pat_exp, exp)
			     in
				 if result then
				     SOME (assigned_patterns, (pat_exp, action_fun))
				 else
				     NONE
			     end)
		      NONE
		      actionlist

	val exp' = case ret of
		       SOME (assigned_patterns, action as (pat_exp, action_fun)) =>
		       let
			   (* apply the matching expression to the action function *)
			   val repl_exp' = action_fun exp

			   (* log if desired *)
			   val _ = if DynamoOptions.isFlagSet "logrewrites" then
				       Util.log ("Rewriting action on pattern '"^(e2s pat_exp)^"': changed expression from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   else
				       ()
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyActionsExp actionlist arg) (level repl_exp'))
		       in
			   exp'
		       end
		     | NONE => 
		       (head exp) (map (fn(arg)=> applyActionsExp actionlist arg) (level exp))
    in
	exp'
    end

(* apply rules and repeat *)
fun repeatApplyRuleExp rule exp =
    let
	val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"

	fun repeatApplyRuleExp_helper limit rule exp =
	    if limit = 0 then
		(Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		 exp)
	    else
		let
		    val exp' = applyRuleExp rule exp
		in
		    if ExpEquality.equiv (exp, exp') then
			exp
		    else
			repeatApplyRuleExp_helper (limit-1) rule exp
		end

    in
	repeatApplyRuleExp_helper iter_limit rule exp
    end

fun repeatApplyRulesExp rules exp =
    let
	val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"

	fun repeatApplyRulesExp_helper limit rules exp =
	    if limit = 0 then
		(Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		 exp)
	    else
		let
		    val exp' = applyRulesExp rules exp
		in
		    if ExpEquality.equiv (exp, exp') then
			exp
		    else
			repeatApplyRulesExp_helper (limit-1) rules exp
		end

    in
	repeatApplyRulesExp_helper iter_limit rules exp
    end

fun repeatApplyActionExp action exp =
    let
	val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"

	fun repeatApplyActionExp_helper limit action exp =
	    if limit = 0 then
		(Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		 exp)
	    else
		let
		    val exp' = applyActionExp action exp
		in
		    if ExpEquality.equiv (exp, exp') then
			exp
		    else
			repeatApplyActionExp_helper (limit-1) action exp
		end

    in
	repeatApplyActionExp_helper iter_limit action exp
    end

fun repeatApplyActionsExp actions exp =
    let
	val iter_limit = DynamoOptions.getIntegerSetting "termrewritelimit"

	fun repeatApplyActionsExp_helper limit actions exp =
	    if limit = 0 then
		(Logger.log_warning(Printer.$("Exceeded iteration limit of " ^ (i2s iter_limit)));
		 exp)
	    else
		let
		    val exp' = applyActionsExp actions exp
		in
		    if ExpEquality.equiv (exp, exp') then
			exp
		    else
			repeatApplyActionsExp_helper (limit-1) actions exp
		end

    in
	repeatApplyActionsExp_helper iter_limit actions exp
    end

end
