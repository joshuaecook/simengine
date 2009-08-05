structure Match =
struct

val b2s = Util.b2s
val e2s = ExpPrinter.exp2str

(* define common patterns *)
fun any sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_any, Pattern.ONE))
fun anyfun sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anyfun, Pattern.ONE))
fun anyterm sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anyterm, Pattern.ONE))
fun anynum sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anynumeric, Pattern.ONE))
fun anysym sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anysymbol, Pattern.ONE))
fun anydiff sym = Exp.TERM (Exp.PATTERN (Symbol.symbol sym, PatternProcess.predicate_anydiffterm, Pattern.ONE))

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
					      repl_exp
					  else
					      exp
      | Exp.TERM (Exp.LIST (termlist, dimlist)) => Exp.TERM (Exp.LIST (map (exp2term o (replaceSymbol (sym, repl_exp)) o Exp.TERM) termlist, dimlist))
      | Exp.TERM (Exp.TUPLE termlist) => Exp.TERM (Exp.TUPLE (map (exp2term o (replaceSymbol (sym, repl_exp)) o Exp.TERM) termlist))
      | Exp.FUN (funtype, args) => Exp.FUN (funtype, map (replaceSymbol (sym, repl_exp)) args)
      | _ => exp
	     

fun replacePattern assigned_patterns exp =
    foldl 
	(fn(pattern,exp) => replaceSymbol pattern exp)
	exp
	assigned_patterns

type rule = (Exp.exp * Exp.exp)

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
			   val _ = Util.log ("Ran replacement '"^(e2s repl_exp)^"' from '"^(e2s exp)^"' to '"^(e2s repl_exp')^"'")
				   
			   (* substitute it back in, but call replaceExp on its arguments *)
			   val exp' = (head repl_exp') (map (fn(arg)=> applyRuleExp rule arg) (level repl_exp'))
		       in
			   exp'
		       end
		   else
		       (head exp) (map (fn(arg)=> applyRuleExp rule arg) (level exp))
		       
	val _ = Util.log ("Apply rule '"^(rule2str (pat_exp, repl_exp))^"' to exp '"^(e2s exp)^"'")
	val _ = Util.log ("  Result: " ^ (e2s exp'))
    in
	exp'
    end

fun applyRulesExp rulelist exp = 
    foldl 
	(fn(rule, exp')=> applyRuleExp rule exp')
	exp
	rulelist

end
