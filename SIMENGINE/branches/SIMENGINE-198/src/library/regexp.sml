structure RegExpLib =
struct

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

structure RE = RegExpFn (structure P = AwkSyntax structure E = BackTrackEngine)

fun error msg =
    Logger.log_error (Printer.$ msg)


fun checkMatch (regexp, str) =
    let		
	val cregex = RE.compileString regexp

	val matches = StringCvt.scanString (RE.find cregex)
					   str
    in
	case matches of
	    SOME _ => true
	  | NONE => false
    end

(* the following code is partially borrowed from SML documentation and the MLton mailing list.  It is a mess to conform to all the types used in the library *)
fun getMatch (regexp, str) =
    let		
	val cregex = RE.compileString regexp

	val matches: (({len: int, pos: Substring.substring}) MatchTree.match_tree * Substring.substring) option 
	  = (RE.find cregex) Substring.getc (Substring.full str)
    in
	case matches of
	    SOME (tree,rest) => 
	    let
		val treenodes = 
		    map (fn(n) => MatchTree.nth(tree, n)) (List.tabulate (MatchTree.num(tree), (fn(i) => i+1) ))
	    in
		map (fn({pos,len}) => Substring.string (Substring.extract(Substring.string (pos), 0, SOME len))) treenodes
	    end
	  | NONE => nil
    end
handle _ => nil (* handle is here since the parser used internally by the regexp engine seems to raise a variety of interesting exceptions.  We don't really care, we just want it to show there are no matches in this case *)

fun std_ismatch exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR pattern), KEC.LITERAL(KEC.CONSTSTR str)] =>
	KEC.LITERAL(KEC.CONSTBOOL (checkMatch (pattern, str)))
      | [a,b] => 
	raise TypeMismatch ("expected 2 strings, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


fun std_getMatches exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR pattern), KEC.LITERAL(KEC.CONSTSTR str)] =>
	exec(KEC.list2kecvector (map (fn(s) => KEC.LITERAL(KEC.CONSTSTR (s))) (getMatch (pattern, str))))
      | [a,b] => 
	raise TypeMismatch ("expected 2 strings, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


val library = [{name="ismatch", operation=std_ismatch},
	       {name="getMatches", operation=std_getMatches}]

end
