structure TypeLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_usererror [PosLog.NOPOS] (Printer.$ msg)

fun decell (KEC.CELL (t,KEC.REFERENCE(ref e))) = decell (e)
  | decell (KEC.CELL (t,KEC.GETSET(get, set))) = decell (get())
  | decell exp = exp


(* Indicates whether a type signature matchees a given quantity. *)
fun check exec (KEC.DONTCARE, _) = true

  | check exec (KEC.UNITTYPE, KEC.UNIT) = true

  | check exec (KEC.TYPE typ, KEC.LITERAL(KEC.CONSTREAL _)) = typ = (Symbol.symbol "Number")
  | check exec (KEC.TYPE typ, KEC.LITERAL(KEC.CONSTBOOL _)) = typ = (Symbol.symbol "Boolean")
  | check exec (KEC.TYPE typ, KEC.LITERAL(KEC.CONSTSTR _)) = typ = (Symbol.symbol "String")
  | check exec (KEC.TYPE typ, KEC.LITERAL(KEC.CONSTBINARY _)) = typ = (Symbol.symbol "Binary")
  | check exec (KEC.TYPE typ, KEC.VECTOR _) = typ = (Symbol.symbol "Vector")
  | check exec (KEC.TYPE typ, KEC.TUPLE _) = typ = (Symbol.symbol "Tuple")
  | check exec (KEC.TYPE typ, KEC.TYPEEXP _) = typ = (Symbol.symbol "Type")

  | check exec (KEC.TUPLETYPE typs, KEC.TUPLE exps) =
    let
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)
	(* val _ = print ("Matching against " ^ (pretty (KEC.TYPEEXP (KEC.TUPLETYPE typs))) ^ " with " ^ (pretty (KEC.TUPLE exps)) ^ "\n") *)
    in
	List.all (check exec) (ListPair.zip (typs, exps))
    end

  | check exec (typepattern, quantity) =
    let
	val pretty = PrettyPrint.kecexp2prettystr (decell o exec)
	(* val _ = print ("Matching against " ^ (pretty (KEC.TYPEEXP typepattern)) ^ " with " ^ (pretty quantity) ^ "\n") *)

	fun get_classname (obj as KEC.OBJECT {members=ref members, ...}) =
	    (case exec (KEC.SEND {message=Symbol.symbol "name", object=obj}) of
		 KEC.LITERAL (KEC.CONSTSTR s) => s
	       | _ => "" before error "Unexpected result of object class name lookup")
	  | get_classname exp =
	    "" before error ("Cannot compute classname on non-object: " ^ (PrettyPrint.kecexp2nickname exp))

	fun type_in_heirarchy typename obj =
	    let 
		val classname = get_classname (exec obj) 
	    in
		typename = classname orelse (classname <> "Object" andalso	    
					     case exec (KEC.SEND{message=Symbol.symbol "parent", object=obj}) of
						 KEC.UNIT => false
					       | parentobj => type_in_heirarchy typename parentobj)
	    end

	fun type_in_interfaces typename obj =
	    case exec (KEC.SEND{message=Symbol.symbol "interfaces", object=KEC.SEND {message=Symbol.symbol "class", object=obj}}) of
		KEC.VECTOR _ => false (*TODO: FILL IN WHEN INTERFACES ARE IMPLEMENETED*)
	      | _ => false before error "Unexpected result of object class interfaces lookup"


	fun arg2pattern (name, pattern) = pattern
	fun args2pattern [] = KEC.UNITTYPE
	  | args2pattern [arg] = arg2pattern arg
	  | args2pattern args = KEC.TUPLETYPE (map arg2pattern args)

	fun fun2pattern {args, return, ...} = KEC.ARROW (args2pattern args, return)

	fun lambda2pattern {args, ...} = 
	    case args of
		[] => KEC.ARROW (KEC.UNITTYPE, KEC.DONTCARE)
	      |	[arg] => KEC.ARROW (KEC.DONTCARE, KEC.DONTCARE)
	      | _ => KEC.ARROW (KEC.TUPLETYPE (map (fn (_) => KEC.DONTCARE) args), KEC.DONTCARE)

	fun isMemberNamedClass (KEC.CONSTANT (name, _, _)) = name = (Symbol.symbol "class")
	  | isMemberNamedClass _ = false


	val builtin_types = [Symbol.symbol "Number", Symbol.symbol "Boolean", Symbol.symbol "String", Symbol.symbol "Binary", Symbol.symbol "Vector", Symbol.symbol "Object", Symbol.symbol "Class", Symbol.symbol "Type"]

	(* Indicates whether a pair of type signatures are compatible. *)
	fun compat (KEC.DONTCARE, _) = true
	  | compat (_, KEC.DONTCARE) = true

	  | compat (KEC.TUPLETYPE typs, KEC.TUPLETYPE others) = 
	    List.all compat (ListPair.zip (typs, others))

	  | compat (KEC.ARROW (argtyp, rettyp), KEC.ARROW (otherargtyp, otherrettyp)) = 
	    compat (argtyp, otherargtyp) andalso 
	    compat (rettyp, otherrettyp)

	  | compat (KEC.COMPOUNDTYPE (name, typ), KEC.COMPOUNDTYPE (othername, othertyp)) = 
	    (name = othername) andalso 
	    compat (typ, othertyp)

	  | compat (KEC.TYPE typ1, KEC.TYPE typ2) =
	    (typ1 = typ2) orelse
	    (* builtin types have no parent type and so must match exactly. *)
	    (not (List.exists (fn (t) => typ1 = t) builtin_types)) andalso
	    let
		val dsltyp = exec (KEC.SYMBOL typ1)
		val parent = exec (Objects.send "parent" dsltyp)
		val parent_name = case exec (Objects.send "name" parent)
				   of KEC.LITERAL (KEC.CONSTSTR s) => s
				    | exp => raise TypeMismatch ("Expected parent name to be a string but found a "^(PrettyPrint.kecexp2nickname exp))
	    in
		compat (KEC.TYPE (Symbol.symbol parent_name), KEC.TYPE typ2)
	    end


	  | compat (typ, other) = (typ = other)

    in
	case (typepattern, quantity) of
	    (KEC.TYPE typ, obj as KEC.OBJECT {members=ref members, ...})
	    => if List.exists isMemberNamedClass members
	       then (* only classes don't contain the member "class" *)
		   (* if typ is in the heirarchy, or it is an interface satisfied by the object, then yes *)
		   type_in_heirarchy (Symbol.name typ) (KEC.SEND{message=Symbol.symbol "class", object=obj}) 
		   orelse type_in_interfaces typ obj
	       else
		   typ = (Symbol.symbol "Class")

	  | (KEC.COMPOUNDTYPE (name, subtype), _) 
	    => (case quantity of
		    KEC.VECTOR vec =>
		    if name = (Symbol.symbol "Vector") then
			(*TODO: THE PERFORMANCE OF THIS SUCKS*)
			foldl (fn(exp,result) => result andalso (check exec (subtype, exp))) true (KEC.kecvector2list vec)
		    else
			false
		  | KEC.TUPLE exps =>
		    if name = (Symbol.symbol "Tuple") then
			(*TODO: THE PERFORMANCE OF THIS SUCKS*)
			foldl (fn(exp,result) => result andalso (check exec (subtype, exp))) true exps
		    else
			false
		  | obj as KEC.OBJECT {members=ref members, ...}
		    => false (*TODO: fill this in when we actually implement subtypes *)
		  | _ => false)
	    

	  | (KEC.ARROW (arg, ret), KEC.RUNNABLE funs)
	    => List.exists (fn (pat) => compat (typepattern, pat)) 
			   (map fun2pattern funs)

	  | (KEC.ARROW (arg, ret), KEC.LAMBDA lam)
	    => compat (typepattern, lambda2pattern lam)
	       
	  (* TODO: check for implicitly callable objects such as Strings, Vectors, Tuples *)
	  | (KEC.ARROW (arg, ret), _)
	    => false

	  | _ => false
    end


(* Indicates whether a type pattern matches a given quantity. *)
fun istype exec args =
    case args of
	[KEC.TYPEEXP typepattern, quantity] 
	=> KEC.LITERAL(KEC.CONSTBOOL (check exec (typepattern, quantity)))
      | [typ, quantity] 
	=> raise TypeMismatch ("expected a type and a quantity but received " ^ (PrettyPrint.kecexp2nickname typ) ^ " and " ^ (PrettyPrint.kecexp2nickname quantity))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

val library = [{name="istype", operation=istype}]

end
