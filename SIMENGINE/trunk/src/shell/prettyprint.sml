structure PrettyPrint =
struct

(*TODO:
  - pretty math printing
  - multi-line formatting with indenting
*)

(*TODO: remove this and replace with error handling*)
fun error msg = (print ("ERROR: " ^ msg ^ "\n");
		 DynException.setErrored())

fun stream2str (KEC.INSTREAM _) = "input"
  | stream2str (KEC.OUTSTREAM _) = "output"

exception ImpossibleListType

fun keclist2list (KEC.VECTOR vec) =
    KEC.kecvector2list vec
  | keclist2list _ = 
    raise ImpossibleListType

fun member2name (KEC.METHOD (name, _, _)) = name
  | member2name (KEC.VAR {name, ...}) = name
  | member2name (KEC.CONSTANT (name, _, _)) = name
  | member2name (KEC.CONSTRUCTOR _) = Symbol.symbol "new"
  | member2name (KEC.PROPERTY (_, {name, ...})) = name

fun typepattern2str (KEC.TYPE s)  = Symbol.name s
  | typepattern2str (KEC.DONTCARE) = "_"
  | typepattern2str (KEC.UNITTYPE) = "()"
  | typepattern2str (KEC.TUPLETYPE tps) = "(" ^ (String.concatWith ", " (map typepattern2str tps)) ^ ")"
  | typepattern2str (KEC.COMPOUNDTYPE (n, typ as KEC.ARROW _)) =
    (Symbol.name n) ^ " of (" ^ (typepattern2str typ) ^ ")"
  | typepattern2str (KEC.COMPOUNDTYPE (n, typ as KEC.COMPOUNDTYPE _)) =
    (Symbol.name n) ^ " of (" ^ (typepattern2str typ) ^ ")"
  | typepattern2str (KEC.COMPOUNDTYPE (n, typ)) = 
    (Symbol.name n) ^ " of " ^ (typepattern2str typ)
  | typepattern2str (KEC.ARROW (typ1, typ2)) = 
    let
	fun typ2str t = 
	    case t of KEC.ARROW _ => "(" ^ (typepattern2str t) ^ ")"
		    | KEC.COMPOUNDTYPE _ => "(" ^ (typepattern2str t) ^ ")"
		    | _ => (typepattern2str t)
    in
	(typ2str typ1) ^ " -> " ^ (typ2str typ2)
    end

fun arg2str (name, KEC.DONTCARE) = (Symbol.name name) 
  | arg2str (name, typesig) = (Symbol.name name) ^ ":" ^ (typepattern2str typesig)

fun args2str [] = "()"
  | args2str [arg] = arg2str arg
  | args2str args = "(" ^ (String.concatWith ", " (map arg2str args)) ^ ")"

fun runnable2name nil = ""
  | runnable2name ({name, ...} :: _) = Symbol.name name

fun func2str {args, return, ...} =
    case return of 
	KEC.ARROW _ => "(" ^ (args2str args) ^ " -> (" ^ (typepattern2str return) ^ "))"
      | _ => "(" ^ (args2str args) ^ " -> " ^ (typepattern2str return) ^ ")"

(* Returns a string representation of a real, converting SML's tilde notation to the more familiar minus sign for negatives. *)
fun real2str x = 
    let
	val trSign = fn #"~" => "-" | c => (Char.toString c)
    in
	String.translate trSign (Real.toString x)
    end


fun kecexp2prettystr (exec : (KEC.exp -> KEC.exp)) (exp: KEC.exp) : string =
    let
	val pretty = kecexp2prettystr exec
    in
	(case exp of
	    KEC.LITERAL (KEC.CONSTREAL r) 
	    => (real2str r)
	  | KEC.LITERAL (KEC.CONSTSTR s) 
	    => "\"" ^ (String.toString s) ^ "\""
	  | KEC.LITERAL (KEC.CONSTBOOL b) 
	    => (Bool.toString b)
	  | KEC.LITERAL (KEC.CONSTBINARY (bits, value)) 
	    => 
	    let
		fun strrep str x = 
		    if x <= 0 then
			""
		    else
			str ^ (strrep str (x-1))

		val str = IntInf.fmt StringCvt.HEX value

		val padding = strrep "0" (Int.div(bits, 4) - (String.size str))
	    in
		"0x" ^ padding ^ str ^ ":" ^ (Int.toString bits)
	    end
	  | KEC.SYMBOL s 
	    => "Symbol '" ^ (Symbol.name s) ^ "'"
	  | KEC.LIBFUN (name, args)
	    => "internal function `" ^ (Symbol.name name) ^ "` " ^ "(" ^ (pretty args) ^ ")"
	  | KEC.LAMBDA {args, body, ...}
	    => "anonymous function with " ^ (Int.toString (length args)) ^ " arguments."
	  | KEC.STMS stms 
	    => "sequence of " ^ (Int.toString (length stms)) ^ " executable statements."
	  | KEC.IFEXP {cond, ift, iff} 
	    => "{" ^ (pretty ift) ^ " WHEN " ^ (pretty cond) ^", " ^ (pretty iff) ^ " OTHERWISE}"
	  | KEC.VECTOR vec
	    => "[" ^ (String.concatWith ", " (map (fn(e) => pretty (e)) (KEC.kecvector2list vec))) ^ "]"
	  | KEC.TUPLE exps 
	    => "(" ^ (String.concatWith ", " (map pretty exps)) ^ ")"
	  | KEC.UNIT
	    => "()"
	  | KEC.OBJECT obj
	    => 
	    let 
		val exp' = exec (KEC.APPLY{func=KEC.SEND {message=Symbol.symbol "tostring", object=exp}, args=KEC.UNIT})
	    in
		case exp' of
		    KEC.LITERAL (KEC.CONSTSTR s) => s
		  | _ => pretty exp before 
			 DynException.stdException("tostring() didn't return a string",
						   "PrettyPrint.pretty",
						   Logger.INTERNAL)
	    end
	  | KEC.RUNNABLE (funcs)
	    => "function '" ^ (runnable2name funcs) ^ "' with types " ^ (String.concatWith " | " (map func2str funcs))
	  | KEC.CELL (_,KEC.REFERENCE e)
	    (* TODO: pretty-print type pattern? *)
	    => "&(" ^ (pretty (!e)) ^ ")"
	  | KEC.CELL (_,KEC.GETSET (g,s))
	    (* TODO: pretty-print type pattern? *)
	    => "&(" ^ (pretty (g())) ^ ")"
	  | KEC.TYPEEXP patt
	    => "TYPE(" ^ (typepattern2str patt) ^ ")"
	  | KEC.ERROR exp
	    => "ERROR"
	  | KEC.CLASSDEF {name, ...} 
	    => "classdef of " ^ (Symbol.name name)
	  | KEC.NAMESPACEDEF {name, ...} 
	    => "namespacedef of " ^ (Symbol.name name)
	  | KEC.APPLY {func, args} => "apply of " ^ (pretty args) ^ " to " ^ (pretty func)
	  | KEC.SEND {message, object} => "send of " ^ (Symbol.name message) ^ " to " ^ (pretty object)
	  | KEC.SATISFIES _ => "satisfies"
	  | KEC.POS _ => "position"
	  | KEC.MAKEREF _ => "makeref"
	  | KEC.DEREF _ => "deref"
	  | KEC.UNDEFINED => "undefined"
	  | KEC.PROCESS (p, name, args) 
	    => "process of " ^ name	    
	  | KEC.STREAM (s,_,n)
	    => "an " ^ (stream2str s) ^ " file stream to: " ^ n
	  | KEC.PROPERTYEXP {name, read=NONE, write=NONE, ...}
	    => "inaccessible property " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=SOME _, write=NONE, ...}
	    => "read-only property " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=NONE, write=SOME _, ...}
	    => "write-only property  " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=SOME _, write = SOME _, ...}
	    => "read/write property " ^ (Symbol.name name))
    end

fun kecexp2debugstr (exp: KEC.exp) : string =
	case exp of
	    KEC.LITERAL (KEC.CONSTREAL r) 
	    => (real2str r)
	  | KEC.LITERAL (KEC.CONSTSTR s) 
	    => "\"" ^ s ^ "\""
	  | KEC.LITERAL (KEC.CONSTBOOL b) 
	    => (Bool.toString b)
	  | KEC.LITERAL (KEC.CONSTBINARY (bits, value)) 
	    => 
	    let
		fun strrep str x = 
		    if x <= 0 then
			""
		    else
			str ^ (strrep str (x-1))

		val str = IntInf.fmt StringCvt.HEX value

		val padding = strrep "0" (Int.div(bits, 4) - (String.size str))
	    in
		"0x" ^ padding ^ str ^ ":" ^ (Int.toString bits)
	    end
	  | KEC.SYMBOL s 
	    => "'" ^ (Symbol.name s) ^ "'"
	  | KEC.LIBFUN (name, args)
	    => "internal function `" ^ (Symbol.name name) ^ "` " ^ "(" ^ (kecexp2debugstr args) ^ ")"
	  | KEC.LAMBDA {args, body, ...}
	    => "anonymous function with " ^ (Int.toString (length args)) ^ " arguments."
	  | KEC.STMS stms 
	    => "sequence of " ^ (Int.toString (length stms)) ^ " executable statements."
	  | KEC.IFEXP {cond, ift, iff} 
	    => "{" ^ (kecexp2debugstr ift) ^ " WHEN " ^ (kecexp2debugstr cond) ^", " ^ (kecexp2debugstr iff) ^ " OTHERWISE}"
	  | KEC.VECTOR vec
	    => "[" ^ (String.concatWith ", " (map (fn(e) => kecexp2debugstr (e)) (KEC.kecvector2list vec))) ^ "]"
	  | KEC.TUPLE exps 
	    => "(" ^ (String.concatWith ", " (map kecexp2debugstr exps)) ^")"
	  | KEC.UNIT
	    => "()"
	  | KEC.OBJECT obj
	    => 
	    let
	    in
		"An object with methods " ^ (String.concatWith ", " ([]))
	    end
	  | KEC.RUNNABLE (funcs)
	    => "function '" ^ (runnable2name funcs) ^ "' with types " ^ (String.concatWith " | " (map func2str funcs))
	  | KEC.CELL (_,KEC.REFERENCE e)
	    (* TODO: pretty-print type pattern? *)
	    => "&(" ^ (kecexp2debugstr (!e)) ^ ")"
	  | KEC.CELL (_,KEC.GETSET (g,s))
	    (* TODO: pretty-print type pattern? *)
	    => "&(" ^ (kecexp2debugstr (g())) ^ ")"
	  | KEC.TYPEEXP patt
	    => "TYPE(" ^ (typepattern2str patt) ^ ")"
	  | KEC.ERROR exp
	    => "ERROR"
	  | KEC.CLASSDEF {name, ...} 
	    => "classdef of " ^ (Symbol.name name)
	  | KEC.NAMESPACEDEF {name, ...} 
	    => "namespacedef of " ^ (Symbol.name name)
	  | KEC.APPLY {func, args} => "apply of " ^ (kecexp2debugstr args) ^ " to " ^ (kecexp2debugstr func)
	  | KEC.SEND {message, object} => "send of " ^ (Symbol.name message) ^ " to " ^ (kecexp2debugstr object)
	  | KEC.SATISFIES _ => "satisfies"
	  | KEC.POS _ => "position"
	  | KEC.MAKEREF _ => "makeref"
	  | KEC.DEREF _ => "deref"
	  | KEC.UNDEFINED => "undefined"
	  | KEC.PROCESS (p, name, args) 
	    => "process of " ^ name	    
	  | KEC.STREAM (s,_,n)
	    => "an " ^ (stream2str s) ^ " file stream to: " ^ n
	  | KEC.PROPERTYEXP {name, read=NONE, write=NONE, ...}
	    => "inaccessible property " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=SOME _, write=NONE, ...}
	    => "read-only property " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=NONE, write=SOME _, ...}
	    => "write-only property  " ^ (Symbol.name name)
	  | KEC.PROPERTYEXP {name, read=SOME _, write = SOME _, ...}
	    => "read/write property " ^ (Symbol.name name)

(* Returns a textual description of the type of an expression. *)
fun kecexp2nickname exp =
    case exp of
	KEC.LITERAL (KEC.CONSTREAL r) 
	=> "a number"
      | KEC.LITERAL (KEC.CONSTSTR s) 
	=> "a string"
      | KEC.LITERAL (KEC.CONSTBOOL b) 
	=> "a boolean value"
      | KEC.LITERAL (KEC.CONSTBINARY b) 
	=> "a binary value"
      | KEC.SYMBOL s 
	=> "an unknown identifier"
      | KEC.LAMBDA _
	=> "a function"
      | KEC.IFEXP _
	=> "an unresolved conditional expression"
      | KEC.VECTOR _
	=> "a vector"
      | KEC.UNIT
	=> "an empty value"
      | KEC.TUPLE _
	=> "a collection of function arguments"
      | KEC.OBJECT _
	=> "an object" (*TODO: use tostring? *)
      | KEC.TYPEEXP _
	=> "a type"
      | KEC.UNDEFINED
	=> "an undefined value"
      | KEC.CELL (_,KEC.REFERENCE e)
	=> kecexp2nickname (!e)
      | KEC.CELL (_,KEC.GETSET (g,s))
	=> kecexp2nickname (g())
      | KEC.PROPERTYEXP {name, read=NONE, write=NONE, ...}
	=> "inaccessible property"
      | KEC.PROPERTYEXP {name, read=SOME _, write=NONE, ...}
	=> "read-only property"
      | KEC.PROPERTYEXP {name, read=NONE, write=SOME _, ...}
	=> "write-only property "
      | KEC.PROPERTYEXP {name, read=SOME _, write = SOME _, ...}
	=> "read/write property"
      | KEC.PROCESS p 
	=> "a running process"
      | KEC.STREAM (s,_,_)
	=> "an " ^ (stream2str s) ^ " file stream"
      | _ => "an uncomputed expression: " ^ (kecexp2debugstr exp)

end
