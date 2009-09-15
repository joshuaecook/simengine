structure MalleabilityLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun std_addvar exec args =
    case args of
	 [KEC.OBJECT {members, allPublic}, KEC.LITERAL (KEC.CONSTSTR name)] => 
	 let
	     val _ = members := (KEC.VAR {name=Symbol.symbol name, 
					  visibility=KEC.PUBLIC, 
					  typepattern=KEC.DONTCARE,
					  value=ref KEC.UNDEFINED}) :: (!members)
	 in  
	     KEC.UNIT
	 end	 
       | [a, b] 
	 => raise TypeMismatch ("expected an object and a string but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


fun std_addAndSetVar exec args =
    case args of
	 [KEC.OBJECT {members, allPublic}, KEC.LITERAL (KEC.CONSTSTR name), exp] => 
	 let
	     val _ = members := (KEC.VAR {name=Symbol.symbol name, 
					  visibility=KEC.PUBLIC, 
					  typepattern=KEC.DONTCARE,
					  value=ref exp}) :: (!members)
	 in  
	     KEC.UNIT
	 end	 
       | [a, b, c] 
	 => raise TypeMismatch ("expected an object,  a string, and a value, but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}


fun std_addconst exec args =
    case args of
	 [KEC.OBJECT {members, allPublic}, KEC.LITERAL (KEC.CONSTSTR name), exp] => 
	 let
	     val _ = members := (KEC.CONSTANT (Symbol.symbol name, KEC.PUBLIC, exp)) :: (!members)
	 in  
	     KEC.UNIT
	 end	 
       | [a, b, c] 
	 => raise TypeMismatch ("expected an object, a string, and a value but received " ^ (PrettyPrint.kecexp2nickname a) ^ ", " ^ (PrettyPrint.kecexp2nickname b) ^ ", and " ^ (PrettyPrint.kecexp2nickname c))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}


fun std_addmethod exec args =
    case args of
	 [KEC.OBJECT {members, allPublic}, KEC.LITERAL (KEC.CONSTSTR name), KEC.RUNNABLE runnable] => 
	 let
	     val _ = members := (KEC.METHOD (Symbol.symbol name, KEC.PUBLIC, runnable)) :: (!members)
	 in  
	     KEC.UNIT
	 end
       | [KEC.OBJECT {members, allPublic}, KEC.LITERAL (KEC.CONSTSTR name), KEC.LAMBDA {args, body, closure, ...}] =>
	 let
	     val _ = members := (KEC.METHOD (Symbol.symbol name, KEC.PUBLIC, [{name=Symbol.symbol name, 
									       args=map (fn(a) => (a, KEC.DONTCARE)) args, 
									       return=KEC.DONTCARE, 
									       stms=[KEC.ACTION(KEC.EXP body, PosLog.NOPOS)],
									       closure=closure}])) :: (!members)
	 in  
	     KEC.UNIT
	 end
	 
       | [a, b, c] => 
	 raise TypeMismatch ("expected an object, a string, and a function but received " ^ (PrettyPrint.kecexp2nickname a) ^ ", " ^ (PrettyPrint.kecexp2nickname b) ^ ", and " ^ (PrettyPrint.kecexp2nickname c))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}

fun std_getmember exec args =
    case args of
	 [object, KEC.LITERAL (KEC.CONSTSTR membername)] => 
	 exec (KEC.SEND {message=Symbol.symbol membername, object=object})
       | [a, b] => 
	 raise TypeMismatch ("expected an object and a string but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


fun std_setmember exec args =
    case args of
	 [object, KEC.LITERAL (KEC.CONSTSTR membername), exp] => 
	 exec (KEC.STMS [KEC.ACTION(KEC.ASSIGN(KEC.SEND {message=Symbol.symbol membername, object=object}, exp), PosLog.NOPOS)])
       | [a, b, c] => 
	 raise TypeMismatch ("expected an object, a string, and a value but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
       | _ => raise IncorrectNumberOfArguments {expected=3, actual=(length args)}


fun std_clone exec args =
    case args of
	[object] =>
	let
	    fun cloneExp exp =
		case exp of
		    KEC.VECTOR {array, front_index, back_index, front_pad_size, back_pad_size}
		    => 
		    let
			val arrayCopy = Array.array(Array.length(!array), KEC.UNDEFINED)
			val _ = Array.copy{src= !array, dst=arrayCopy, di = 0}
		    in
			KEC.VECTOR {array= ref arrayCopy,
				    front_index = ref (!front_index),
				    back_index = ref(!back_index),
				    front_pad_size = ref(!front_pad_size),
				    back_pad_size = ref(!back_pad_size)}
				    
		    end

		  | KEC.CELL (tp, KEC.REFERENCE (ref e))
		    => KEC.CELL (tp, KEC.REFERENCE(ref e))
		  | KEC.CELL (tp, KEC.GETSET gs)
		    => KEC.CELL (tp, KEC.GETSET gs)
		  | KEC.OBJECT {members=ref members, allPublic}
		    => KEC.OBJECT {members=ref (map cloneMember members), allPublic=allPublic}
		  | _ => exp
	    and cloneMember member =
		case member of
		    KEC.VAR {name,visibility,typepattern,value=ref e} => 
		    (* re-cell the value *)
		    KEC.VAR {value=ref e,name=name,visibility=visibility,typepattern=typepattern}
		  | _ => member


	    val object' = cloneExp object
	in
	    object'
	end
      | _ =>raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_deepclone exec args =
    case args of
	[object] =>
	let
	    (* using naive table implementation for now *)
	    val vectorMap = ref nil
	    val objectMap = ref nil
	    val expMap = ref nil

	    fun findVector v1 =
		let		    
		    fun findVectorHelper nil =
			NONE
		      | findVectorHelper ((v2, v2')::rest) =
			if v1 = v2 then
			    SOME v2'
			else
			    findVectorHelper rest
		in
		    findVectorHelper (!vectorMap)
		end

	    fun addVector (v, v') =
		vectorMap := (v,v')::(!vectorMap)

	    fun addObject (obj, obj') =
		objectMap := (obj,obj')::(!objectMap)

	    fun addExp (e, e') =
		expMap := (e,e')::(!expMap)

	    fun findObject o1 =
		let
		    fun findObjectHelper nil = 
			NONE
		      | findObjectHelper ((o2, o2')::rest) =
			if o1 = o2 then
			    SOME o2'
			else
			    findObjectHelper rest
		in
		    findObjectHelper (!objectMap)
		end

	    fun findExp e1 =
		let
		    fun findExpHelper nil =
			NONE
		      | findExpHelper ((e2, e2')::rest) =
			if e1 = e2 then
			    SOME e2'
			else
			    findExpHelper rest
		in
		    findExpHelper (!expMap)
		end

		    

	    fun cloneExp originalexp =
		case originalexp of
		    KEC.LITERAL(lit) 
		    => originalexp
		  | KEC.SYMBOL _ 
		    => originalexp
		  | KEC.LIBFUN (s, exp) 
		    => KEC.LIBFUN(s, cloneExp exp)
		  | KEC.LAMBDA {args, body, closure, undefined_args_allowed}
		    => KEC.LAMBDA {args=args,
				   body=cloneExp body,
				   closure=closure,
				   undefined_args_allowed = undefined_args_allowed}
		  | KEC.STMS (stms) 
		    => KEC.STMS(map cloneStm stms)
		  | KEC.APPLY {func, args} 
		    => KEC.APPLY{func=cloneExp func,
				 args=cloneExp args}
		  | KEC.IFEXP {cond, ift, iff}
		    => KEC.IFEXP {cond=cloneExp cond, 
				  ift=cloneExp ift, 
				  iff=cloneExp iff}
		  | KEC.VECTOR {array, front_index, back_index, front_pad_size, back_pad_size}
		    => 
		    (case findVector array of
			 NONE =>
			 let
			     val vecref' = {array=ref (Array.fromList[]), front_index=ref 0, back_index=ref 0, front_pad_size = ref 0, back_pad_size = ref 0}
			     val _ = addVector (array, vecref')
			     val arrayCopy = Array.array(Array.length(!array), KEC.UNDEFINED)
			     val _ = Array.copy{src= !array, dst=arrayCopy, di = 0}
			     val _ = (#array vecref') := arrayCopy
				     
			     val _ = (#front_index vecref') := !front_index
			     val _ = (#back_index vecref') := !back_index
			     val _ = (#front_pad_size vecref') := !front_pad_size
			     val _ = (#back_pad_size vecref') := !back_pad_size
				    
			 in
			     KEC.VECTOR (vecref')
			     
			 end
		       | SOME newvec =>
			 KEC.VECTOR (newvec)
		    )
		  | KEC.TUPLE exps
		    => KEC.TUPLE (map cloneExp exps)
		  | KEC.UNIT 
		    => originalexp
		  | KEC.UNDEFINED
		    => originalexp
		  | KEC.OBJECT {members, allPublic}
		    =>
		    (case findObject members of
			 NONE =>
			 let
			     val members' = ref []
			     val _ = addObject (members, members')
			     val _ = members' := (map cloneMember (!members))
			 in
			     KEC.OBJECT {members=members',allPublic=allPublic}
			 end
		       | SOME members'=>
			 KEC.OBJECT{members=members', allPublic=allPublic}
		    )
		  | KEC.SEND {message, object}
		    => KEC.SEND {message=message,
				 object=cloneExp object}
		  | KEC.ERROR exp
		    => KEC.ERROR (cloneExp exp)
		  | KEC.POS (exp, pos) 
		    => KEC.POS (cloneExp exp, pos)
		  | KEC.TYPEEXP patt
		    => originalexp
		  | KEC.CELL (tp, KEC.REFERENCE (expref))
		    => 
		    (case findExp expref of
			 NONE =>
			 let
			     val expref' = ref KEC.UNIT
			     val _ = addExp (expref, expref')
			     val _ = expref' := (cloneExp (!expref))
			 in
			     KEC.CELL (tp, KEC.REFERENCE (expref'))
			 end
		       | SOME expref'=>
			 KEC.CELL (tp, KEC.REFERENCE expref')
		    )
		  | KEC.CELL (tp, KEC.GETSET (gs))
		    => KEC.CELL (tp, KEC.GETSET (gs))
		  | KEC.MAKEREF (typepattern, exp)
		    => KEC.MAKEREF (typepattern, cloneExp exp)
		  | KEC.DEREF exp
		    => KEC.DEREF (cloneExp exp)
		  | KEC.RUNNABLE runnable
		    => KEC.RUNNABLE (map cloneRunnable runnable)
		  | KEC.CLASSDEF {name, members, parent}
		    => originalexp
		  | KEC.NAMESPACEDEF {name, stms}
		    => KEC.NAMESPACEDEF {name=name,
					 stms=map (fn(v,s) => (v, cloneStm s)) stms}
		  | KEC.SATISFIES {class, interface}
		    => KEC.SATISFIES {class=cloneExp class,
				      interface=cloneExp interface}
		  | KEC.PROPERTYEXP property
		    => KEC.PROPERTYEXP (cloneProperty property)

	    and cloneProperty {name, expansionAllowed, read, write} =
		{name=name,
		 expansionAllowed=expansionAllowed,
		 read= case read of
			   SOME reads =>
			   SOME (map cloneRunnable reads)
			 | NONE => NONE,
		 write= case write of
			    SOME writes 
			    => SOME (map cloneRunnable writes)
			  | NONE => NONE}
		    
	    and cloneStm stm =
		case stm of
		    KEC.DEFINITION(KEC.DEFGLOBAL (r,s,t,exp), pos)
		    => KEC.DEFINITION(KEC.DEFGLOBAL (r,s,t,cloneExp exp), pos)

		  | KEC.DEFINITION(KEC.DEFLOCAL  (r,s,t,exp), pos)
		    => KEC.DEFINITION(KEC.DEFLOCAL  (r,s,t,cloneExp exp), pos)

		  | KEC.DEFINITION(KEC.DEFCONST  (r,s,t,exp), pos)
		    => KEC.DEFINITION(KEC.DEFCONST  (r,s,t,cloneExp exp), pos)

		  | KEC.ACTION(KEC.EXP exp, pos)
		    => KEC.ACTION (KEC.EXP (cloneExp exp), pos)

		  | KEC.ACTION(KEC.IMPORT _, _) 
		    => stm

		  | KEC.ACTION(KEC.OPEN {obj, excludes, include_privates}, pos)
		    => KEC.ACTION(KEC.OPEN {obj=cloneExp obj, 
					    excludes=excludes, 
					    include_privates=include_privates}, 
				  pos)

		  | KEC.ACTION(KEC.ASSIGN (dest, src), pos)
		    => KEC.ACTION(KEC.ASSIGN (cloneExp dest, cloneExp src), pos)
		       
	    and cloneMember member =
		case member of
		    KEC.METHOD (s,v,r) 
		    => KEC.METHOD (s,v,map cloneRunnable r) 
		  | KEC.CONSTRUCTOR {name, init_methods, new_object}
		    => KEC.CONSTRUCTOR {name=name,
					init_methods=init_methods, (*TODO: does this need to be mapped?*)
					new_object=new_object}
		  | KEC.VAR {name, visibility, typepattern, value} => 
		    (case findExp value
		      of SOME expref' => 
			 KEC.VAR {value=expref', name=name, visibility=visibility, typepattern=typepattern}
		       | NONE =>
			 let
			     val expref' = ref KEC.UNIT
			     val _ = addExp (value, expref')
			     val _ = expref' := (cloneExp (!value))
			 in
			     KEC.VAR {value=expref', name=name, visibility=visibility, typepattern=typepattern}
			 end)
		  | KEC.CONSTANT (s,v,e)
		    => KEC.CONSTANT (s,v,cloneExp e)
		  | KEC.PROPERTY (v,p)
		    => KEC.PROPERTY (v, cloneProperty p)
		    
	    and cloneRunnable {name, args, return, stms, closure} =
		{name=name, 
		 args=args, 
		 return=return, 
		 stms=map cloneStm stms,
		 closure=closure}

	    val object' = cloneExp object
	in
	    object'
	end
      | _ =>raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


val library = [{name="addvar", operation=std_addvar},
	       {name="addandsetvar", operation=std_addAndSetVar},
	       {name="addconst", operation=std_addconst},
	       {name="addmethod", operation=std_addmethod},
	       {name="getmember", operation=std_getmember},
	       {name="setmember", operation=std_setmember},
	       {name="clone", operation=std_clone},
	       {name="deepclone", operation=std_deepclone}]

end
