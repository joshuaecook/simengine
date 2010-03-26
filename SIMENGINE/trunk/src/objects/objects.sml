signature OBJECTS =
sig
    (* Returns a class definition expression. *)
    val classdef: string -> KEC.exp option -> (KEC.replacement * KEC.member) list 
		  -> KEC.exp

    (* Returns a public instance method definition expression. *)
    val method: string -> (Symbol.symbol * KEC.typepattern) list -> KEC.exp 
		-> KEC.member

    (* Returns an public constant instance member expression. *)
    val constant: string -> KEC.exp 
		  -> KEC.member

    (* Returns the name of an object instance member. *)
    val memberName: KEC.member 
		    -> Symbol.symbol

    (* Indicates whether a given object instance member is publicly visible. *)
    val memberIsPublic: KEC.member 
			-> bool

    (* Returns an object instance construction expression. *)
    val instance: bool -> KEC.member list 
		  -> KEC.exp

    (* Returns a class object instance. *)
    val classinstance: (KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env * KEC.exp Env.env option ref) * PosLog.pos list) -> Symbol.symbol -> (KEC.replacement * KEC.member) list -> KEC.exp 
		       -> KEC.exp

    (* Returns an object instance method or attribute expression. *)
    val send: string -> KEC.exp 
	      -> KEC.exp
end

structure Objects : OBJECTS = 
struct

fun method name args exp =
    KEC.METHOD (Symbol.symbol name,
		KEC.PUBLIC,
		[{name=Symbol.symbol name,
		  args=args,
		  return=KEC.DONTCARE,
		  stms=[KEC.ACTION (KEC.EXP exp, PosLog.NOPOS)],
		  closure=Env.new()}])

fun constant name value =
    KEC.CONSTANT (Symbol.symbol name, KEC.PUBLIC, value)

fun instance allPublic members =
    KEC.OBJECT {members=ref members,
		allPublic=allPublic}

(* Returns the list of an object instance's members. *)
fun instanceMembers (KEC.OBJECT {members, allPublic}) = (! members)
  | instanceMembers obj = 
    raise DynException.TypeMismatch ("Expected a object instance but received " ^ (PrettyPrint.kecexp2nickname obj))

fun classdef name parent members =
    KEC.CLASSDEF {name=Symbol.symbol name,
		  members=members,
		  parent=(case parent of
			      SOME obj => obj
			    | NONE => instance false nil)}

fun send name object =    
    KEC.SEND {message=Symbol.symbol name, object=object}

fun memberName (KEC.METHOD (name, _, _)) = name
  | memberName (KEC.VAR {name, ...}) = name
  | memberName (KEC.CONSTANT (name, _, _)) = name
  | memberName (KEC.PROPERTY (_, {name, ...})) = name
  | memberName (KEC.CONSTRUCTOR {name=SOME name, ...}) = name
  | memberName (KEC.CONSTRUCTOR {name=NONE, ...}) = Symbol.symbol "new"

fun memberIsPublic (KEC.METHOD (_, KEC.PUBLIC, _)) = true
  | memberIsPublic (KEC.VAR {visibility=KEC.PUBLIC, ...}) = true
  | memberIsPublic (KEC.CONSTANT (_, KEC.PUBLIC, _)) = true
  | memberIsPublic (KEC.PROPERTY (KEC.PUBLIC, _)) = true
  | memberIsPublic (KEC.CONSTRUCTOR _) = true
  | memberIsPublic _ = false

fun classinstance env name members parent =
    let
	
	fun isConstructor (KEC.CONSTRUCTOR _) = true
	  | isConstructor _ = false

	fun getConstructor object =
	    List.find isConstructor (instanceMembers object)

	fun remove_repmod (_, member) = member


	fun inject_env (rep, KEC.CONSTANT (name, vis, value)) =
	    (rep, KEC.CONSTANT (name, vis, 
				KEC.APPLY {args=KEC.UNIT,
					   func=KEC.LAMBDA {args=nil, body=value, 
							    closure=Env.closure_union(env, Env.new()), 
							    undefined_args_allowed=false}}))
	  | inject_env (rep, KEC.VAR {name,visibility,typepattern,value}) =
	    (rep, KEC.VAR {name=name, visibility=visibility, typepattern=typepattern,
			   value=ref (KEC.APPLY {args=KEC.UNIT,
						 func=KEC.LAMBDA {args=nil, body=(! value), 
								  closure=Env.closure_union(env, Env.new()), 
								  undefined_args_allowed=false}})})
	  | inject_env (rep, KEC.METHOD (name, vis, runnables)) =
	    let
		fun inject {name, args, return, stms, closure} =
		    {closure=Env.closure_union (env, closure), name=name, args=args, return=return, stms=stms}
	    in
		(rep, KEC.METHOD (name, vis, map inject runnables))
	    end
	  | inject_env (rep, KEC.PROPERTY (vis, {name, read, write, expansionAllowed})) =
	    let
		fun inject {name, args, return, stms, closure} =
		    {closure=Env.closure_union (env, closure), name=name, args=args, return=return, stms=stms}
	    in
		(rep, KEC.PROPERTY (vis, {name=name,
					  expansionAllowed=expansionAllowed,
					  read=GeneralUtil.mapOpt inject read,
					  write=GeneralUtil.mapOpt inject write}))
	    end
	  | inject_env (rep, KEC.CONSTRUCTOR {name, init_methods, new_object}) =
	    let
		fun inject {super, args, return, stms, closure} =
		    {closure=Env.closure_union (env, closure), super=super, args=args, return=return, stms=stms}
	    in
		(rep, KEC.CONSTRUCTOR {name=name, init_methods=map inject init_methods, new_object=new_object})
	    end
	    

	fun fold_member ((_, member as KEC.VAR _), members) = member :: members
	  | fold_member ((_, member as KEC.CONSTANT _), members) = member :: members
	  | fold_member ((_, member as KEC.PROPERTY _), members) = member :: members
	  | fold_member ((KEC.REPLACE, member as KEC.METHOD _), members) = member :: members

	  | fold_member ((KEC.OVERLOAD, member as KEC.METHOD (name, vis, runnables)), members) =
	    let
		fun merge (KEC.METHOD (_, oldvis, oldrunnables), KEC.METHOD (name, vis, runnables)) =
		    let
			val vis' =
			    case (oldvis, vis) of
				(KEC.PUBLIC, KEC.PUBLIC) => KEC.PUBLIC
			      | (KEC.HIDDEN, KEC.PUBLIC) => KEC.PUBLIC
			      | (KEC.HIDDEN, KEC.HIDDEN) => KEC.HIDDEN
			      | (KEC.PUBLIC, KEC.HIDDEN) 
				=> raise DynException.TypeMismatch "Attempted to hide a previously public definition."

			val runnables' = runnables @ oldrunnables
		    in
			KEC.METHOD (name, vis', runnables')
		    end
		  | merge (_, _) = raise DynException.TypeMismatch "Unexpected type of member received in fold_member.merge."

		val oldmember = 
		    case List.find (fn (m) => PrettyPrint.member2name m = name) members of
			SOME (m as KEC.METHOD _) => m
		      | NONE => raise DynException.TypeMismatch "Attempted to overload non-existant definition."
		      | SOME _ => raise DynException.TypeMismatch "Attempted to overload non-function definition."

		val member' = merge (oldmember, member)
	    in
		member' :: members
	    end

	  | fold_member _ =
	    raise DynException.TypeMismatch "Unexpected member type in fold_member."

	fun prune_redundancy members =
	    let
		fun sameName a b = PrettyPrint.member2name a = PrettyPrint.member2name b

		fun filter_if_found_later nil = nil
		  | filter_if_found_later (member :: rest) =
		    if List.exists (sameName member) rest then
			filter_if_found_later rest
		    else
			member :: (filter_if_found_later rest)
	    in
		rev (filter_if_found_later (rev members))
	    end


	val self = KEC.SYMBOL (Symbol.symbol "self")
	val interfaces = KEC.list2kecvector []

        fun exp2stms exp = 
 	    [KEC.ACTION (KEC.EXP exp, PosLog.NOPOS)] 

	val malleability_methods = 
	    [KEC.METHOD (Symbol.symbol "addVar", KEC.PUBLIC, 
			 [{name=Symbol.symbol "addVar", 
 		           args=[(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String"))],  
 		           return=KEC.DONTCARE,  
 		           stms=exp2stms (KEC.LIBFUN (Symbol.symbol "addvar", KEC.TUPLE [KEC.SYMBOL (Symbol.symbol "self"), KEC.SYMBOL (Symbol.symbol "name")])),  
 		           closure=Env.new()}, 
 		          {name=Symbol.symbol "addVar", 
 		           args=[(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String")),  
 		                 (Symbol.symbol "value", KEC.DONTCARE)],  
 		           return=KEC.DONTCARE,  
 		           stms=exp2stms (KEC.LIBFUN (Symbol.symbol "addandsetvar", KEC.TUPLE [KEC.SYMBOL (Symbol.symbol "self"), KEC.SYMBOL (Symbol.symbol "name"), KEC.SYMBOL (Symbol.symbol "value")])),  
 		           closure=Env.new()}]),


	     method "addConst" [(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String")),
				(Symbol.symbol "exp", KEC.DONTCARE)]
		    (KEC.LIBFUN (Symbol.symbol "addconst",
				 KEC.TUPLE [self, KEC.SYMBOL (Symbol.symbol "name"), KEC.SYMBOL (Symbol.symbol "exp")])),
	     method "addMethod" [(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String")),
				 (Symbol.symbol "method", KEC.ARROW (KEC.DONTCARE, KEC.DONTCARE))]
		    (KEC.LIBFUN (Symbol.symbol "addmethod",
				 KEC.TUPLE [self, KEC.SYMBOL (Symbol.symbol "name"), KEC.SYMBOL (Symbol.symbol "method")])),
	     method "getMember" [(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String"))]
		    (KEC.LIBFUN (Symbol.symbol "getmember",
				 KEC.TUPLE [self, KEC.SYMBOL (Symbol.symbol "name")])),
	     method "setMember" [(Symbol.symbol "name", KEC.TYPE (Symbol.symbol "String")), (Symbol.symbol "value", KEC.DONTCARE)]
		    (KEC.LIBFUN (Symbol.symbol "setmember", 
				 KEC.TUPLE [KEC.SYMBOL (Symbol.symbol "self"), 
					    KEC.SYMBOL (Symbol.symbol "name"), KEC.SYMBOL (Symbol.symbol "value")])),
	     KEC.VAR {name=Symbol.symbol "isModified", visibility=KEC.PUBLIC, typepattern=KEC.TYPE (Symbol.symbol "Boolean"),
		      value=ref (KEC.LITERAL (KEC.CONSTBOOL false))}]
	    
	val (constructors, members') = List.partition (isConstructor o remove_repmod) (map inject_env members)

	val (parent_instance_members, parent_init_methods) = 
	    case getConstructor parent of
		NONE => (nil, nil)
	      | SOME (KEC.CONSTRUCTOR {name, init_methods, new_object}) => (new_object, init_methods)
	      | SOME _ => raise DynException.TypeMismatch ("Expected a constructor but received something else.")

	 (* Assumption: new_object will be filled in *) 
 	val parent_constructor = KEC.CONSTRUCTOR {name=SOME (Symbol.symbol "super"), init_methods=parent_init_methods, new_object=nil} 
 						 
				 
	val object = List.filter (not o isConstructor) 
				 (prune_redundancy ((foldl fold_member parent_instance_members members') @ malleability_methods))

	(* Reconciles the dummy reference to "super" in a constructors. *)
	fun inject_super_constructor (KEC.CONSTRUCTOR {init_methods, ...}) =
	    let 
		fun inject {args, return, stms, super, closure} =
		    {super=parent_constructor, args=args, return=return, stms=stms, closure=closure}
	    in
		KEC.CONSTRUCTOR {name=NONE,
				 new_object=object,
				 init_methods=map inject init_methods}
	    end
	  | inject_super_constructor _ = 
	    raise DynException.TypeMismatch ("Expected a constructor but received something else.")

	(* The default constructor for a class which has no user-defined constructor. *)
	val default_constructor = 
	    KEC.CONSTRUCTOR {name=NONE,
			     new_object=object,
			     init_methods=[{super=parent_constructor,
					    closure=Env.new(), 
					    args=nil, 
					    return=KEC.DONTCARE, 
					    stms=nil}]}

	(* Merges a list of constructor methods into a single datum. *)
	fun merge_constructors [] = inject_super_constructor default_constructor
	  | merge_constructors [cons as KEC.CONSTRUCTOR {name, init_methods, new_object}] = cons
	  | merge_constructors (KEC.CONSTRUCTOR {name=name1, init_methods=init_methods1, new_object=new_object1} ::
				KEC.CONSTRUCTOR {name=name2, init_methods=init_methods2, new_object=new_object2} :: 
				rest) =
	    merge_constructors (KEC.CONSTRUCTOR {name=NONE, init_methods=(init_methods1 @ init_methods2), new_object=new_object1} :: rest)
	  | merge_constructors (obj :: _) =
	    raise DynException.TypeMismatch ("Expected a constructor but received something else.")

	val constructor = merge_constructors (map inject_super_constructor (map remove_repmod constructors))

	val class_members = constructor :: 
			    malleability_methods @
			    [constant "parent" parent,
			     constant "name" (KEC.LITERAL (KEC.CONSTSTR (Symbol.name name))),
			     constant "interfaces" interfaces,
			     constant "instancemembers" (KEC.TUPLE (KEC.LITERAL (KEC.CONSTSTR "members") ::
								    (map (fn (m) => KEC.LITERAL (KEC.CONSTSTR (Symbol.name (memberName m)))) 
									 object))),
			     method "tostring" nil (KEC.LITERAL (KEC.CONSTSTR ("Class " ^ (Symbol.name name))))]

    in
	instance false class_members
    end


end
