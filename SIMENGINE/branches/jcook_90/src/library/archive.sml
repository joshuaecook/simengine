structure ArchiveLib = struct

val TypeMismatch = DynException.TypeMismatch
and ValueError = DynException.ValueError
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

val nick = PrettyPrint.kecexp2nickname

structure Descriptors = ListMapFn(open Int type ord_key = int)

val descriptors: Archive.archive Descriptors.map ref = ref Descriptors.empty

local
    (* An archive descriptor, analogous to a unix file descriptor, is an opaque, unique integer. *)
    val ad: int ref = ref 0
in
fun descriptor () = let val x = 1 + (! ad) in x before ad := x end
end

val descriptorToKEC = KEC.LITERAL o KEC.CONSTREAL o Real.fromInt
val kecToDescriptor =
 fn KEC.LITERAL (KEC.CONSTREAL r) => Real.toInt IEEEReal.TO_NEAREST r
  | kec => raise TypeMismatch ("expected a number but received " ^ (nick kec))

fun kecDescriptor archive =
    let
	val desc = descriptor ()
	val _ = descriptors := Descriptors.insert (! descriptors, desc, archive)
    in
	descriptorToKEC desc
    end

fun archiveOpen exec =
 fn [KEC.LITERAL (KEC.CONSTSTR filename)] =>
    (kecDescriptor (Archive.openArchive filename)
     handle _ => KEC.UNIT)
  | [a] => raise TypeMismatch ("expected a string but received " ^ (nick a))
  | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}


fun archiveCreate exec =
    fn [KEC.LITERAL (KEC.CONSTSTR filename),
	KEC.LITERAL (KEC.CONSTSTR dolFilename),
	dslFilenames,
        executable] =>
       let val exe = Manifest.EXE {debug = true,
				   cSourceFilename = "",
				   precision = DOF.DOUBLE,
				   profile = false,
				   target = Target.CPU}
       in
	   kecDescriptor (Archive.new {filename = filename,
				       dolFilename = dolFilename,
				       dslFilenames = nil,
				       environment = nil,
				       executable = exe})
       end
     | [a,b,c, d] => raise TypeMismatch ("expected a string, a string, a sequence of strings, and an Executable object but received " ^ (String.concatWith ", " [nick a, nick b, nick c]) ^ ", and " ^ (nick d))
     | args => raise IncorrectNumberOfArguments {expected = 3, actual = length args}

fun descriptorToArchive desc =
    case Descriptors.find (! descriptors, desc)
     of SOME archive => archive
      | NONE => raise ValueError ("invalid archive descriptor")

fun archivefun (f: (Archive.archive -> KEC.exp)) =
 fn [kec] => 
    let val archive = descriptorToArchive (kecToDescriptor kec)
    in
	f archive
    end
  | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}


fun archiveVersion exec =
    archivefun (KEC.LITERAL o KEC.CONSTREAL o Real.fromInt o Archive.version)

fun archiveCreationDate exec =
    archivefun (KEC.LITERAL o KEC.CONSTREAL o Real.fromInt o IntInf.toInt o Time.toSeconds o Archive.creationDate)

fun executableToKEC (Manifest.EXE {debug, cSourceFilename, precision, profile, target}) =
    let 
	val kecSymbol = KEC.SYMBOL o Symbol.symbol

	val tableConstructor = 
	    KEC.SEND {message = Symbol.symbol "new",
		      object = kecSymbol "Table"}

	fun toVector object =
	    KEC.APPLY {func = KEC.SEND {message = Symbol.symbol "tovector",
					object = object},
		       args = KEC.UNIT}

	val targetClass = 
	    case target
	     of Target.CPU => Symbol.symbol "TargetCPU"
	      | Target.OPENMP => Symbol.symbol "TargetOpenMP"
	      | Target.CUDA => Symbol.symbol "TargetCUDA"

	val targetConstructor = 
	    KEC.SEND {message = Symbol.symbol "new",
		      object = KEC.SEND {message = targetClass,
					 object = kecSymbol "SimCompile"}}

	val executableConstructor =
	    KEC.SEND {message = Symbol.symbol "new",
		      object = KEC.SEND {message = Symbol.symbol "Executable",
					 object = kecSymbol "Archive"}}

	val settingsPairs = 
	    map (fn (k, v) => KEC.TUPLE [KEC.LITERAL (KEC.CONSTSTR k), KEC.LITERAL v])
		[("debug", KEC.CONSTBOOL debug),
		 ("emulate", KEC.CONSTBOOL false),
		 ("parallel_models", KEC.CONSTREAL 1.0),
		 ("precision", KEC.CONSTSTR (case precision of DOF.SINGLE => "single" | _ => "double")),
		 ("profile", KEC.CONSTBOOL profile),
		 ("target", KEC.CONSTSTR (case target of Target.CUDA => "cuda" | Target.OPENMP => "openmp" | Target.CPU => "cpu"))]

	val settings = 
	    KEC.APPLY {func = tableConstructor,
		       args = KEC.TUPLE [toVector (KEC.TUPLE (settingsPairs))]}

	val target = 
	    KEC.APPLY {func = targetConstructor,
		       args = KEC.TUPLE [settings]}
    in
	KEC.APPLY {func = executableConstructor,
		   args = KEC.TUPLE [target, KEC.LITERAL (KEC.CONSTSTR cSourceFilename)]}
				     
    end

fun archiveFindExecutable exec =
    fn [desc, predicate] =>
       let 
	   val archive = descriptorToArchive (kecToDescriptor desc)
	   fun matching executable =
	       case exec (KEC.APPLY {func = predicate,
				     args = KEC.TUPLE [executableToKEC executable]})
		of KEC.LITERAL (KEC.CONSTBOOL b) => b
		 | x => raise TypeMismatch ("expected a boolean but recieved " ^ (nick x))
       in
	   case Archive.findExecutable matching archive
	    of SOME executable => exec (executableToKEC executable)
	     | NONE => KEC.UNIT
       end

     | args => raise IncorrectNumberOfArguments {expected = 2, actual = length args}

fun archiveClose exec =
    archivefun (fn a => KEC.UNIT before Archive.close a)

fun archiveDestroy exec =
    archivefun (fn a => KEC.UNIT before Archive.destroy a)


val library = [{name = "archiveCreate", operation = archiveCreate},
	       {name = "archiveOpen", operation = archiveOpen},
	       {name = "archiveClose", operation = archiveClose},
	       {name = "archiveDestroy", operation = archiveDestroy},
	       {name = "archiveVersion", operation = archiveVersion},
	       {name = "archiveCreationDate", operation = archiveCreationDate}]

end
