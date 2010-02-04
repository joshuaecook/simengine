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
     handle _ => exec (KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("unable to open archive \"" ^ filename ^ "\"")))))
  | [a] => raise TypeMismatch ("expected a string but received " ^ (nick a))
  | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}


fun archiveCreate exec =
    fn [KEC.LITERAL (KEC.CONSTSTR filename),
	KEC.LITERAL (KEC.CONSTSTR dolFilename),
	dslFilenames] =>
       let val exe = Manifest.EXE {debug = true,
				   target = Target.CPU,
				   precision = DOF.DOUBLE,
				   profile = false}
       in
	   kecDescriptor (Archive.new {filename = filename,
				       dolFilename = dolFilename,
				       dslFilenames = nil,
				       environment = nil,
				       executable = exe})
       end
     | [a,b,c] => raise TypeMismatch ("expected a string, a string, and a sequence of strings but received " ^ (String.concatWith ", " [nick a, nick b]) ^ ", and " ^ (nick c))
     | args => raise IncorrectNumberOfArguments {expected = 3, actual = length args}

fun archivefun (f: (Archive.archive -> KEC.exp)) =
 fn [kec] => 
    let 
	val desc = kecToDescriptor kec
	val archive = case Descriptors.find (! descriptors, desc)
		       of NONE => raise ValueError ("invalid archive descriptor")
			| SOME x => x
    in
	f archive
    end
  | args => raise IncorrectNumberOfArguments {expected = 1, actual = length args}


fun archiveVersion exec =
    archivefun (KEC.LITERAL o KEC.CONSTREAL o Real.fromInt o Archive.version)

fun archiveCreationDate exec =
    archivefun (KEC.LITERAL o KEC.CONSTREAL o Real.fromInt o IntInf.toInt o Time.toSeconds o Archive.creationDate)

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
