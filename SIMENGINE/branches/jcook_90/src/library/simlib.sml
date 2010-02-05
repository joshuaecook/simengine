(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure SIMLIBLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun makeObjectFromFile exec =
    fn [KEC.LITERAL (KEC.CONSTSTR objectName), KEC.LITERAL (KEC.CONSTSTR filename)] => (KEC.LITERAL (KEC.CONSTSTR (Simlib.makeObjectFromFile {objectName=objectName, filename=filename}))
       handle _ => exec (KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("Simlib could not create object '" ^ objectName ^ "' from file '" ^ filename ^ "'.")))))
     | [a, b] => raise TypeMismatch ("expected String, String but received " ^ (PrettyPrint.kecexp2nickname a) ^ ",  " ^ (PrettyPrint.kecexp2nickname b)) 
     | args => raise IncorrectNumberOfArguments {expected = 2, actual = length args}

fun makeObjectFromContents exec =
    fn [KEC.LITERAL (KEC.CONSTSTR objectName), KEC.LITERAL (KEC.CONSTSTR data)] => (KEC.LITERAL (KEC.CONSTSTR (Simlib.makeObjectFromContents {objectName=objectName, data=data}))
       handle _ => exec (KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("Simlib could not create object '" ^ objectName ^ "' from contents in memory.")))))
     | [a, b] => raise TypeMismatch ("expected String, String but received " ^ (PrettyPrint.kecexp2nickname a) ^ ",  " ^ (PrettyPrint.kecexp2nickname b)) 
     | args => raise IncorrectNumberOfArguments {expected = 2, actual = length args}


fun getFileFromArchive exec =
    fn [KEC.LITERAL (KEC.CONSTSTR archive), KEC.LITERAL (KEC.CONSTSTR objectName), KEC.LITERAL (KEC.CONSTSTR filename)] => (KEC.UNIT before Simlib.getFileFromArchive {archive=archive, objectName=objectName, filename=filename}
       handle _ => exec (KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("Simlib could not retrieve object '" ^ objectName ^ "' from archive '" ^ archive ^ "' to file '" ^ filename ^ "'.")))))
     | [a, b, c] => raise TypeMismatch ("expected String, String, String but received " ^ (PrettyPrint.kecexp2nickname a) ^ ",  " ^ (PrettyPrint.kecexp2nickname b) ^ ",  " ^ (PrettyPrint.kecexp2nickname c)) 
     | args => raise IncorrectNumberOfArguments {expected = 3, actual = length args}


fun getContentsFromArchive exec =
    fn [KEC.LITERAL (KEC.CONSTSTR archive), KEC.LITERAL (KEC.CONSTSTR objectName)] => (KEC.LITERAL (KEC.CONSTSTR (Simlib.getContentsFromArchive {archive=archive, objectName=objectName}))
       handle _ => exec (KEC.ERROR (KEC.LITERAL (KEC.CONSTSTR ("Simlib could not retrieve object '" ^ objectName ^ "' contents from archive '" ^ archive ^"'.")))))
     | [a, b] => raise TypeMismatch ("expected String, String but received " ^ (PrettyPrint.kecexp2nickname a) ^ ",  " ^ (PrettyPrint.kecexp2nickname b)) 
     | args => raise IncorrectNumberOfArguments {expected = 2, actual = length args}


val library = [{name="makeObjectFromFile", operation=makeObjectFromFile},
	       {name="makeObjectFromContents", operation=makeObjectFromContents},
	       {name="getFileFromArchive", operation=getFileFromArchive},
	       {name="getContentsFromArchive", operation=getContentsFromArchive}]
end
