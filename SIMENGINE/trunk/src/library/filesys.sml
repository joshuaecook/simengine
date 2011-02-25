(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

structure FileSysLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)

fun std_pwd exec args =
    KEC.LITERAL(KEC.CONSTSTR (Directory.pwd()))

fun std_chmod exec args =
    let
    in
	KEC.UNIT
    end

fun std_getPermissions exec args =
    KEC.UNIT

fun std_rmfile exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	KEC.UNIT before Directory.rmFile file
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_rmdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.rmDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_chdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.chDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_ls exec args =
    case args of
	nil =>
	let
	    fun buildentry file =
		KEC.LITERAL(KEC.CONSTSTR file)
	in
	    exec(KEC.list2kecvector (map buildentry (Directory.dir2files ".")))
	end
      | [KEC.LITERAL(KEC.CONSTSTR dir)] =>
	(let
	     val _ = print ("entering ls\n")
	     fun buildentry file =
		 KEC.LITERAL(KEC.CONSTSTR file)
	     val files = Directory.dir2files dir
	     val _ = print ("  files = " ^ (String.concatWith ", " files) ^ "\n")
	 in
	     exec(KEC.list2kecvector (map buildentry (files)))
	 end
	 handle _ => (print ("caught an exception\n");
		      KEC.UNIT))
      | [arg] 
	=> raise TypeMismatch ("expected no arguments or a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun std_isdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Directory.isDir dir))
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_isfile exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	KEC.LITERAL(KEC.CONSTBOOL (Directory.isFile file))
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_mkdir exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR dir)] =>
	KEC.UNIT before Directory.mkDir dir
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_realpath exec args =
    case args of
	[KEC.LITERAL(KEC.CONSTSTR file)] =>
	(case FilePath.getfullpath file of
	     SOME str =>
	     KEC.LITERAL(KEC.CONSTSTR(str))
	   | NONE => KEC.UNIT)
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun modtime exec = 
 fn args => (LibraryUtil.strToRealFun (fn path => Time.toReal (OS.FileSys.modTime path)) args
	     handle OS.SysErr _ => KEC.UNIT)

val library = [{name="pwd", operation=std_pwd},
	       {name="chmod", operation=std_chmod},
	       {name="getPermissions", operation=std_getPermissions},
	       {name="rmfile", operation=std_rmfile},
	       {name="rmdir", operation=std_rmdir},
	       {name="chdir", operation=std_chdir},
	       {name="ls", operation=std_ls},
	       {name="isdir", operation=std_isdir},
	       {name="isfile", operation=std_isfile},
	       {name="mkdir", operation=std_mkdir},
	       {name="realpath", operation=std_realpath},
	       {name="modtime", operation=modtime}]

end
