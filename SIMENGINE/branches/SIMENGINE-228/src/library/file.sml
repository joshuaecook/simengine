structure FileLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)


fun std_openTextIn exec args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s)] =>
	(KEC.STREAM(KEC.INSTREAM(TextIO.openIn s), ref false, s)) (*TODO: exception handling if file not there *)
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_openTextOut exec args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s)] =>
	(KEC.STREAM(KEC.OUTSTREAM(TextIO.openOut s), ref false, s)) (*TODO: exception handling if file not there *)
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_openTextAppend exec args =
    case args of
	[KEC.LITERAL (KEC.CONSTSTR s)] =>
	(KEC.STREAM(KEC.OUTSTREAM(TextIO.openAppend s), ref false, s)) (*TODO: exception handling if file not there *)
      | [arg] 
	=> raise TypeMismatch ("expected a string but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}


fun std_getline exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename)] =>
	if !isdone then
	    KEC.UNIT
	else
	    (case TextIO.inputLine stream of
		 SOME s => KEC.LITERAL(KEC.CONSTSTR s)
	       | NONE => KEC.UNIT)
	    before isdone := TextIO.endOfStream(stream)
      | [arg] 
	=> raise TypeMismatch ("expected an input stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_getchar exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename)] =>
	if !isdone then
	    KEC.UNIT
	else
	    (case TextIO.input1 stream of
		 SOME (c) => KEC.LITERAL(KEC.CONSTSTR (Char.toString c))
	       | NONE => KEC.UNIT)
	    before isdone := TextIO.endOfStream(stream)
      | [arg] 
	=> raise TypeMismatch ("expected an input stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_getchars exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename), KEC.LITERAL(KEC.CONSTREAL r)] =>
	if !isdone then
	    KEC.UNIT
	else
	    KEC.LITERAL(KEC.CONSTSTR(implode(GeneralUtil.vector2list (TextIO.inputN (stream, Real.round r)))))
	    before isdone := TextIO.endOfStream(stream)
      | [arg1,arg2] 
	=> raise TypeMismatch ("expected an input stream and a number but received " ^ (PrettyPrint.kecexp2nickname arg1) ^ " and " ^ (PrettyPrint.kecexp2nickname arg2))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

fun std_getall exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename)] =>
	if !isdone then
	    KEC.UNIT
	else
	    let 
		val strs = GeneralUtil.vector2list(TextIO.inputAll stream)
		val kecstrs = KEC.LITERAL(KEC.CONSTSTR (implode strs))
		val _ = isdone := true
	    in
		kecstrs
	    end
      | [arg] 
	=> raise TypeMismatch ("expected an input stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_close exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename)] =>
	 KEC.UNIT before (TextIO.closeIn stream; isdone := true)

      | [KEC.STREAM(KEC.OUTSTREAM stream, isdone, filename)] =>
	 KEC.UNIT before (TextIO.closeOut stream; isdone := true)
      | [arg] 
	=> raise TypeMismatch ("expected an io stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}
    

fun std_endofstream exec args =
    case args of
	[KEC.STREAM(KEC.INSTREAM stream, isdone, filename)] =>
	let
	    val done = !isdone orelse TextIO.endOfStream(stream)
	in
	    KEC.LITERAL(KEC.CONSTBOOL done)
	    before isdone := done
	end
      | [arg] 
	=> raise TypeMismatch ("expected an input stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_flush exec args =
    case args of
	[KEC.STREAM(KEC.OUTSTREAM stream, isdone, filename)] =>
	KEC.UNIT before TextIO.flushOut stream
      | [arg] 
	=> raise TypeMismatch ("expected an output stream but received " ^ (PrettyPrint.kecexp2nickname arg))
      | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

fun std_outputstring exec args =
    case args of
	[KEC.STREAM(KEC.OUTSTREAM stream, isdone, filename), KEC.LITERAL(KEC.CONSTSTR s)] =>
	KEC.UNIT before TextIO.output (stream, s)
      | [arg1, arg2] 
	=> raise TypeMismatch ("expected an output stream and a string but received " ^ (PrettyPrint.kecexp2nickname arg1) ^ " and " ^ (PrettyPrint.kecexp2nickname arg2))
      | _ => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}
    





val library = [{name="getline", operation=std_getline},
	       {name="getchar", operation=std_getchar},
	       {name="getchars", operation=std_getchars},
	       {name="getall", operation=std_getall},
	       {name="close", operation=std_close},
	       {name="endofstream", operation=std_endofstream},
	       {name="openTextIn", operation=std_openTextIn},
	       {name="openTextOut", operation=std_openTextOut},
	       {name="openTextAppend", operation=std_openTextAppend},
	       {name="flush", operation=std_flush},
	       {name="outputstring", operation=std_outputstring}]

end
