(* logger.sml - Dynamo System Logger
 * Copyright 2007 Simatra Modeling Technologies
 * --------------------------------------------
 * 
 * See the Wiki entry for dynamo logger at:
 *   http://www.simatra-internal.com/doc/doku.php?id=dynamo_logger
 *)
signature LOGGER =
sig

(* Define the message type used in Logger *)
type message = Printer.text

(* Define data types used in logger *)
datatype message_type = 
	 NOTICE
       | WARNING
       | ERROR
       | FAILURE

datatype characterization =
	 INTERNAL
       | DATA
       | ASSERTION
       | USER
       | OTHER

(* any time this is updated, update the logging group in Options.sml*)
datatype group =
	 LUT_GEN
       | SCHEDULING      
       | LIBRARY
       | NOGROUP

datatype log_level =
	 ALL
       | WARNINGS (* and errors and failures *)
       | ERRORS (* and failures *)
       | FAILURES

type logtype = {loglevel: log_level,
		outstream: TextIO.outstream,
		output_groups: group list,
		id: int,
		name: string}


(* Define basic methods for logging events *)
val log_notice : message -> unit
val log_warning : message -> unit
val log_error : message -> unit
val log_error_with_position : PosLog.pos list -> message -> unit
val log_failure : message -> unit

(* When reading a data file, such as the registry, use this error routine *)
val log_data_error : string -> message -> unit (* first argument is the filename, second is the message to print *)

(* for exception handling *)
val log_stack : (unit -> message list) -> unit
val log_exception : characterization -> message_type -> message -> unit

(* to creating logging streams and add/remove to the logging list *)
val log_stdout : (log_level * group list) -> int
val log_stderr : (log_level * group list) -> int
val log_add : (string * log_level * group list) -> int (* generic logger *)
val log_remove : int -> unit

(* grab error counts *)
val getErrorCount : unit -> int
val getWarningCount : unit -> int

end
structure Logger : LOGGER =
struct

(* Define the message type used in Logger *)
type message = Printer.text

datatype message_type = 
	 NOTICE
       | WARNING
       | ERROR
       | FAILURE

datatype characterization =
	 INTERNAL
       | DATA
       | ASSERTION
       | USER
       | OTHER

(* any time this is updated, update the logging group in Options.sml*)
datatype group =
	 LUT_GEN
       | SCHEDULING      
       | LIBRARY
       | NOGROUP

datatype log_level =
	 ALL
       | WARNINGS (* and errors and failures *)
       | ERRORS (* and failures *)
       | FAILURES

type logtype = {loglevel: log_level,
		outstream: TextIO.outstream,
		output_groups: group list,
		id: int,
		name: string}

val logs = ref nil : logtype list ref
val warning_count = ref 0
val error_count = ref 0
val log_id = ref 0

fun getWarningCount () = !warning_count
fun getErrorCount () = !error_count

fun get_next_log_id() =
    let 
	val id = !log_id
	val _ = log_id := !log_id + 1
    in
	id
    end

fun group2str g =
    case g of
	LUT_GEN => "LUT GENERATION"
      | SCHEDULING => "SCHEDULING"
      | LIBRARY => "LIBRARY"
      | NOGROUP => ""

fun characterization2str c =
    case c of
	INTERNAL => "INTERNAL"
      | DATA => "DATA"
      | ASSERTION => "ASSERTION"
      | USER => "USER"
      | OTHER => "OTHER"

fun messagetype2str mt =
    case mt of
	NOTICE => ""
      | WARNING => "WARNING"
      | ERROR => "ERROR"
      | FAILURE => "FAILURE"

fun output_text outstream characterization messagetype message =
    let
	val c = 
	    case characterization of
		OTHER => ""
	      | _ => (characterization2str characterization) ^ " "
	    
	val mt = messagetype2str messagetype

	val prefix = case c ^ mt of
		       "" => []
		     | prefix => [Printer.$(prefix ^ ": ")]

	val text = Printer.SUB (prefix @ [message])
    in
	Printer.printtext (outstream, text, ~1)
    end
(*
fun output_line outstream characterization messagetype message =
    let
	val c = 
	    case characterization of
		OTHER => ""
	      | _ => (characterization2str characterization) ^ " "
	    
	val mt = messagetype2str messagetype

	val text = "\n" ^ c ^ mt ^ ": " ^ message

    in
	TextIO.output(outstream, text)
    end

fun output_lines outstream messages =
    (TextIO.output(outstream, "\n");
     app (fn(m) => TextIO.output(outstream, m ^ "\n"))
	 messages)
*)

fun is_logged output_groups NOGROUP = true
  | is_logged output_groups group = 
    List.exists (fn(g) => g = group) (output_groups)

fun sufficient_loglevel loglevel messagetype =
    case (messagetype, loglevel) of
	(_, ALL) => true
      | (WARNING, WARNINGS) => true
      | (ERROR, WARNINGS) => true
      | (FAILURE, WARNINGS) => true
      | (ERROR, ERRORS) => true
      | (FAILURE, ERRORS) => true
      | (FAILURE, FAILURES) => true
      | _ => false

fun log characterization messagetype message =
    (if messagetype2str messagetype = "WARNING" then
	 warning_count := !warning_count + 1
     else
	 if messagetype2str messagetype = "ERROR" then
	     error_count := !error_count + 1
	 else
	     ();
    app (fn({loglevel, outstream, output_groups,...}) =>
	   if sufficient_loglevel loglevel messagetype then
	       output_text outstream characterization messagetype message
	   else
	       ())
	(!logs))

fun log_exception characterization messagetype message =
    log characterization messagetype message 
(*
fun log_lines characterization messagetype nil = ()
  | log_lines characterization messagetype messages =
    app (fn({loglevel, outstream, output_groups}) =>
	   if sufficient_loglevel loglevel messagetype then
	       (output_line outstream characterization messagetype (hd messages);
		output_lines outstream (tl messages))
	   else
	       ())
	(!logs)
*)

fun log_notice message =
    app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel NOTICE then
	       output_text outstream OTHER NOTICE message
	   else
	       ())
	(!logs)

fun log_warning message =
    (warning_count := !warning_count + 1;
    app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel WARNING then
	       output_text outstream OTHER WARNING message
	   else
	       ())
	(!logs))


fun log_failure message =
    (error_count := !error_count + 1;
     app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel FAILURE then
	       output_text outstream OTHER FAILURE message
	   else
	       ())
	(!logs))


fun check_warning condition message =
    if condition then
	log_warning message
    else
	()

fun log_error message =
    (error_count := !error_count + 1;
    app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel ERROR then
	       output_text outstream USER ERROR (Printer.SUB [message])
	   else
	       ())
	(!logs))
    
fun log_internalerror message =
    (error_count := !error_count + 1;
    app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel ERROR then
	       output_text outstream INTERNAL ERROR (Printer.SUB [message])
	   else
	       ())
	(!logs))

fun log_data_error filename message =
    (error_count := !error_count + 1;
    app (fn({outstream, loglevel, ...}) =>
	   if sufficient_loglevel loglevel ERROR then
	       output_text outstream DATA ERROR (Printer.SUB [Printer.$("Error occurred in data file: " ^ filename),
							      message])
	   else
	       ())
	(!logs))
    
fun log_information messageFun group =
    let
	val logsToUse = List.filter (fn({output_groups, ...}) => is_logged output_groups group) (!logs)

	fun group2str' group =
	    let
		val str = group2str group
	    in
		if str = "" then
		    ""
		else
		    " for " ^ str
	    end
    in
	case logsToUse of
	    nil => ()
	  | _ => 
	    let
		val message = messageFun()
	    in
		app
		    (fn({loglevel, outstream, output_groups,...}) =>
		       if sufficient_loglevel loglevel NOTICE then
			   output_text outstream 
				       OTHER
				       NOTICE
				       (Printer.SUB((Printer.$("==Informational Log Entry" ^ (group2str' group) ^ "=="))
						    :: message))
		       else
			   ())
		    logsToUse
	    end
    end
fun log_stack messageFun = 
    log_information messageFun NOGROUP

fun log_user (poslog: PosLog.pos list) level message =
    let
	fun firstPos pos =
	    Printer.$("occurred at " ^ (PosLog.pos2str pos))

	fun backPos pos =
	    Printer.$("called from " ^ (PosLog.pos2str pos))

	fun prune_samelines nil = nil
	  | prune_samelines (entry::nil) = [entry]
	  | prune_samelines ((entry1 as PosLog.FILE {filename=fn1, filepath=fp1, line=l1, column=c1}) ::
			     (entry2 as PosLog.FILE {filename=fn2, filepath=fp2, line=l2, column=c2}) :: rest) =
	    if fn1 = fn2 andalso fp1 = fp2 andalso l1 = l2 then
		prune_samelines (entry2 :: rest)
	    else
		entry1 :: (prune_samelines (entry2 :: rest))
	  | prune_samelines ((entry1 as PosLog.CONSOLE {line=l1, column=c1}) ::
			     (entry2 as PosLog.CONSOLE {line=l2, column=c2}) :: rest) =
	    if l1 = l2 then
		prune_samelines (entry2 :: rest)
	    else
		entry1 :: (prune_samelines (entry2 :: rest))
	  | prune_samelines (entry1::entry2::rest) =
	    entry1 :: (prune_samelines (entry2::rest))

	fun posDump poslog =
	    let
		(* weed out nopos entries *)
		val poslog' = (List.filter (fn(p) => p <> PosLog.NOPOS) poslog)

		(* remove multiple consecutive entries from the same line *)
		val poslog'' = prune_samelines poslog'
	    in
		case poslog'' of
		    nil => Printer.SUB []
		  | first::rest =>
		    Printer.SUB(firstPos first :: 
				(map backPos rest))
	    end

	val message = Printer.SUB [message, posDump poslog]
		      
    in
	log USER level message
    end
	
fun log_usererror poslog message =
    log_user poslog ERROR message
val log_error_with_position = log_usererror

fun log_userwarning poslog message =
    log_user poslog WARNING message

fun log_stream (name, level, ostream, groups) =
    let
	val id = get_next_log_id()
	val _ = logs := {loglevel=level, outstream=ostream, output_groups=groups, id=id, name=name} :: (!logs)
		
	fun CurrentDateTime () = (Date.fromTimeLocal (Posix.ProcEnv.time ())) : Date.date
	val date = Date.toString (CurrentDateTime())
		   
	val _ = if sufficient_loglevel level NOTICE then
		    output_text ostream OTHER NOTICE (Printer.SUB [Printer.$("== Log '"^name^"' Initialization at " ^ date ^ " ==")])
		else
		    ()

    in
	id
    end
 
fun log_add (filename, level, groups) = 
    let
	val ostream = TextIO.openOut (filename)
    in
	log_stream(filename, level, ostream, groups)
    end

fun log_stderr (level, groups) =
    log_stream ("stderr", level, TextIO.stdErr, groups)

fun log_stdout (level, groups) =
    log_stream ("stdout", level, TextIO.stdOut, groups)

fun log_remove log_id =
    let
	val (matched, unmatched) = List.partition (fn{id,...}=>id=log_id) (!logs)
	val _ = logs := unmatched

	(* now close the files ... *)
	val _ = app (fn{outstream,...}=>TextIO.closeOut outstream) matched
    in
	()
    end


end
