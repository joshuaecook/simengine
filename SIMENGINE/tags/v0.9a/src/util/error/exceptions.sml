(* Dynamo Exceptions library
 * 
 *   This module provides support for standardized exception raising, handling, and logging.
 *   See the wiki for more information.
 *)

structure DynException =
struct

exception InternalError of {message: string,
			    severity: Logger.message_type,
			    characterization: Logger.characterization,
			    location: string}
exception TooManyErrors

(* Raised when an error occurs within an action and the repl should restart. *)
exception RestartRepl

(* Raised when an operation or function is applied to an object of an inappropriate type.
   The associated string is a message giving details about the type mismatch. *)
exception TypeMismatch of string

(* Raised when an operation or function is applied to an object of the correct type but an inappropriate value.
   The associated string is a message giving details about the value mismatch. *)
exception ValueError of string

(* Raised when an operation or function is applied to too many or too few arguments.
   The associated record comprises the expected number of arguments and 
   the actual number of received arguments. *)
exception IncorrectNumberOfArguments of {expected: int, actual: int}

(* Raised when an unknown identifier is encountered.
   The associated string is the symbolic name. *)
exception NameError of string

(* Raised when import cannot find a file.
   The associated string is the file name.
   The associated list of strings contains the search paths. *)
exception ImportError of string * string list



val showStackTrace = ref false

fun setShowStackTrace(bool) =
    showStackTrace := bool

open Printer

fun log_stack e () =
    if (!showStackTrace) then
	[$"  Exception stack trace:",
	 (SUB (map (fn(s) => $s) (MLton.Exn.history e)))]
    else
	[]
				  
fun log handlelocation (e as InternalError {message, severity, characterization, location}) =
    (Logger.log characterization severity 
		(Printer.$("Exception caught at " ^ handlelocation 
			   ^ " and raised at " ^ location ^ " - (" ^ message ^ ")"));
     Logger.log_information (log_stack e) Logger.NOGROUP)
  | log handlelocation (e as TooManyErrors) =
    ()
  | log handlelocation e = 
    (Logger.log Logger.OTHER Logger.FAILURE 
		($("Unknown exception caught at " ^ handlelocation));
     Logger.log_information (log_stack e) Logger.NOGROUP)

fun checkpoint handlelocation e =
    (log handlelocation e;
     raise e)


fun stdException (message, location, characterization) =
    raise InternalError {message=message,
			 severity=Logger.ERROR,
			 characterization=characterization,
			 location=location}

val terminal_errors = ref false

fun setErrored() =
    terminal_errors := true

fun resetErrored() =
    terminal_errors := false

fun checkToProceed() =
    if (!terminal_errors) then
	raise TooManyErrors
    else
	()

fun isErrored() = !terminal_errors

fun assert flag message =
    if not flag then
	(Logger.log Logger.ASSERTION Logger.FAILURE message;
	 setErrored())
    else
	()
		

end
