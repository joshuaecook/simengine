structure LibraryUtil =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

fun error msg =
    Logger.log_error (Printer.$ msg)

(* Many of the numeric functions have the same (real -> real) signature with a simple operation.
   This is a helper function for creating library methods of that type. *)
fun realfun (f: (real -> real)) = 
 fn [KEC.LITERAL (KEC.CONSTREAL r)] 
    => KEC.LITERAL (KEC.CONSTREAL (f r))
  | [a] 
    => raise TypeMismatch ("expected a number but received " ^ (PrettyPrint.kecexp2nickname a))
  | args => raise IncorrectNumberOfArguments {expected=1, actual=(length args)}

(* As above, but with the signature (real * real -> real) *)
fun realsfun (f: (real * real -> real)) =
 fn [KEC.LITERAL (KEC.CONSTREAL r1), KEC.LITERAL (KEC.CONSTREAL r2)] 
    => KEC.LITERAL (KEC.CONSTREAL (f (r1,r2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 numbers but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, but with the signature (real * real -> bool) *)
fun reals2boolfun (f: (real * real -> bool)) = 
 fn [KEC.LITERAL (KEC.CONSTREAL r1), KEC.LITERAL (KEC.CONSTREAL r2)] 
    => KEC.LITERAL (KEC.CONSTBOOL (f (r1,r2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 numbers but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string * string -> string) *)
fun strsfun (f: (string * string -> string)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s1), KEC.LITERAL (KEC.CONSTSTR s2)] 
    => KEC.LITERAL (KEC.CONSTSTR (f (s1, s2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}

(* As above, with with the signature (string * string -> bool) *)
fun strs2boolfun (f: (string * string -> bool)) =
 fn [KEC.LITERAL (KEC.CONSTSTR s1), KEC.LITERAL (KEC.CONSTSTR s2)] 
    => KEC.LITERAL (KEC.CONSTBOOL (f (s1, s2)))
  | [a, b] 
    => raise TypeMismatch ("expected 2 strings but received " ^ (PrettyPrint.kecexp2nickname a) ^ " and " ^ (PrettyPrint.kecexp2nickname b))
  | args => raise IncorrectNumberOfArguments {expected=2, actual=(length args)}


end
