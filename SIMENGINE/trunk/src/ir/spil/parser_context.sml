signature PARSER_CONTEXT = sig
    type point
    type context

    val point: int -> point
    val printPoint: point -> string

    val lines: unit -> int
    val newline: int -> unit
    val point_at_bol: unit -> point

    val column: point -> int

    val reset: unit -> unit

    val file: string -> context
    val stdin: context
    val filename: context -> string
end

functor ParserContext (): PARSER_CONTEXT = struct
type point = int
datatype context = File of string | Stdin

fun point x = x
fun printPoint x = Int.toString x

fun file name = File name
val stdin = Stdin
val filename = fn File name => name | Stdin => "stdin"

val ref_lines = ref 0
val ref_point_at_bol = ref 0

fun lines () = ! ref_lines
fun point_at_bol () = ! ref_point_at_bol
fun newline point = (ref_lines := 1 + (! ref_lines); ref_point_at_bol := point)

fun column point = point - (! ref_point_at_bol)

fun reset () = (ref_lines := 0; ref_point_at_bol := 0)
end
