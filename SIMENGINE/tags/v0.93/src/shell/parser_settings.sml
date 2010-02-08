structure ParserSettings =
struct

val isConsole = ref false
val filename = ref ""
val filepath = ref ""
val lineCount = ref 1
val lastLinePos = ref 0

fun getSettings () =
    (!isConsole, !filename, !filepath, !lineCount, !lastLinePos)

fun setSettings (cons, name, path) =
    (isConsole := cons;
     filename := name;
     filepath := path;
     lineCount := 1;
     lastLinePos := 0)

fun restoreSettings (cons, name, path, lines, llpos) =
    (isConsole := cons;
     filename := name;
     filepath := path;
     lineCount := lines;
     lastLinePos := llpos)
    

fun nextLine () = 
    lineCount := 1 + !lineCount

end
