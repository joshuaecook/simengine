signature FILEENTRY = 
sig
type fileentry

val makeFileEntry :  Symbol.symbol * Symbol.symbol -> fileentry    
val tostring : fileentry -> string
val file : fileentry -> Symbol.symbol
val entry : fileentry -> Symbol.symbol

end

structure FileEntry : FILEENTRY =
struct
type fileentry = {file: Symbol.symbol, entry: Symbol.symbol}

fun file {file=f, entry} = f
fun entry {entry=e, file} = e

fun makeFileEntry (name, entry) =
    {file=name,
     entry=entry}

fun tostring {file, entry} =
    "&(" ^ (Symbol.name file) ^ " : " ^ (Symbol.name entry) ^ ")"
end

