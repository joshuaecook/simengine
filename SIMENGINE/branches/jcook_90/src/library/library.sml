structure Library =
struct

fun error msg =
    Logger.log_error (Printer.$ msg)

val core_library : {name: string, operation: (KEC.exp -> KEC.exp) -> KEC.exp list -> KEC.exp} list = 
    SystemLib.library @
    TimeLib.library @
    ArithmeticLib.library @
    ComparisonLib.library @
    TrigonometryLib.library @
    StringLib.library @
    FileLib.library @
    FileSysLib.library @
    FunctionLib.library @
    PathLib.library @
    TypeLib.library @
    BooleanLib.library @
    VectorLib.library @
    SettingsLib.library @
    MalleabilityLib.library @
    (*PrecisionLib.library @*)
    EnvironmentLib.library @
    ProcessLib.library @
    DevicesLib.library @
    JSONLib.library @
    ArchiveLib.library @
    RegExpLib.library @
    CompilerLib.library

structure LibraryMap = BinaryMapFn (struct
				    type ord_key = Symbol.symbol
				    val compare = Symbol.compare
				    end)

fun buildLibrary ({name, operation}, map) =
    LibraryMap.insert (map, Symbol.symbol name, operation)

val libmap = foldl buildLibrary LibraryMap.empty core_library

exception StubbedOut



fun exec exec name (args: KEC.exp list) =
    let
(*	val operation = List.find (fn({name=n, ...}) => n = name) core_library
	val {name, operation}*)
	val operation = case LibraryMap.find(libmap, name) of
			    SOME operation => operation
			  | NONE => raise StubbedOut before error ("UNKNOWN LIB FUN: " ^ (Symbol.name name)) (*TODO: make a reasonable error msg here *)
    in
	operation exec args
    end
	

end
