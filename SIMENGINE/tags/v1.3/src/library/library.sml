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

structure Library =
struct

fun error msg =
    (Logger.log_error (Printer.$ msg);
     raise DynException.RestartRepl)

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
    LicensingLib.library @
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
			  | NONE => error ("UNKNOWN LIB FUN: " ^ (Symbol.name name)) (*TODO: make a reasonable error msg here *)
    in
	operation exec args
    end
	

end
