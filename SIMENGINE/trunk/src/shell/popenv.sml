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

structure PopulatedEnv =
struct

datatype def = DIRECTDEF of KEC.stm
	     | CODEDEF of string

fun defglobal name definition =
    DIRECTDEF (KEC.DEFINITION (KEC.DEFGLOBAL (KEC.REPLACE, Symbol.symbol name, KEC.DONTCARE, definition), 
			       PosLog.NOPOS))
        
fun code code =
    CODEDEF ((String.concatWith "\n" code) ^ "\n")

fun import filename =
    DIRECTDEF (KEC.ACTION (KEC.IMPORT filename, PosLog.NOPOS))

local
    fun process_def parse (DIRECTDEF stm, env) =
	let
	    val _ = Globals.eof_encountered := false
	    val (env, _) =
		Exec.run parse env [stm]
	in
	    env
	end
      | process_def parse (CODEDEF code, env) =
	let 
	    val _ = Globals.eof_encountered := false
	    val (_, env) = parse (TextIO.openString code) env
	in
	    env
	end
in

fun new (parse) =
    let
	val initial_defs =
	    defglobal "Object" (Objects.classdef "Object" NONE
						 [(KEC.REPLACE, 
						   Objects.method "tostring" nil
								  (Objects.send "name" (KEC.SYMBOL (Symbol.symbol "class")))),
						  (KEC.REPLACE,
						   Objects.method "()" [(Symbol.symbol "table", KEC.TYPE (Symbol.symbol "Table"))]
								  (KEC.APPLY {func=KEC.SYMBOL(Symbol.symbol "initObjWithTable"),
									      args=KEC.TUPLE [KEC.SYMBOL(Symbol.symbol "self"),
											      KEC.SYMBOL(Symbol.symbol "table")]}))]) :: 
	    (map import ["stdlib.dsl", "simulation.dsl", "rules.dsl", "dependency.dsl", "devices.dsl", "startup.dsl"])

	    
	val env = foldl (process_def parse) (ref (Env.new()), (Env.new(), Env.new(), ref NONE), PosLog.new()) initial_defs
    in
	env
	before Globals.base_env := env
	before Globals.eof_encountered := false
	before Globals.core_init := false
    end

fun importSettings parse env =
    let
	val defs = map (fn(stm) => DIRECTDEF stm) (ShellOptions.getOptionsInit())
	val env = foldl (process_def parse) env defs

    in
	env
	before Globals.base_env := env
	before Globals.eof_encountered := false
	before Globals.core_init := false
    end

end

end
