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

signature JSON_PARSE_STRUCTS = sig
    structure JS: JSON

    structure Token: sig
	datatype token = EOF | COMMA | COLON
		       | NULL | TRUE | FALSE
		       | LARRAY | RARRAY 
		       | LOBJECT | ROBJECT
		       | INT of IntInf.int
		       | REAL of LargeReal.real
		       | STRING of string
	val toString: token -> string
    end

    structure Lex: sig
	val makeLexer: (int -> string) -> (unit -> Token.token)
    end
end

signature JSON_PARSE = sig
    include JSON_PARSE_STRUCTS

    val parse: TextIO.instream -> JS.json
    val parseFile: string -> JS.json
    val parseString: string -> JS.json
end
