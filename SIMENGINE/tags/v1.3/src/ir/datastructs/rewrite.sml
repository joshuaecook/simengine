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

structure Rewrite =
struct

type patterns_matched = Exp.exp SymbolTable.table

type assigned_pattern_type = Exp.exp SymbolTable.table
type test_type = ((Exp.exp * assigned_pattern_type) -> bool) option

datatype rewrite_type = RULE of Exp.exp (* from and to *)
		     | ACTION of (Symbol.symbol * (Exp.exp -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)
		     | MATCHEDACTION of (string * (Exp.exp * patterns_matched -> Exp.exp)) (* when finding the first expression, process it through the supplied function *)
type rewrite = {find:Exp.exp, test:test_type, replace:rewrite_type}

val e2s = ExpPrinter.exp2str
fun rewrite2str {find,test,replace} = 
    (e2s find) ^ " -> " ^ (case replace of 
			       RULE exp => e2s exp
			     | ACTION (sym,_) => "ACTION:"^(Symbol.name sym)
			     | MATCHEDACTION (str,_) => "MATCHEDACTION:"^(str))

end
