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

val l2s = Util.l2s;
val log = Util.log;

val _ = log ""
val _ = ExpProcess.log_exps ("FN Model (exps)", TestModels.fnmodel_exps)
(*val _ = log ""
val _ = EqUtil.log_eqs ("FN Model (eqs)", TestModels.fnmodel_eqs)*)
val _ = log ""
val _ = log "Symbols:"
val _ = app log (map (l2s o ExpProcess.exp2symbol_names) TestModels.fnmodel)
val _ = log ""
val _ = log "Operations:"
val _ = app log (map (l2s o ExpProcess.exp2fun_names) TestModels.fnmodel)

val _ = log ""
val _ = log (ExpProcess.exp2str (Exp.FUN(Symbol.symbol "EQUALS", [Exp.TERM (Exp.PATTERN (Symbol.symbol "a", ("ANY", fn(x)=>true), Pattern.ONE)),Exp.TERM (Exp.PATTERN (Symbol.symbol "b", ("ANY", fn(x)=>true), Pattern.ONE))])))

val _ = CWriterUtil.log_c_exps ("FN Model (C)", TestModels.fnmodel_exps)
val _ = CWriterUtil.log_c_eqs ("FN Model (C)", TestModels.fnmodel_eqs)

val _ = log ""
val _ = DOFPrinter.printModel TestModels.fn_model

val _ = CurrentModel.setCurrentModel TestModels.fn_pop_model
val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())

val _ = EqUtil.order_eqs (!(#eqs TestModels.fn_pop_class))
val _ = ModelProcess.normalizeModel (CurrentModel.getCurrentModel())

(*val _ = CWriter.buildC TestModels.fn_model*)
val _ = CWriter.buildC (CurrentModel.getCurrentModel())
val _ = MexWriter.buildMex (CurrentModel.getCurrentModel())

