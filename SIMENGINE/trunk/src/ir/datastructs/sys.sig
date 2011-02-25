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

structure System =
struct

(*type outmask = string

datatype dynamo_type = 
	 CONSTANT
       | STATE
       | PARAMETER
       | DERIVED_PARAMETER
       | UNKNOWN
       | INPUT
       | INPUT_AVAIL of string

(* type plforest = *)
(*      {param_init: ProgrammersLambda.exp, *)
(*       state_init: ProgrammersLambda.exp, *)
(*       run: ProgrammersLambda.exp, *)
(*       runarg: ProgrammersLambda.exp, *)
(*       ranges: ProgrammersLambda.exp, *)
(*       types: ProgrammersLambda.exp, *)
(*       inputs: string list, *)
(*       outputs: outmask list, *)
(*       samplerate: real option} *)
     
type treeforest =
     {params: ExpTree.stm list,
      states_init: ExpTree.stm list, 
      states_run: ExpTree.stm list,
      ranges: (string * ExpTree.range) list,
      typetable: string -> dynamo_type,
      inputs: string list,
      outputs: {name:string, condition:ExpTree.exp option} list,
      samplerate: real option}

type range = ExpTree.range
type table_info = {table2range : string -> range,
		   table2depth : string -> real}

type expgraph = ExpGraph.graph
type ptegraph = PTEGraph.graph

type dfgs = 
     {lutg: expgraph,
      paramg: ptegraph,
      sinitg: ptegraph,
      table_info: table_info,
      srung: ptegraph,
      srung_noluts: ptegraph}

type simulationDataFlow =
     {inputs: string list,
      outputs: {name:string, condition:string option} list,
      ranges: (string * ExpTree.range) list,
      typetable: string -> dynamo_type,
      samplerate: real option,
      dfgs: dfgs}
*)
datatype status =
	 SUCCESS 
       | FAILURE of string

end

(* signature FE = *)
(* sig *)
(*     type plforest = System.plforest *)

(*     val parser : (string * real) list -> string -> string -> plforest *)
(* end *)

(* signature ME = *)
(* sig *)
(*     type plforest = System.plforest *)
(*     type treeforest = System.treeforest *)
(*     type library = DynamoLibrary.library *)

(*     val plf2tf : int -> library -> plforest -> treeforest *)
(* end *)

(*signature OPTIMIZE =
sig
    type treeforest = System.treeforest
    type sdf = System.simulationDataFlow

    val optimize : string * treeforest -> sdf
end

signature CODEGEN =
sig
    type status = System.status
    type sdf = System.simulationDataFlow

    val generate : string * sdf -> status
end
*)
