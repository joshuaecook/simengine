(* 
 *  gr.sml - definitions that are shared among different graph implementations
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structures and functors defined:

    GraphNode          defines type of graph nodes
    GraphExceptions    defines graph exceptions
    GraphTypes         functor for deriving types from graph implementation
    UnlabGraphTypes    functor for deriving types from graph implementation
    StampUtil          time stamping utilities
    SortEdges          utilities to sort successors/predecessors) by edge labels
 *)

structure GraphNode : GRAPH_NODE =
struct
  type node = int
end


structure GraphExceptions : GRAPH_EXCEPTIONS =
struct
  exception Node
  exception Edge
  exception NotImplemented
end



(* functors deriving types from graph implementations *)

functor UnlabGraphTypes (type 'a graph) : UNLAB_GRAPH_TYPES =
struct
  type node          = GraphNode.node
  type 'a graph      = 'a graph
  type 'a adj        = 'a * node list
  type 'a context    = node list * node * 'a * node list
  type 'a decomp     = 'a context * 'a graph
  type 'a fwd_decomp = 'a adj * 'a graph
end


functor GraphTypes (type ('a,'b) graph) : GRAPH_TYPES =
struct
  type node               = GraphNode.node
  type ('a,'b) graph      = ('a,'b) graph
  type     'b out         = ('b * node) list
  type ('a,'b) adj        = 'a * 'b out
  type ('a,'b) context    = 'b out * node * 'a * 'b out
  type ('a,'b) decomp     = ('a,'b) context * ('a,'b) graph
  type ('a,'b) fwd_decomp = ('a,'b) adj * ('a,'b) graph
end


(* Time stamping utilities for nodes:

     Stamps are integers whose absolute value is strictly increasing. 
     Zero or negative values represent deleted nodes.

     stampTrue   gets next stamp and makes it positive
     stampFalse  gets next stamp and makes it negative
     getStamp    simply obtains a node's stamp
     getNegStamp gets a node's negative stamp, ie, the calling
                 function insists that the node is not existing.
     getPosStamp gets a node's positive stamp, ie, the calling
                 function insists of the node's existence.
     stampList   extends a list of nodes by their current stamps
     lstampList  extends a list of labeled nodes by their current stamps
*)

signature STAMP_UTIL =
sig
  include GRAPH_NODE
  type 'a array
  type stamp = int
  type stamp_adj        = (node * stamp) list array
  type 'a stamp_lab_adj = (('a * node) * stamp) list array
  val stampTrue   : stamp -> stamp
  val stampFalse  : stamp -> stamp
  val getStamp    : stamp array * node -> stamp
  val getNegStamp : stamp array * node -> stamp
  val getPosStamp : stamp array * node -> stamp  
  val stampList   : stamp array * node list -> (node * stamp) list
  val lstampList  : stamp array * ('a * node) list -> (('a * node) * stamp) list
  val valid       : stamp array * stamp_adj * node -> node list
  val lvalid      : stamp array * 'a stamp_lab_adj * node -> ('a * node) list
(*
  val sreset      : unit -> unit
  val sstat       : unit -> unit
*)
end


functor StampUtil (FunArray:FUN_ARRAY) : STAMP_UTIL =
struct
  local
    open GraphNode GraphExceptions
    open FunArray  
(*
    only used for profiling:
    val (selected,skipped) = (ref 0,ref 0)
*)
  in
    type node  = int
    type stamp = int
    type 'a array = 'a FunArray.array
    type stamp_adj = (node * stamp) list array
    type 'a stamp_lab_adj = (('a * node) * stamp) list array

    fun stampTrue  i = abs i+1
    fun stampFalse i = ~(abs i+1)
    fun getStamp (na,n)    = sub (na,n)
    fun getNegStamp (na,n) = let val s=sub (na,n)
                              in if s>0 then raise Node else s end
    fun getPosStamp (na,n) = let val s=sub (na,n) 
                              in if s<=0 then raise Edge else s end
    fun stampList (na,l) = map (fn x=>(x,getPosStamp (na,x))) l
    fun lstampList (na,l) = map (fn (lab,x)=>((lab,x),getPosStamp (na,x))) l
    fun valid (s,a,n) = 
        UList.select UTuple.p1 (fn (v,i)=>i=getStamp (s,v)) (sub (a,n))
    fun lvalid (s,a,n) = 
        UList.select UTuple.p1 (fn ((_,v),i)=>i=getStamp (s,v)) (sub (a,n))
(*
 * profiling: off
 *
    fun lvalid (s,a,n) = 
        let val all = sub (a,n)
            val sel = UList.select UTuple.p1 (fn ((_,v),i)=>i=getStamp (s,v)) all
            val nsel = length sel
            val _ = (selected := !selected + nsel;
                     skipped := !skipped + length all - nsel)
         in sel end
    fun sreset () = (selected := 0;skipped := 0)
    fun sstat ()  = (print ("Selected: "^Int.toString (!selected)^
                           ", Skipped: "^Int.toString (!skipped)^
                           ". Ration: "^Int.toString 
                 (Int.div (Int.max (!selected,!skipped),
                           Int.min (!selected,!skipped)))))
 *)
  end (* local *)
end (* functor StampUtil *)


structure SortEdges =
struct
  (* 
     labsort    is used by "matchOrd" and "matchOrdFwd" to sort
                successors (and predecessors) by edge labels 
   *)
  local  
    fun labsel (x,(lab,v)::nl) = 
          if x=lab then ((lab,v),nl) else 
             UTuple.P2 (UList.cons (lab,v)) (labsel (x,nl))
     |  labsel _ = raise Match
  in
    fun labsort ([],nl)   = nl
     |  labsort (l,[])    = []
     |  labsort (x::l,nl) = 
          let val (t,rest) = labsel (x,nl) in t::labsort (l,rest) end
  end (* local *)
end (* structure SortEdges *)
