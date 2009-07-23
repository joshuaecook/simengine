(*
 *  clique.sml  --  generating complete graphs
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structure defined:
 
    Clique  generate cliques of unlabeled graphs
 *)

structure Clique =
struct
  (*
     clique   based on tree implementation
     cliqueA  based on array implementation
              This is possible, since the argument of clique can be 
              well used as the upper bound on the graph size
  *)
  local 
    structure T = Graph;
    structure A = BoundedGraphArray(CArray);
    fun nolab l = map (fn x=>((),x)) l
    fun clique' (n,0) = A.emptyBound n
     |  clique' (n,i) = 
        let val g   = clique' (n,i-1)
            val l   = nolab (A.nodes g)
            val [v] = A.newNodes 1 g
         in A.embed ((l,v,(),l),g) end
  in
    fun clique 0 = T.empty
     |  clique n = 
        let val g   = clique (n-1)
            val l   = nolab (T.nodes g)
            val [v] = T.newNodes 1 g
         in T.embed ((l,v,(),l),g) end
    fun cliqueA n = clique' (n,n)
  end (* local *)
end (* Clique *)
