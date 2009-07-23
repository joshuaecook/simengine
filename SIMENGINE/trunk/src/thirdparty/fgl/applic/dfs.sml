(*
 *  dfs.sml  --  depth first search related functions:
 *               * building depth first spanning trees
 *               * computing postorder node sequence/topological sorting
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structures and functors defined:
 
    DFSTree   defines a tree data type to represent dfs-trees
    DFS       dfs for labeled graphs 
    DFSu      dfs for unlabeled graphs 
 *) 

structure DFSTree =
struct
  datatype tree = BRANCH of int * tree list
end


functor DFS (G:GRAPH) =
struct
  open DFSTree

  fun dfs1 (v,g) = 
      let val ((_,s),g1) = G.matchFwd (v,g)
          val (f,g2) = dfsn (map UTuple.p2 s,g1)
       in
          (BRANCH (v,f),g2)
      end

  and dfsn ([],g)   = ([],g)
   |  dfsn (v::l,g) =
      let val (t,g1) = dfs1 (v,g)
          val (f,g2) = dfsn (l,g1)
       in
          (t::f,g2)
      end
      handle Match => dfsn (l,g)

  fun dfs g = #1 (dfsn (G.nodes g,g))
end (* DFS *)


functor DFSu (G:UNLAB_GRAPH) =
struct
  open DFSTree

  fun dfs1 (v,g) = 
      let val ((_,s),g1) = G.matchFwd (v,g)
          val (f,g2) = dfsn (s,g1)
       in
          (BRANCH (v,f),g2)
      end

  and dfsn ([],g)   = ([],g)
   |  dfsn (v::l,g) =
      let val (t,g1) = dfs1 (v,g)
          val (f,g2) = dfsn (l,g1)
       in
          (t::f,g2)
      end
      handle Match => dfsn (l,g)

  fun dfs g = #1 (dfsn (G.nodes g,g))
end (* DFSu *)
   