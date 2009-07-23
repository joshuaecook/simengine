(*
 *  paths.sml  --  finding all simple paths
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   functor defined:

    Paths    finding simple paths
 *)

functor Paths (G:GRAPH) =
struct
  local
    fun conspaths (v,l) = map (fn p=>v::p) l
  
    fun from (v,g) = 
        let val ((lab,s),g') = G.matchFwd (v,g)
         in 
            conspaths (v,pathsfrom (map UTuple.p2 s,G.embed (([],v,lab,[]),g'))) 
        end
 
    and pathsfrom ([],g)   = [[]]
     |  pathsfrom (v::l,g) = from (v,g) @ pathsfrom (l,g)
  in
    fun paths g = pathsfrom (G.nodes g,g)
  end (* local *)
end (* Paths *)

   

(* example graphs and test runs *)

(*
structure G = StaticGraphArrayFwd(CArray);
structure P = Paths(G);

val l = String.explode "abc";
val g = G.mkgr (l,[(0,1,()),(1,2,()),(2,0,())]);
val t = P.paths g;

val l = String.explode "yzstxwvu";
val g = G.mkgr (l,[(0,4,()),(1,2,()),(2,1,()),(2,5,()),(3,6,()),(3,7,()),
                   (4,1,()),(5,4,()),(6,2,()),(6,5,()),(7,3,()),(7,6,())]);
val t = P.paths g;
*)
