(*
 *  rev.sml  --  reversing edges in a directed (un)labeled graph
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)

(*
   functors defined:
   
    REV   reverse for edge-labeled graphs 
    REVu  reverse for edge-unlabeled graphs 
    REVd  direct method for (static) array implementation
 *)
   
functor REV (G:GRAPH) =
struct
  fun rev g = if G.isEmpty g then g else 
      let val ((p,v,l,s),g') = G.matchAny g
       in G.embed ((s,v,l,p), rev g') end
end (* REV *)


functor REVu (G:UNLAB_GRAPH) =
struct
  fun rev g = if G.isEmpty g then g else 
      let val ((p,v,l,s),g') = G.matchAny g
       in G.embed ((s,v,l,p), rev g') end
end (* REVu *)


functor REVd (G:GRAPH) =
struct
  local
    fun revd ([],g)   = g
     |  revd (v::l,g) = 
        let val ((p,_,lab,s),g') = G.match (v,g)
         in G.embed ((s,v,lab,p), revd (l,g')) end
        handle Match => g
  in     
    fun rev g = revd (G.nodes g,g)
  end (* local *)
end (* REVd *)



