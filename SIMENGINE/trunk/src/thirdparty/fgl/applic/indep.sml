(*
 *  indep.sml  --  computing maximal independent node sets
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   functor defined:

    Indep    compute independent node sets of maximum size
 *)

   
functor Indep (G:GRAPH)=
struct
  local 
    fun deg g v = length (G.suc (v,g))

    fun delNodes ([],g) = g
     |  delNodes (x::l,g) = delNodes (l,UTuple.p2 (G.match (x,g)))
        handle Match => delNodes (l,g)
  in 
    fun indep g = if G.isEmpty g then [] else
        let val n = UList.thatOne (Int.max,deg g) (G.nodes g)
            val ((p,_,_,l),g1) = G.match (n,g)
            val i1 = indep g1
            val i2 = n::indep (delNodes (map UTuple.p2 (l@p),g1))
         in 
            if length i1>length i2 then i1 else i2
        end
  end (* local *)
end (* Indep *)
