(*
 *  gr_ab.sml  --  bounded (semi-dynamic) graph implementation based 
 *                 on functional arrays
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   functor defined:

    BoundedGraphArray         labeled graphs, semi-dynamic array implementation
 *)

 
functor BoundedGraphArray (FunArray:FUN_ARRAY) : BOUNDED_GRAPH =
struct
  structure Stamp = StampUtil(FunArray)
  structure P = Partition(FunArray)
  open Stamp FunArray UTuple UList
  open GraphExceptions
  
  datatype ('a,'b) graph = 
     Empty of int
   | Full of P.part (* partition of nodes *) *
             stamp array * 'a array * 'b stamp_lab_adj * 'b stamp_lab_adj  
                                      (* pred *)         (* suc  *)

  structure Types = GraphTypes (struct type ('a,'b) graph=('a,'b) graph end)
  open Types

  (* exported functions *)
 
  val empty        = Empty 50
  fun emptyBound n = Empty n
  
  fun embed ((p,n,l,s),Empty i) = embed ((p,n,l,s),
            Full (P.empty i,array (i,0),array (i,l),array (i,[]),array (i,[])))
   |  embed ((p,n,l,s),Full (part,na,la,pa,sa)) = 
      let val stampN = stampTrue (getNegStamp (na,n))
          val stampedPred = lstampList (na,p)
          val stampedSuc  = lstampList (na,s)
          fun updAdj (a,[])       = a
           |  updAdj (a,(lab,v)::l) =
              updAdj (apply (a,v,fn adj=>((lab,n),stampN)::adj),l)
       in
          Full (P.insert (n,part),
                update (na,n,stampN),
                update (la,n,l),
                updAdj (update (pa,n,stampedPred),s),
                updAdj (update (sa,n,stampedSuc),p))
      end

  fun match (_,Empty _) = raise Match
   |  match (n,Full (part,na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((lvalid (na,pa,n),n,sub (la,n),lvalid (na,sa,n)),
          Full (P.delete (n,part),apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchFwd (n,Empty _) = raise Match
   |  matchFwd (n,Full (part,na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((sub (la,n),lvalid (na,sa,n)),
          Full (P.delete (n,part),apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchAny (Empty _) = raise Match
   |  matchAny (g as (Full (part,_,_,_,_))) = 
               (match (P.any part,g) handle P.Empty => raise Match)

  fun matchAnyFwd (Empty _) = raise Match
   |  matchAnyFwd (g as (Full (part,na,_,_,_))) = 
             (matchFwd (P.any part,g) handle P.Empty => raise Match)
       
  fun matchOrd (n,l,l',g) =
      let val ((p,_,lab,s),g') = match (n,g)
       in ((SortEdges.labsort (l,p),n,lab,SortEdges.labsort (l',s)),g') end
       
  fun matchOrdFwd (n,l,g) =
      let val ((lab,s),g') = matchFwd (n,g) 
       in ((lab,SortEdges.labsort (l,s)),g') end
       
  fun context (_,Empty _) = raise Match
   |  context (n,Full (_,na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         (lvalid (na,pa,n),n,sub (la,n),lvalid (na,sa,n))
      else raise Match

  fun fwd (n,Empty _) = raise Match
   |  fwd (n,Full (_,na,la,_,sa)) = 
      if getStamp (na,n)>0 then (sub (la,n),lvalid (na,sa,n)) else raise Match

  fun bwd (n,Empty _) = raise Match
   |  bwd (n,Full (_,na,la,pa,_)) = 
      if getStamp (na,n)>0 then (sub (la,n),lvalid (na,pa,n)) else raise Match

  fun suc (n,Empty _) = raise Match
   |  suc (n,Full (_,na,la,_,sa)) = 
      if getStamp (na,n)>0 then map p2 (lvalid (na,sa,n)) else raise Match

  fun pred (n,Empty _) = raise Match
   |  pred (n,Full (_,na,la,pa,_)) = 
      if getStamp (na,n)>0 then map p2 (lvalid (na,pa,n)) else raise Match

  fun ufold f u (Empty _) = u
   |  ufold f u (Full (_,na,la,pa,sa)) =
      let val V = toImpArray na
          val n = Array.length V
          fun lvalid (a,x) = select p1 (fn ((_,v),i)=>Array.sub (V,v)=i) (sub (a,x))
          fun ufoldi x = 
	          if x<n then
                 let val c = (lvalid (pa,x),x,sub (la,x),lvalid (sa,x)) 
		             val _ = Array.update (V,x,~1)
		             val r = ufoldi (x+1)
		          in f (c,r) end
	          else u
	in ufoldi 0 end
      
  fun gfold f d b u l (Empty _) = u
   |  gfold f d b u l (Full (_,na,la,pa,sa)) =
      let val V = toImpArray na
          fun lvalid (a,x) = select p1 (fn ((_,v),i)=>Array.sub (V,v)=i) (sub (a,x))
          fun gfold1 v = (Array.update (V,v,~1);
                          let val l=sub (la,v)
                           in 
                              d (l,gfoldn (f (lvalid (pa,v),v,l,lvalid (sa,v))))
                          end)
          and gfoldn []     = u
           |  gfoldn (v::l) = if Array.sub (V,v)<0 then gfoldn l
                                                   else b (gfold1 v,gfoldn l)
        in
           gfoldn l
       end

  fun nodes (Empty _)             = []
   |  nodes (Full (part,_,_,_,_)) = P.members part
   
  fun labNodes (Empty _)                = []
   |  labNodes (g as Full (_,_,la,_,_)) = map (fn v=>(v,sub (la,v))) (nodes g)

  fun noNodes (Empty _)             = 0
   |  noNodes (Full (part,_,_,_,_)) = P.noElems part

  fun isEmpty g = noNodes g=0

  fun newNodes i (Empty _)             = UGeneral.to (0,i-1)
   |  newNodes i (Full (part,_,_,_,_)) = P.new (i,part) 
  
  fun mkgr (nl,el:(node * node * 'b) list) =
      let fun maxNode []           = 0
           |  maxNode ((v,w,_)::l) = Int.max (Int.max (v,w),maxNode l)
          val i = maxNode el
          val n = i+1
          val N = Array.array (n,0)
          val L = Array.array (n,hd nl)
          val P = Array.array (n,[]:(('b * node) * stamp) list)
          val S = Array.array (n,[]:(('b * node) * stamp) list)
          fun setnlab (_,[])   = ()
           |  setnlab (i,x::l) = (Array.update (L,i,x);setnlab (i+1,l))
          fun scan []          = ()
           |  scan ((v,w,l)::el) = 
              (Array.update (N,v,1);Array.update (N,w,1);
               Array.update (P,w,((l,v),1)::Array.sub (P,w));
               Array.update (S,v,((l,w),1)::Array.sub (S,v));
              scan el)
       in
          (setnlab (1,tl nl); scan el;
           Full (P.initial (i+1,n),
                 fromImpArray N,fromImpArray L,fromImpArray P,fromImpArray S))
      end

  fun adj (Empty _) = []
   |  adj (g as Full (_,na,la,_,sa)) = 
      let fun adj' n = (n,(sub (la,n),lvalid (na,sa,n)))
       in map adj' (nodes g) end
  
(*  
  fun reset () = init ()
  fun report () = stat ()
*)
end (* functor BoundedArrayGraph *)
