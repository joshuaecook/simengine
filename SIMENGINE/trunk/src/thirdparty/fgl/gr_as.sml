(*
 *  gr_as.sml  --  static graph implementations based on functional arrays
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structures and functors defined:

    StaticGraphArray,
    UnlabStaticGraphArray:
      A graph is represented by two arrays of adjacency lists storing 
      successors and predecessors. This slows down update operations
      a bit, but speeds up (full) context determination since 
      predecessors are immediately available. 
  
    StaticGraphArrayFwd,
    UnlabStaticGraphArrayFwd:  
      Only successors are stored. This speeds up operations 
      like "matchFwd" and "suc" that do not access the full context
     
   Employed utilities:
     UTuple:
       p1 (x,y) = x
       P2 f (x,y) = (x,f y)
     UList:
       cons x l = x::l
       select f p l = map f (filter p l)
*)


(*
 * NOTE the hack in StaticArrayGraph.mkGr : 
 *      make graph larger than needed to
 *      enable dynamic extensions in combred.sml
 *)
 
functor StaticGraphArray (FunArray:FUN_ARRAY) : STATIC_GRAPH =
struct
  structure Stamp = StampUtil (FunArray)
  open Stamp FunArray UTuple UList
  open GraphNode GraphExceptions
  
  datatype ('a,'b) graph = 
     Empty of int
   | Full of stamp array * 'a array * 'b stamp_lab_adj * 'b stamp_lab_adj  
                                      (* pred *)         (* suc  *)

  structure Types = GraphTypes (struct type ('a,'b) graph=('a,'b) graph end)
  open Types

  (* exported functions *)
 
  (* 
     Actually, empty is of no use in fixed implementations 
     since no new nodes can be created, and thus no nodes 
     can be inserted into a graph.
     Note that emptyN woule also make no sense, and thus we can 
     stay with the general GRAPH interface.
  *)
  val empty    = Empty 1
  
  fun embed ((p,n,l,s),Empty i) = 
      embed ((p,n,l,s),Full (array (i,0),array (i,l),array (i,[]),array (i,[])))
   |  embed ((p,n,l,s),Full (na,la,pa,sa)) = 
      let val stampN = stampTrue (getNegStamp (na,n))
          val stampedPred = lstampList (na,p)
          val stampedSuc  = lstampList (na,s)
          fun updAdj (a,[])       = a
           |  updAdj (a,(lab,v)::l) =
              updAdj (apply (a,v,fn adj=>((lab,n),stampN)::adj),l)
       in
          Full (update (na,n,stampN),
                update (la,n,l),
                updAdj (update (pa,n,stampedPred),s),
                updAdj (update (sa,n,stampedSuc),p))
      end

  fun match (_,Empty _) = raise Match
   |  match (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((lvalid (na,pa,n),n,sub (la,n),lvalid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchFwd (_,Empty _) = raise Match
   |  matchFwd (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((sub (la,n),lvalid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchAny (Empty _) = raise Match
   |  matchAny (g as (Full (na,_,_,_))) = 
               (match (firstIndex (na,fn i=>i>0),g)
                handle Subscript => raise Match)

  fun matchAnyFwd (Empty _) = raise Match
   |  matchAnyFwd (g as (Full (na,_,_,_))) = 
             (matchFwd (firstIndex (na,fn i=>i>0),g)
              handle Subscript => raise Match)
       
  fun matchOrd (n,l,l',g) =
      let val ((p,_,lab,s),g') = match (n,g)
       in ((SortEdges.labsort (l,p),n,lab,SortEdges.labsort (l',s)),g') end
       
  fun matchOrdFwd (n,l,g) =
      let val ((lab,s),g') = matchFwd (n,g) 
       in ((lab,SortEdges.labsort (l,s)),g') end
       
  fun context (_,Empty _) = raise Match
   |  context (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         (lvalid (na,pa,n),n,sub (la,n),lvalid (na,sa,n))
      else raise Match

  fun fwd (n,Empty _) = raise Match
   |  fwd (n,Full (na,la,_,sa)) = 
      if getStamp (na,n)>0 then (sub (la,n),lvalid (na,sa,n)) else raise Match

  fun bwd (n,Empty _) = raise Match
   |  bwd (n,Full (na,la,pa,_)) = 
      if getStamp (na,n)>0 then (sub (la,n),lvalid (na,pa,n)) else raise Match

  fun suc (n,Empty _) = raise Match
   |  suc (n,Full (na,la,_,sa)) = 
      if getStamp (na,n)>0 then map p2 (lvalid (na,sa,n)) else raise Match

  fun pred (n,Empty _) = raise Match
   |  pred (n,Full (na,la,pa,_)) = 
      if getStamp (na,n)>0 then map p2 (lvalid (na,pa,n)) else raise Match

  fun ufold f u (Empty _) = u
   |  ufold f u (Full (na,la,pa,sa)) =
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
   |  gfold f d b u l (Full (na,la,pa,sa)) =
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

  fun nodes (Empty _)         = []
   |  nodes (Full (na,_,_,_)) = toList (fn i=>i>0,p1) na
   
  fun labNodes (Empty _)              = []
   |  labNodes (g as Full (_,la,_,_)) = map (fn v=>(v,sub (la,v))) (nodes g)

  fun noNodes g = length (nodes g)

  fun isEmpty (Empty _)         = true
   |  isEmpty (Full (na,_,_,_)) =
      (fn x=>false) (firstIndex (na,fn i=>i>0)) handle Subscript => true

  fun newNodes _ _ = raise NotImplemented
  
  fun mkgr (nl,el:(node * node * 'b) list) =
      let fun maxNode []         = 0
           |  maxNode ((v,w,_)::l) = Int.max (Int.max (v,w),maxNode l)
          val n = maxNode el+1
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
           Full (fromImpArray N,fromImpArray L,fromImpArray P,fromImpArray S))
      end

  fun adj (Empty _) = []
   |  adj (g as Full (na,la,_,sa)) = 
      let fun adj' n = (n,(sub (la,n),lvalid (na,sa,n)))
       in map adj' (nodes g) end
(*  
  fun reset () = init ()
  fun report () = stat ()
*)
end (* functor StaticGraphArray *)


functor UnlabStaticGraphArray (FunArray:FUN_ARRAY) : UNLAB_STATIC_GRAPH =
struct
  structure Stamp = StampUtil (FunArray)
  open Stamp FunArray UTuple UList
  open GraphNode GraphExceptions
  
  datatype 'a graph = 
     Empty of int
   | Full  of stamp array * 'a array * stamp_adj * stamp_adj
                                       (* pred *)  (* suc *)
                    
  structure Types = UnlabGraphTypes (struct type 'a graph='a graph end)
  open Types

  (* exported functions *)
 
  val empty    = Empty 1
  
  fun embed ((p,n,l,s),Empty i) = 
      embed ((p,n,l,s),Full (array (i,0),array (i,l),array (i,[]),array (i,[])))
   |  embed ((p,n,l,s),Full (na,la,pa,sa)) = 
      let val stampN = stampTrue (getNegStamp (na,n))
          val stampedPred = stampList (na,p)
          val stampedSuc  = stampList (na,s)
          fun updAdj (a,[])   = a
           |  updAdj (a,v::l) = updAdj (apply (a,v,fn adj=>(n,stampN)::adj),l)
       in
          Full (update (na,n,stampN),
                update (la,n,l),
                updAdj (update (pa,n,stampedPred),s),
                updAdj (update (sa,n,stampedSuc),p))
      end

  fun match (n,Empty _) = raise Match
   |  match (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((valid (na,pa,n),n,sub (la,n),valid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchFwd (n,Empty _) = raise Match
   |  matchFwd (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then
         ((sub (la,n),valid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,pa,sa))
      else raise Match

  fun matchAny (Empty _) = raise Match
   |  matchAny (g as (Full (na,_,_,_))) = 
               (match (firstIndex (na,fn i=>i>0),g)
                handle Subscript => raise Match)
       
  fun matchAnyFwd (Empty _) = raise Match
   |  matchAnyFwd (g as (Full (na,_,_,_))) = 
             (matchFwd (firstIndex (na,fn i=>i>0),g)
              handle Subscript => raise Match)

  fun context (n,Empty _) = raise Match
   |  context (n,Full (na,la,pa,sa)) = 
      if getStamp (na,n)>0 then (valid (na,pa,n),n,sub (la,n),valid (na,sa,n))
                           else raise Match

  fun fwd (n,Empty _) = raise Match
   |  fwd (n,Full (na,la,_,sa)) = 
      if getStamp (na,n)>0 then (sub (la,n),valid (na,sa,n)) else raise Match

  fun bwd (n,Empty _) = raise Match
   |  bwd (n,Full (na,la,pa,_)) = 
      if getStamp (na,n)>0 then (sub (la,n),valid (na,pa,n)) else raise Match

  fun suc (n,Empty _) = raise Match
   |  suc (n,Full (na,la,_,sa)) = 
      if getStamp (na,n)>0 then valid (na,sa,n) else raise Match

  fun pred (n,Empty _) = raise Match
   |  pred (n,Full (na,la,pa,_)) = 
      if getStamp (na,n)>0 then valid (na,pa,n) else raise Match

  fun ufold f u (Empty _) = u
   |  ufold f u (Full (na,la,pa,sa)) =
      let val V = toImpArray na
          val n = Array.length V
          fun valid (a,x) = select p1 (fn (v,i)=>Array.sub (V,v)>i) (sub (a,x))
          fun ufoldi x = 
	          if x<n then
                 let val c = (valid (pa,x),x,sub (la,x),valid (sa,x)) 
		             val _ = Array.update (V,x,~1)
		             val r = ufoldi (x+1)
		          in f (c,r) end
	          else u
	in ufoldi 0 end
      
  fun gfold f d b u l (Empty _) = u
   |  gfold f d b u l (Full (na,la,pa,sa)) =
      let val V = toImpArray na
          fun valid (a,x) = select p1 (fn (v,i)=>Array.sub (V,v)=i) (sub (a,x))
          fun gfold1 v = (Array.update (V,v,~1);
                          let val l=sub (la,v)
                           in 
                              d (l,gfoldn (f (valid (pa,v),v,l,valid (sa,v))))
                          end)
          and gfoldn []     = u
           |  gfoldn (v::l) = if Array.sub (V,v)<0 then gfoldn l
                                                   else b (gfold1 v,gfoldn l)
        in
           gfoldn l
       end

  fun nodes (Empty _)         = []
   |  nodes (Full (na,_,_,_)) = toList (fn i=>i>0,p1) na
   
  fun labNodes (Empty _)              = []
   |  labNodes (g as Full (_,la,_,_)) = map (fn v=>(v,sub (la,v))) (nodes g)

  fun noNodes g = length (nodes g)

  fun isEmpty (Empty _)         = true
   |  isEmpty (Full (na,_,_,_)) = 
      (fn x=>false) (firstIndex (na,fn i=>i>0)) handle Subscript => true

  fun newNodes _ _ = raise NotImplemented
  
  fun mkgr (nl,el) =
      let fun maxNode []         = 0
           |  maxNode ((v,w)::l) = Int.max (Int.max (v,w),maxNode l)
          val n = maxNode el+1
          val N = Array.array (n,0)
          val L = Array.fromList nl
          val P = Array.array (n,[]:(node * stamp) list)
          val S = Array.array (n,[]:(node * stamp) list)
          fun scan []          = ()
           |  scan ((v,w)::el) = (Array.update (N,v,1);Array.update (N,w,1);
                                  Array.update (P,w,(v,1)::Array.sub (P,w));
                                  Array.update (S,v,(w,1)::Array.sub (S,v));
                                  scan el)
       in
          (scan el;
           Full (fromImpArray N,fromImpArray L,fromImpArray P,fromImpArray S))
      end

  fun adj (Empty _) = []
   |  adj (g as Full (na,la,_,sa)) = 
      let fun adj' n = (n,(sub (la,n),valid (na,sa,n)))
       in map adj' (nodes g) end
(*  
  fun reset () = init ()
  fun report () = stat ()
*)
end (* functor UnlabStaticGraphArray *)


functor StaticGraphArrayFwd (FunArray:FUN_ARRAY) : STATIC_GRAPH =
struct
  structure Stamp = StampUtil (FunArray)
  open Stamp FunArray UTuple UList
  open GraphNode GraphExceptions
  
  datatype ('a,'b) graph = 
     Empty of int
   | Full  of stamp array * 'a array * 'b stamp_lab_adj (* suc *)
   
  structure Types = GraphTypes (struct type ('a,'b) graph=('a,'b) graph end)
  open Types

  (* exported functions *)
 
  val empty    = Empty 1
  
  fun embed ((p,n,l,s),Empty i) = 
      embed ((p,n,l,s),Full (array (i,0),array (i,l),array (i,[])))
   |  embed ((p,n,l,s),Full (na,la,sa)) = 
      let val stampN      = stampTrue (getNegStamp (na,n))
          val stampedSuc  = lstampList (na,s)
          fun updAdj (a,[])         = a
           |  updAdj (a,(lab,v)::l) =
              updAdj (apply (a,v,fn adj=>((lab,n),stampN)::adj),l)
       in
          Full (update (na,n,stampN),update (la,n,l),
                updAdj (update (sa,n,stampedSuc),p))
      end

  fun match    _ = raise NotImplemented
  fun matchAny _ = raise NotImplemented
  fun matchOrd _ = raise NotImplemented
  fun context  _ = raise NotImplemented
  fun bwd      _ = raise NotImplemented
  fun pred     _ = raise NotImplemented

  fun matchFwd (n,Empty _) = raise Match
   |  matchFwd (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then
         ((sub (la,n),lvalid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,sa))
      else
         raise Match

  fun matchAnyFwd (Empty _) = raise Match
   |  matchAnyFwd (g as (Full (na,_,_))) = 
             (matchFwd (firstIndex (na,fn i=>i>0),g)
              handle Subscript => raise Match)
       
  fun matchOrdFwd (n,l,g) =
      let val ((lab,s),g') = matchFwd (n,g) 
       in ((lab,SortEdges.labsort (l,s)),g') end
       
  fun fwd (n,Empty _) = raise Match
   |  fwd (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then (sub (la,n),lvalid (na,sa,n)) else raise Match

  fun suc (n,Empty _) = raise Match
   |  suc (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then map p2 (lvalid (na,sa,n)) else raise Match

  fun ufold _ _ _       = raise NotImplemented
  fun gfold _ _ _ _ _ _ = raise NotImplemented

  fun nodes (Empty _)       = []
   |  nodes (Full (na,_,_)) = toList (fn i=>i>0,p1) na
   
  fun labNodes (Empty _)            = []
   |  labNodes (g as Full (_,la,_)) = map (fn v=>(v,sub (la,v))) (nodes g)

  fun noNodes g = length (nodes g)

  fun isEmpty (Empty _)       = true
   |  isEmpty (Full (na,_,_)) =
      (fn x=>false) (firstIndex (na,fn i=>i>0)) handle Subscript => true   

  fun newNodes _ _ = raise NotImplemented
  
  fun mkgr (nl,el:(node * node * 'b) list) =
      let fun maxNode []         = 0
           |  maxNode ((v,w,_)::l) = Int.max (Int.max (v,w),maxNode l)
          val n = maxNode el+1
          val N = Array.array (n,0)
          val L = Array.fromList nl
          val S = Array.array (n,[]:(('b * node) * stamp) list)
          fun scan []          = ()
           |  scan ((v,w,l)::el) = 
              (Array.update (N,v,1);Array.update (N,w,1);
               Array.update (S,v,((l,w),1)::Array.sub (S,v));
              scan el)
       in
          (scan el;
           Full (fromImpArray N,fromImpArray L,fromImpArray S))
      end
   
  fun adj (Empty _) = []
   |  adj (g as Full (na,la,sa)) = 
      let fun adj' n = (n,(sub (la,n),lvalid (na,sa,n)))
       in map adj' (nodes g) end
  
(*  
  fun reset () = init ()
  fun report () = stat ()
*)
end (* functor StaticGraphArrayFwd *)


functor UnlabStaticGraphArrayFwd (FunArray:FUN_ARRAY) : UNLAB_STATIC_GRAPH =
struct
  structure Stamp = StampUtil (FunArray)
  open Stamp FunArray UTuple UList
  open GraphNode GraphExceptions
  
  datatype 'a graph = 
     Empty of int
   | Full  of stamp array * 'a array * stamp_adj (* suc *)

  structure Types = UnlabGraphTypes (struct type 'a graph='a graph end)
  open Types

  (* exported functions *)
 
  val empty    = Empty 1
  
  fun embed ((p,n,l,s),Empty i) = 
      embed ((p,n,l,s),Full (array (i,0),array (i,l),array (i,[])))
   |  embed ((p,n,l,s),Full (na,la,sa)) = 
      let val stampN      = stampTrue (getNegStamp (na,n))
          val stampedSuc  = stampList (na,s)
          fun updAdj (a,[])   = a
           |  updAdj (a,v::l) = updAdj (apply (a,v,fn adj=>(n,stampN)::adj),l)
       in
          Full (update (na,n,stampN),update (la,n,l),
                updAdj (update (sa,n,stampedSuc),p))
      end

  fun match    _ = raise NotImplemented
  fun matchAny _ = raise NotImplemented
  fun context  _ = raise NotImplemented
  fun bwd      _ = raise NotImplemented
  fun pred     _ = raise NotImplemented

  fun matchFwd (n,Empty _) = raise Match
   |  matchFwd (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then
         ((sub (la,n),valid (na,sa,n)),
          Full (apply (na,n,stampFalse),la,sa))
      else raise Match

  fun matchAnyFwd (Empty _) = raise Match
   |  matchAnyFwd (g as (Full (na,_,_))) = 
             (matchFwd (firstIndex (na,fn i=>i>0),g)
              handle Subscript => raise Match)

  fun fwd (n,Empty _) = raise Match
   |  fwd (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then (sub (la,n),valid (na,sa,n)) else raise Match

  fun suc (n,Empty _) = raise Match
   |  suc (n,Full (na,la,sa)) = 
      if getStamp (na,n)>0 then valid (na,sa,n) else raise Match

  fun ufold _ _ _       = raise NotImplemented
  fun gfold _ _ _ _ _ _ = raise NotImplemented

  fun nodes (Empty _)       = []
   |  nodes (Full (na,_,_)) = toList (fn i=>i>0,p1) na
   
  fun labNodes (Empty _)            = []
   |  labNodes (g as Full (_,la,_)) = map (fn v=>(v,sub (la,v))) (nodes g)

  fun noNodes g = length (nodes g)

  fun isEmpty (Empty _)       = true
   |  isEmpty (Full (na,_,_)) =
      (fn x=>false) (firstIndex (na,fn i=>i>0)) handle Subscript => true
   
  fun newNodes _ _ = raise NotImplemented
  
  fun mkgr (nl,el) =
      let fun maxNode []         = 0
           |  maxNode ((v,w)::l) = Int.max (Int.max (v,w),maxNode l)
          val n = maxNode el+1
          val N = Array.array (n,0)
          val L = Array.fromList nl
          val S = Array.array (n,[]:(node * stamp) list)
          fun scan []          = ()
           |  scan ((v,w)::el) = (Array.update (N,v,1);Array.update (N,w,1);
                                  Array.update (S,v,(w,1)::Array.sub (S,v));
                                  scan el)
       in
          (scan el;
           Full (fromImpArray N,fromImpArray L,fromImpArray S))
      end

  fun adj (Empty _) = []
   |  adj (g as Full (na,la,sa)) = 
      let fun adj' n = (n,(sub (la,n),valid (na,sa,n)))
       in map adj' (nodes g) end
(*  
  fun reset () = init ()
  fun report () = stat ()
*)
end (* functor UnlabStaticGraphArrayFwd *)
