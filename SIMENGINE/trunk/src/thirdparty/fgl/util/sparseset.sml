(*
 *  util/sparseset.sml  --  2-partition (members/deleted) over a fixed universe 
 *                          of n integersdescription
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
 
signature PARTITION =
sig
  exception Empty
  exception Full
  type part
  val full    : int -> part
  val empty   : int -> part
  val initial : int * int -> part
  val member  : int * part -> bool
  val insert  : int * part -> part
  val delete  : int * part -> part
  val new     : int * part -> int list
  val any     : part -> int
  val members : part -> int list
  val deleted : part -> int list
  val noElems : part -> int
end


functor Partition (FunArray:FUN_ARRAY) : PARTITION =
struct
  structure A=FunArray
  infixr 9 $
  val op $ = A.sub
  fun idmap n = A.fromList (List.tabulate (n,fn x=>x))
  fun upd2 (a,i,x,j,y) = A.update (A.update (a,i,x),j,y)
  

  type part = int A.array * int A. array * int
              (* index *)   (* elems *)    (* count *)
  
  exception Empty
  exception Full
  
  fun full n = (idmap n,idmap n,n)
  fun empty n = (idmap n,idmap n,0)
  fun initial (i,n) = (idmap n,idmap n,i)
  
(*
  fun member (x,(index,elem,n)) = 
      let val i = index$x
       in 
          i<n andalso elem$i=x 
      end
*)
  fun member (x,(index,_,n:int)) = index$x < n
      
  fun insert (x,s as (index,elem,n)) =
      let val i = index$x
       in
          if i<n (* == member (x,s) *) then s else 
          let val y = elem$n
           in
             (upd2 (index,x,n,y,i),
              upd2 (elem,n,x,i,y),n+1)
          end
      end
   
  fun delete (x,s as (index,elem,n)) =
      let val i = index$x
          val p = n-1
       in
          if i<n (* == member (x,s) *)  then
             let val y = elem$p
              in 
                 (upd2 (index,x,p,y,i),
                  upd2 (elem,p,x,i,y),p)
             end
          else 
             s
      end
  
  fun any (_,elem,n) = if n=0 then raise Empty else elem$0

  fun new (i,(_,elem,n)) =
      let val m=n+i-1
          fun from k = if k<=m then elem$k::from (k+1) else []
       in
          if m=A.size elem then raise Full else from n
      end 
      
  fun members (_,elem,n) =
      let fun from i = if i<n then elem$i::from (i+1) else []
       in from 0 end

  fun deleted (_,elem,n) =
      let val m = A.size elem
          fun from i = if i<m then elem$i::from (i+1) else []
       in from n end
   
  fun noElems (_,_,n) = n
  
  fun p (index,elem,n) =
      (A.toList (fn x=>true,UTuple.p2) index,
       A.toList (fn x=>true,UTuple.p2) elem,n)
end
;
(* 
   debug
   
   Compiler.Control.Print.printDepth := 15;
   structure S = Partition (CArray);
   open S;
   
   val s = empty 5;
   
   val s1 = insert (3,s);
   val s2 = insert (1,s1);
   val s3 = delete (3,s2);
   val s4 = insert (2,s3);
   
   map (fn s=>(members s,deleted s)) [s1,s2,s3,s4];

*)
   