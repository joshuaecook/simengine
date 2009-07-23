(*
 *  util/funarray/carray.sml  --  version tree implementation of functional 
 *                                arrays, extended by a cache to speed up 
 *                                single-threaded use
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)


structure CArray:FUN_ARRAY = 
struct

  (* An array is either the (immutable) original array (Root of ...)
     or a modified array (Node of ...). A modified array consists
     of the index to be modified (int), the new value for this 
     index ('a), and the old array ('a array).
     
     So far this is the original structure of v-arrays. Search goes from
     a modification node towards the original array until a node with
     the searched index is found, or the original array is reached.
     
     A c-array stores an additional array used as a cache for the
     "last" updates on the functional array. Assume the updates to 
     an array are ordered left-to-right, then the array cache contains 
     the values of the array represented by the minimum (leftmost) node
     in the array tree. Initially, the cache contains the initial array
     values. Now searches in the leftmost array can be served by the
     cache. In particular, this means that single-threaded arrays (which
     are characterized by being a path in the v-array-representation)
     can be updated and searched in constant time. Since any but the first
     update of an array cannot make any use of the cache, we use two
     constructors: 
       Cache: for nodes on the left spine of the array version tree.
              These may use the cache
       Node:  for all other nodes which cannot use the cache.
     This saves a little space.
     
     How do we know that we can use the cache? When has the cache to
     be updated? We keep and propagate a boolean flag which is set to 
     true only for the leftmost node.
     
     Extension (13 Aug 1997): 
     - create a new cache for each version derived directly from the original
     - create cache only on demand

  *)     

(* 
 *  profiling info (turned off)
 *
 *  cacc - number of cache accesses
 *  tacc - number of tree searches
 *  plen - total path length (number of edges) traversed in version trees

  val (cacc,tacc,plen) = (ref 0,ref 0,ref 0)  
  fun inc r = (r := !r + 1)
  fun init () = (cacc := 0; tacc := 0; plen := 0);
  fun stat () = (print ("Cache: "^Int.toString (!cacc)^
                        ", Version Tree: "^Int.toString (!tacc)^
                        ", Path Length: "^Int.toString (!plen)^"\n"))
 *)

  
  datatype 'a array = 
      Root  of 'a Array.array
    | Node  of int * 'a * 'a array
    | Cache of int * 'a * 'a array * bool ref * 'a Array.array
 
  fun array (n,x) = Root (Array.array (n,x))
  
  fun search (Root a,i)               = Array.sub (a,i)
   |  search (Cache (j,x,tree,_,_),i) = if i=j then x else search (tree,i)
   |  search (Node (_,_,Root a),i)    = Array.sub (a,i)
   |  search (Node (j,x,tree),i)      = if i=j then x else search (tree,i)
 
  and sub (Root a,i) = Array.sub (a,i)
   |  sub (tree as Cache (_,_,_,ref cache,c),i) =
          if cache then Array.sub (c,i) else search (tree,i)
   |  sub (tree,i) = search (tree,i)

(*
 * profiling: off
 *
  fun search (tree,i) = (inc plen; search' (tree,i))
  
  and search' (Root a,i)               = Array.sub (a,i)
   |  search' (Cache (j,x,tree,_,_),i) = if i=j then x else search (tree,i)
   |  search' (Node (_,_,Root a),i)    = Array.sub (a,i)
   |  search' (Node (j,x,tree),i)      = if i=j then x else search (tree,i)
 
  and sub (Root a,i) = Array.sub (a,i)
   |  sub (tree as Cache (_,_,_,ref cache,c),i) =
          if cache then (inc cacc; Array.sub (c,i))
                   else (inc tacc; search (tree,i))
   |  sub (tree,i) = (inc tacc; search (tree,i))
*)

  fun size (Root a)            = Array.length a
   |  size (Node (_,_,a))      = size a
   |  size (Cache (_,_,_,_,a)) = Array.length a
   
  fun update (Root a,i,x) =
      let val c = Array.array (Array.length a,x)
       in (Array.copy {src=a,si=0,len=NONE,dst=c,di=0};
           Array.update (c,i,x);
           Cache (i,x,Root a,ref true,c))
      end
   |  update (a as Cache (_,_,_,cache,c),i,x) =
      if !cache then 
         (cache := false; Array.update (c,i,x); Cache (i,x,a,ref true,c))
      else 
         Node (i,x,a)
   |  update (a,i,x) = Node (i,x,a)
   
  fun fromList l = Root (Array.fromList l)


  (* additional array functions *)

  fun apply (a,i,f) = update (a,i,f (sub (a,i)))
 
  fun firstIndex (a,p) =
      let fun scan (i,p) = if p (sub (a,i)) then i else scan (i+1,p)
       in scan (0,p) end
      
  fun toList (p,f) a =
      let val n = size a
          fun from i = 
              if i<n then
                 if p (sub (a,i)) then 
                    f (i,sub (a,i))::from (i+1) 
                 else 
                    from (i+1)
              else []
       in from 0 end

  fun fromImpArray a = Root a

  exception NotImplemented
  fun toImpArray (Root a) = 
      let val b = Array.array (Array.length a,Array.sub (a,0))
       in (Array.copy {src=a,si=0,len=NONE,dst=b,di=0}; b) end
   |  toImpArray a = Array.fromList (toList (fn _ => true,fn (_,y)=>y) a)        
end

(*
   some tests ...
   
   Compiler.Control.Print.printDepth := 15;
   open CArray;
   fun p a = (toList (fn x=>true,UTuple.p2) a,a);
   
   val a    = array (4,0)
   val a1   = update (a,1,1);         (* 0 1 0 0 *)
   val a11  = update (a1,2,2);        (* 0 1 2 0 *)
   val a2   = update (a,2,7);         (* 0 0 7 0 *)
   val a12  = update (a1,2,3);        (* 0 1 3 0 *)
   val a111 = update (a11,1,4);       (* 0 4 2 0 *)
   
   map p [a,a1,a11,a2,a12,a111];
   
*)
