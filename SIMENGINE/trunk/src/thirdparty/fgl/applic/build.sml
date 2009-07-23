(*
 *  build.sml  --  auxiliary functions for building graphs
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structure defined:
   
    Build   defines twofunctions for generating (un)labeled edge lists 
            to construct sparse graphs
 *)
  
structure Build =
struct
  (* 
     edgeList       : build labeled edge list for sparse graphs 
     unlabEdgeList  : build unlabeled edge list for sparse graphs
  *)
  local
    fun tailMkSuc (n,d,i,[],el)   = el
     |  tailMkSuc (n,d,i,x::l,el) = 
        if i>d then tailMkSuc (n,d,1,l,el) 
               else tailMkSuc (n,d,i+1,x::l,(x,(x+i) mod n,())::el)      
  in
     fun edgeList (n,d) = 
         let val l = UGeneral.to (0,n-1)
          in 
             (l,tailMkSuc (n,d,1,l,[])) 
         end
  end  

  local
    fun tailMkSuc (n,d,i,[],el)   = el
     |  tailMkSuc (n,d,i,x::l,el) = 
        if i>d then tailMkSuc (n,d,1,l,el) 
               else tailMkSuc (n,d,i+1,x::l,(x,(x+i) mod n)::el)      
  in
     fun unlabEdgeList (n,d) = 
         let val l = UGeneral.to (0,n-1)
          in 
             (l,tailMkSuc (n,d,1,l,[])) 
         end
  end  (* local for unlabEdgeList *)
end (* structure Build *)
