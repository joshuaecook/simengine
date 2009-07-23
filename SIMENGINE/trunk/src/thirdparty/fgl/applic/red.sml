(*
 *  red.sml  --  Combinator Graph Reduction
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structures and functors defined:
 
    Value   defines values that are stored in graphs
    GrRed   defines a combinator graph reducer
 *) 

(* 
   Some remarks on "ordered" graphs:
   In an *ordered* graph, the successors of a node are totally ordered.
   Consider an expression embed ((p,v,l,s),g):
   (1) the order of the successors of v can be assumed as imlicitly given
       by the order of elements in the list s, but
   (2) for each predecessor w of p, the position of v in w's
       successor list must be explicitly specified.
   Thus, ordered graphs can be appropriately modeled by labeled
   graphs where integer edge labels specify order of successors.
   The user has to care for duplicate edge labels, especially
   when specifying predecessors.   
   Specifically for combinator graph reduction, it is sufficient
   to consider only two edge labels "Left" and "Right".

*)


structure Value =
struct
  datatype value = INT of int | BOOL of bool 
  datatype edge_label = L | R                      
  datatype node_label = 
     APP
   | COND
   | VAL   of value 
   | OP    of (value * value -> value) * string
   | COMB  of string

  fun nlabToString APP            = "@"
   |  nlabToString COND           = "IF"
   |  nlabToString (VAL (INT i))  = Int.toString i
   |  nlabToString (VAL (BOOL b)) = if b then "true" else "false"
   |  nlabToString (OP  (_,s))    = s
   |  nlabToString (COMB s)       = s

  val plus  = OP (fn (INT x,INT y)=>INT (x+y),"+")
  val minus = OP (fn (INT x,INT y)=>INT (x-y),"-")
  val times = OP (fn (INT x,INT y)=>INT (x*y),"*")
  val eq    = OP (fn (INT x,INT y)=>BOOL (x=y) 
                   | (BOOL x,BOOL y)=>BOOL (x=y),"=")
  val lt    = OP (fn (INT x,INT y)=>BOOL (x<y),"<")
  val gt    = OP (fn (INT x,INT y)=>BOOL (x>y),"<")
  val i = VAL o INT
  val b = VAL o BOOL
end


functor GrRed (G:GRAPH) =
struct
  open Value
  
  fun noArgs COND       = 3
   |  noArgs (VAL _)    = 0
   |  noArgs (OP _)     = 2  (* treat only binary operations *)
   |  noArgs (COMB "I") = 1
   |  noArgs (COMB "Y") = 1
   |  noArgs (COMB "K") = 2
   |  noArgs (COMB _)   = 3  (* S, B, or C *)
   
  val [I,K,S,B,C] = map COMB ["I","K","S","B","C"]

  exception Reduction
  fun error s = (print "\n";print s;print "\n"; raise Reduction)
  fun argError (i,j,C,arg) = 
      (print "\nWrong no. of arguments. ";
       print "Expected: ";print (Int.toString i);
       print ", found: ";print (Int.toString j);
       print ". \nIn combinator expression: "; print (nlabToString C); 
(*
       print (ListFormat.formatList {init="(",sep=",",final=")",
             fmt=fn v=>Int.toString v} arg);
*)
       print "\n"; raise Reduction)

(*
 *  unwind follows L-edges until a non-APP node is found
 *  depending on the combinator found, it returns a list
 *  of k argument nodes followed by the node of the 
 *  corresponding root node to be overwritten (replaced)
 *)
  exception tooFewArguments
  fun unwind (v,stack,g) =
      let val (comb,suc) = G.fwd (v,g)
          fun pop (0,_)        = []
           |  pop (1,(w,r)::l) = [r,w] (* last arg r followed by root w *)
           |  pop (n,(_,r)::l) = r::pop (n-1,l) 
           |  pop (n,[])       = raise tooFewArguments
          fun lr [(L,v),(R,w)] = (v,w)
           |  lr [(R,w),(L,v)] = (v,w)
           |  lr _             = error ("Expected 2 suc. of APP, found: "^
                                         Int.toString (length suc))
       in 
          case comb of 
            APP => let val (l,r) = lr suc
                    in unwind (l,(v,r)::stack,g) end
          | _   => (comb,pop (noArgs comb,stack))
      end
              
  val reductions = ref 0;
  val allocations = ref 0;
  
  fun init () = (reductions := 0; allocations := 0)
  fun inc r = (r := !r + 1)
  fun report () = (print (Int.toString (!reductions)^" Reductions, "^
                          Int.toString (!allocations)^" Allocations.\n"))

  fun newSuc (v,s,g) =
      let val ((p,_,l,_),g') = G.match (v,g)
       in G.embed ((p,v,l,s),g') end
                  
  fun combRed (v,e) =
    let val (comb,args) = unwind (v,[],e)
     in
        (inc reductions;
         case comb of
          COMB "I" => 
            let val [x,r] = args 
                val ((rp,_,_,_),e') = G.match (r,e)
                val (xl,xs) = G.fwd (x,e')
             in G.embed ((rp,r,xl,xs),e') end
        | COMB "K" =>
            let val [x,y,r] = args 
                val ((rp,_,_,_),e') = G.match (r,e)
                val (xl,xs) = G.fwd (x,e')
             in G.embed ((rp,r,xl,xs),e') end
        | COMB "S" =>
            let val [f,g,x,r] = args 
                val [n,m] = (inc allocations; inc allocations; G.newNodes 2 e)
             in 
                newSuc (r,[(L,n),(R,m)],
                   G.embed (([],n,APP,[(L,f),(R,x)]),
                   G.embed (([],m,APP,[(L,g),(R,x)]),e)))
            end
        | COMB "B" =>
            let val [f,g,x,r] = args 
                val [n] = (inc allocations; G.newNodes 1 e)
             in 
                newSuc (r,[(L,f),(R,n)],
                   G.embed (([],n,APP,[(L,g),(R,x)]),e))
            end
        | COMB "C" =>
            let val [f,x,y,r] = args 
                val [n] = (inc allocations; G.newNodes 1 e)
             in 
                newSuc (r,[(L,n),(R,x)],
                   G.embed (([],n,APP,[(L,f),(R,y)]),e))
            end
        | COMB "Y" =>
            let val [f,r] = args 
                val ((rp,_,_,_),e') = G.match (r,e)
             in 
                G.embed ((rp,r,APP,[(L,f),(R,r)]),e')
            end
        | OP (f,_) =>
            let val [x,y,r] = args 
                val (a,e1) = eval x e
                val (b,e2) = eval y e1
                val ((rp,_,_,_),e') = G.match (r,e2)
             in 
                G.embed ((rp,r,VAL (f (a,b)),[]),e')
            end
        | COND =>
            let val [c,x,y,r] = args 
                val (BOOL a,e1) = eval c e
                    handle Bind => error ("Expected boolean value")
                val ((rp,_,rl,rs),e') = G.match (r,e1)
                val z=if a then x else y
                val (zl,zs) = if z=r then (rl,rs) else G.fwd (z,e')
             in 
               G.embed ((rp,r,zl,zs),e')
           end            
        | _ => error ("Unknown Combinator. Cannot reduce "^nlabToString comb)
       ) handle Bind => 
         error ("Illegally found too few arguments in "^nlabToString comb)
    end

  and eval v e = 
    let val (lab,_) = G.fwd (v,e)
     in
        (case lab of
           VAL x => (x,e)
         | APP   => eval v (combRed (v,e))
         | _     => error ("Cannot eval "^nlabToString lab) )
        handle tooFewArguments => (BOOL true,e)
    end

  fun root g = hd (List.filter (fn v=>G.pred (v,g)=[]) (G.nodes g));
  fun eval' e = (init ();
                 let val r = eval (root e) e
                  in (report ();r) end)

end (* GrRed *)

(*
   ... reducing gaphs

structure R=GrRed(Graph);


(1) some example expressions, see also lamtr.sml

local open Value R
in 
   (* Field and Harrison, p. 284: lam x.+ 2 (- x 3)  *)
   val g = G.mkgr ([APP,APP,i 4,APP,APP,B,APP,APP,i 3,plus,i 2,C,minus],
                   [(0,1,L),(0,2,R),(1,3,L),(1,4,R),
                    (3,5,L),(3,6,R),(4,7,L),(4,8,R),
                    (6,9,L),(6,10,R),(7,11,L),(7,12,R)]);
   
   (* fac *)
   val f = G.mkgr ([APP,APP,APP,S,APP,APP,APP,
                    APP,i 2,S,times,APP,APP,C,APP,B,APP,i 1,
                    APP,APP,C,minus,B,COND,eq,i 2],
                   [(0,1,L),(0,2,R),(1,3,L),(1,4,R),(2,5,L),(2,6,R),
                    (4,7,L),(4,8,R),(5,9,L),(5,10,R),(6,11,L),(6,12,R),
                    (7,13,L),(7,14,R),(11,15,L),(11,0,R),(12,16,L),(12,17,R),
                    (14,18,L),(14,19,R),(16,20,L),(16,21,R),
                    (18,22,L),(18,23,R),(19,24,L),(19,25,R)]);
   
   (* fac 3  (do not try fac x for x>12) *)
   val f3 = G.mkgr ([APP,APP,APP,S,APP,APP,APP,
                    APP,i 2,S,times,APP,APP,C,APP,B,APP,i 1,
                    APP,APP,C,minus,B,COND,eq,i 2,
                    APP,i 3],
                   [(0,1,L),(0,2,R),(1,3,L),(1,4,R),(2,5,L),(2,6,R),
                    (4,7,L),(4,8,R),(5,9,L),(5,10,R),(6,11,L),(6,12,R),
                    (7,13,L),(7,14,R),(11,15,L),(11,0,R),(12,16,L),(12,17,R),
                    (14,18,L),(14,19,R),(16,20,L),(16,21,R),
                    (18,22,L),(18,23,R),(19,24,L),(19,25,R),
                    (26,0,L),(26,27,R)]);
   
   (* simple conditional: if true then 1 else 2 *)
   val g2 = G.mkgr ([COND,b true,i 1,i 2],
                    [(0,1,1),(0,2,2),(0,3,3)]);    
end;


(2) test graph reducer

R.eval' f3;
R.eval' (fac 3);  (* use examples from lamtr.sml *)
R.eval' (Yfac 3); (* use Y combinator to represent recurison *)


*)
