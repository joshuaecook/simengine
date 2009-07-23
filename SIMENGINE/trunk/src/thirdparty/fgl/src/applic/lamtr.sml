(*
 *  lamtr.sml  --  Translating lambda-expressions into combinator expressions
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)
 
(*
   structures and functors defined:
 
    LambdaCalc    defines a representation for lambda expressions
    CombExpr      defines a representation for combinator expressions and
                  a translation "abs" from lambda terms into combinator 
                  expressions
    LambdaTrans   defines a translation function "toGraph" from 
                  lambda terms into combinator expressions
 *) 

structure LambdaCalc =
struct
  structure V = Value  (* defined in red.sml *)
  
  datatype lambda = 
     VAR  of string
   | COND 
   | VAL  of V.value
   | OP   of (V.value * V.value -> V.value) * string
   | LAM  of string * lambda
   | APP  of lambda * lambda
   | SELF
  
  fun APPL [x,y]     = APP (x,y) 
   |  APPL [x,y,z]   = APP (APP (x,y),z) 
   |  APPL [x,y,z,a] = APP (APP (APP (x,y),z),a)

  val plus  = OP (fn (V.INT x,V.INT y)=>V.INT (x+y),"+")
  val minus = OP (fn (V.INT x,V.INT y)=>V.INT (x-y),"-")
  val times = OP (fn (V.INT x,V.INT y)=>V.INT (x*y),"*")
  val eq    = OP (fn (V.INT x,V.INT y)=>V.BOOL (x=y) 
                  | (V.BOOL x,V.BOOL y)=>V.BOOL (x=y),"=")
  val lt    = OP (fn (V.INT x,V.INT y)=>V.BOOL (x<y),"<")
  val gt    = OP (fn (V.INT x,V.INT y)=>V.BOOL (x>y),">")
  val i = VAL o V.INT
  val b = VAL o V.BOOL
end


structure CombExpr =
struct
  structure V = Value
  structure L = LambdaCalc

  datatype comb_term =
     VAR  of string   (* only in intermediate values *)
   | COND
   | VAL  of V.value
   | OP   of (V.value * V.value -> V.value) * string
   | COMB of string
   | APP  of comb_term * comb_term
   | SELF

  val [I,K,S,B,C] = map COMB ["I","K","S","B","C"]

  fun abs (x,VAR y) = if x=y then I else APP (K,VAR y)
   |  abs (x,APP (e1,e2)) = 
      let val a1 = abs (x,e1)
          val a2 = abs (x,e2)
       in
          case a1 of 
             APP (COMB "K",M) => (
               case a2 of COMB "I"         => M
                       |  APP (COMB "K",N) => APP (K,APP (M,N))
                       |  _                => APP (APP (B,M),a2) )
           | _ => (
               case a2 of APP (COMB "K",N) => APP (APP (C,a1),N)
                       |  _                => APP (APP (S,a1),a2) )
      end
   |  abs (x,comb) = APP (K,comb)

  fun comb (L.VAR x)       = VAR x
   |  comb (L.COND)        = COND
   |  comb (L.VAL x)       = VAL x
   |  comb (L.OP f)        = OP f
   |  comb (L.LAM (x,e))   = abs (x,comb e)
   |  comb (L.APP (e1,e2)) = APP (comb e1,comb e2)
   |  comb (L.SELF)        = SELF
   
  fun pr (VAR s)          = s
   |  pr COND             = "COND"
   |  pr (VAL (V.INT i))  = Int.toString i
   |  pr (VAL (V.BOOL b)) = if b then "true" else "false"
   |  pr (OP (_,s))       = s
   |  pr (COMB s)         = s
   |  pr (APP (f,g))      = "("^pr f^" "^pr g^")"
   |  pr SELF             = "SELF"
end


functor LambdaTrans (G:GRAPH) =
struct
  structure V = Value
  structure L = LambdaCalc
  structure E = CombExpr
  
  (* i is a counter for generating nod identifiers
     r is the root node of the function which might be used
       recursively. r is used in edges from APP nodes to SELF nodes
  *)
  fun toEdgeList (r,i,E.COND)      = (i+1,[V.COND],[])
   |  toEdgeList (r,i,E.VAL x)     = (i+1,[V.VAL x],[])
   |  toEdgeList (r,i,E.OP f)      = (i+1,[V.OP f],[])
   |  toEdgeList (r,i,E.COMB k)    = (i+1,[V.COMB k],[])
   |  toEdgeList (r,i,E.APP (f,g)) = 
      let val (j,nl,el)   = toEdgeList (r,i+1,f)
          val (k,nl',el') = toEdgeList (r,j,g)
          val left = if j=i then r else i+1
          val right = if k=j then r else j
       in
          (k,V.APP::nl@nl',(i,left,V.L)::(i,right,V.R)::el@el')
      end
   |  toEdgeList (r,i,E.SELF)      = (i,[],[])

  (* toGraph works on lambda expressions and constructs
             cyclic graphs for recursive functions
     toDAG   works on combinator expressions and constructs
             acyclic graphs (assuming use of Y combinator)
   *)
  fun toGraph r l = (G.mkgr o UTuple.t23 o toEdgeList) (r,0,E.comb l)
  fun toDAG e = (G.mkgr o UTuple.t23 o toEdgeList) (0,0,e)
  
end 

(* 
   ... generating test graphs 

structure T = LambdaTrans(Graph);


(1) define some lambda expressions:
    lfac and lfib are two function generating applications
    of lambda expressions (for the factorial and fibonacci functions)
    to constants.
(1b) try representation with Y combinator
     f(x)=E  ==>  f=Y(lam f.lam x.E)
    
local open LambdaCalc
 in
    fun lfac x = APP (LAM ("x",APPL [COND,APPL [eq,i 2,VAR "x"],i 2,
                      APPL [times,VAR "x",APP (SELF,APPL [minus,VAR "x",i 1])]]),
                      i x)
    fun lfib x = APP (LAM ("x",APPL [COND,APPL [gt,i 3,VAR "x"],i 1,
                      APPL [plus,APP (SELF,APPL [minus,VAR "x",i 2]),
                                 APP (SELF,APPL [minus,VAR "x",i 1])]]),
                      i x)
end;       


local open LambdaCalc
 in
    val lYfac = LAM ("f",LAM ("x",APPL [COND,APPL [eq,i 2,VAR "x"],i 2,
                 APPL [times,VAR "x",APP (VAR "f",APPL [minus,VAR "x",i 1])]]))
end;       

    
(2) translate lambda expressions into graphs:
    fac and fib are two functions that create a lambda expression
    (by using lfac and lfib) which is then translated into a graph by
    toGraph. The root nodes of the functions are 1 (0 is the root
    of the whole expression, ie, the APP node).
(2b) try representation with Y combinator
     f a   ==>  Y(lam f.lam x.E) y  ==> Y([f][x]comb(E)) a

local open LambdaCalc 
 in
    fun fac x = T.toGraph 1 (lfac x)
    fun fib x = T.toGraph 1 (lfib x)
    val co    = T.toGraph 0 (APP (LAM ("x",i 17),i 13))
end;

     
local open CombExpr 
 in
    fun Yfac a = T.toDAG (APP (APP (COMB "Y",comb lYfac),VAL (V.INT a)))
end;       

local open CombExpr 
 in
    val yc3 = APP (APP (COMB "Y",comb lYfac),VAL (V.INT 3));
    val cp  = pr
end                
;

*)