(*
 *  util/funarray/array.sig  --  interface for functional arrays
 *
 *  COPYRIGHT (c) 1997 by Martin Erwig.  See COPYRIGHT file for details.
 *)


signature FUN_ARRAY = 
sig
  type 'a array
  val array        : int * 'a -> 'a array
  val sub          : 'a array * int -> 'a
  val size         : 'a array -> int
  val update       : 'a array * int * 'a -> 'a array
  val apply        : 'a array * int * ('a -> 'a) -> 'a array
  val firstIndex   : 'a array * ('a -> bool) -> int
  val toList       : ('a -> bool) * (int * 'a -> 'b) -> 'a array -> 'b list
  val toImpArray   : 'a array -> 'a Array.array
  val fromList     : 'a list -> 'a array
  val fromImpArray : 'a Array.array -> 'a array
(*
  to gather some statistical information ...
    
  val init : unit-> unit
  val stat : unit -> unit
*)
end
;
