(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. 
 * Based on lib/mlton/basic/layout.sig from MLton:
 *
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LAYOUT =
   sig
      type t

      structure Out: sig
	  type t
	  val outputc: t -> string -> unit
	  val newline: t -> unit
      end

      (* layout the objects on separate lines*)
      val align: t list -> t
      val alignPrefix: t list * string -> t
      val array: t array -> t
      (* Whether or not to print things in detail -
       * routines that create layouts should use this flag to decide
       * how detailed to print.
       *)
      val detailed: bool ref
      val empty: t
      val ignore: 'a -> t
      val isEmpty: t -> bool
      val makeOutput: ('a -> t) -> 'a * Out.t -> unit
      (* layout the objects on separate lines, if necessary *)
      val mayAlign: t list -> t
      val namedRecord: string * (string * t) list -> t
      (* indent the entire object *)
      val indent: t * int -> t
      val list: t list -> t
      val output: t * Out.t -> unit
      val outputl: t * Out.t -> unit
      val outputTree: t * Out.t -> unit
      val outputWidth: t * int * Out.t -> unit
      val paren: t -> t
      (* print the object *)
      val print: t * (string -> unit) -> unit
      val record: (string * t) list -> t
      val schemeList: t list -> t
      (* put string between elements *)
      val separate: t list * string -> t list
      (* adds string at beginning of all objects except first *)
      val separateLeft: t list * string -> t list
      (* adds string at the end of all objects except last *) 
      val separateRight: t list * string -> t list
      (* layout the objects on the same line *)
      val seq: t list -> t
      (* convert a string to a layout object *)
      val str: string -> t
      val switch: {detailed: 'a -> t, normal: 'a -> t} -> 'a -> t
      val toString: t -> string
      val tuple: t list -> t
      val tuple2: ('a -> t) * ('b -> t) -> 'a * 'b -> t
      val tuple3: ('a -> t) * ('b -> t) * ('c -> t) -> 'a * 'b * 'c -> t
      val tuple4: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t)
         -> 'a * 'b * 'c * 'd -> t
      val tuple5: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t) * ('e -> t)
         -> ('a * 'b * 'c * 'd * 'e) -> t
      val vector: t vector -> t

      (* SIMATRA added functions *)
      (* series wraps a list with a starting string, ending string, and a delimeter, 
       * with mayAlign for all the entries *)
      val series: (string * string * string) -> t list -> t
      (* parenList, bracketList, and curlyList all use series *)
      val parenList: t list -> t
      val bracketList: t list -> t
      val curlyList: t list -> t
      (* bracket and curly are complementary to paren *)
      val bracket: t -> t
      val curly: t -> t
      (* creates a newline token *)
      val newline: t
      (* or append a newline *)
      val add_newline: t -> t
      (* label and heading are useful for displaying tree like data.  In a label, the string is printed on the
       * same line as the following layout.  For a heading, the layout appears on the line following the string, 
       * indented by a two spaces. *)
      val label: (string * t) -> t
      val heading: (string * t) -> t
      (* separate with spaces *)
      val space: t list -> t
      (* write to the screen *)
      val log: t -> unit
      (***  utility functions added by Simatra ****)
      val repStr : (t * string * string) -> t (* replace the second string with the third string in the first layout *)

      (* Sets the "detailed" option for the duration of a function. *)
      val withDetail: (unit -> 'a) -> 'a
      val withoutDetail: (unit -> 'a) -> 'a

   end
