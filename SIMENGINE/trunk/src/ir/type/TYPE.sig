signature TYPE = sig
    type var = string
    type size = int

    type prim
    datatype rep 
      = Var of var
      | Arrow of rep * rep
      | Pair of rep * rep
      | Array of rep
      | Abstract of var * rep
      | Apply of rep * rep
      | Universal of var * rep
      | Primitive of prim

    type 'a typet

    val var: var -> var typet
    val int: size -> int typet
    val real: size -> real typet
    val bool: bool typet

    val tuple: ('a typet * 'b typet) -> ('a * 'b) typet
    val arrow: ('a typet * 'b typet) -> ('a -> 'b) typet
    val array: ('a -> 'b) typet

    val apply: ('a -> 'b) typet -> 'a typet -> 'b typet
    val poly: ('a typet -> 'b typet) -> 'b typet

    val rep: 'a typet -> rep
    val toString: 'a typet -> string
end
