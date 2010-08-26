functor TestType(T: TYPE) = struct

local 
    open T
    val bool = bool
    val int32 = int 32
    val int64 = int 64
    val real64 = real 64
in
val toString = toString
val equiv = equiv
val app = apply

val int = int32
val real = real64
val tuple = tuple
val array = array

val intpair = tuple(int, int)
val realpair = tuple(real, real)
val intreal = tuple(int, real)
val realint = tuple(real, int)

val intarray = app(array, int)

val intpairarray = app(array, intpair)

val _ = 
    case equiv (intpair, intpair)
     of false => raise Fail "bug"
      | _ => ()
val _ =
    case equiv (intpair, intpairarray)
     of true => raise Fail "bug"
      | _ => ()

end

end

structure Test = TestType(Type)
