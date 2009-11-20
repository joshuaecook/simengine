namespace Rules

var simplification = [


/* Operator Simplification */
/* a-b -> a+(-b) */
rulematch $$a.one - $$b.one -> $a + (-$b),

/* a/b -> a*b^(-1) */
rulematch $$a.one / $$b.one -> $a * ($b  ^ (-1)),

/* a*(-1) -> -a */ //removed for now to treat negation as multiplication and use other rules
//rulematch modelop("mul", [$$c.any, $$a.one, -1, $$b.any]) -> modelop("mul", [$b, -$a, $c]),
//rulematch modelop("mul", [$$c.any, -1, $$a.one,$$b.any]) -> modelop("mul", [$b, -$a, $c]),

/* negation removal */
/* (-a) -> -1 * a */
rulematch -$$a.one -> modelop("mul", [-1, $a]),

/* Aggregation: multi-op creation*/
rulematch modelop("mul", [$$d1.any, modelop("mul", [$$d2.any]), $$d3.any]) -> modelop("mul", [$d1, $d2, $d3]),
rulematch modelop("add", [$$d1.any, modelop("add", [$$d2.any]), $$d3.any]) -> modelop("add", [$d1, $d2, $d3]),
rulematch modelop("and", [$$d1.any, modelop("and", [$$d2.any]), $$d3.any]) -> modelop("and", [$d1, $d2, $d3]),
rulematch modelop("or", [$$d1.any, modelop("or", [$$d2.any]), $$d3.any]) -> modelop("or", [$d1, $d2, $d3]),


/* -0 -> 0 */
rulematch modelop("neg", [0]) -> 0,

// a + a -> 2 * a
rulematch modelop("add", [$$d1.any, $$a.one, $$d2.any, $$a.one, $$d3.any]) -> modelop("add", [$d1, $d2, 2 * $a, $d3]),

/* a^0 -> 1 */
rulematch $$a.one ^ 0 -> 1,

/* 0^a -> 0 */
rulematch 0 ^ ($$a.one) -> 0,

/* add(a) -> a */ /* a is a scalar */
rulematch modelop("add", [$$a.one]) -> $a,

/* mul(a) -> a */
rulematch modelop("mul", [$$a.one]) -> $a,

/* and(a) -> a */ /* a is a scalar */
rulematch modelop("and", [$$a.one]) -> $a,

/* or(a) -> a */ /* a is a scalar */
rulematch modelop("or", [$$a.one]) -> $a,


/* add() -> sequence() */
//rulematch modelop("add", []) -> a sequence....
/* mul() -> sequence() */
//rulematch modelop("mul", []) -> a sequence....


/* Power Rules */
/* a^b*a^c -> a^(b+c) */
rulematch modelop("mul", [$$d1.any, $$a.one^$$b.one, $$d2.any, $$a.one^$$c.one, $$d3.any]) -> modelop("mul", [$d1, $d2, $d3, $a ^ ($b + $c)]),

/* a^c*b^c -> (a*b)^c */
rulematch modelop("mul", [$$d1.any, $$a.one^$$c.one, $$d2.any, $$b.one^$$c.one, $$d3.any]) -> modelop("mul", [$d1, $d2, $d3, (($a * $b) ^ ($c))]),

/* (a^b)^c -> a^(b*c) */
rulematch ($$a.one^$$b.one)^$$c.one -> $a^($b * $c),

/* a*a -> a^2 */
rulematch modelop("mul", [$$d1.any, $$a.one, $$d2.any, $$a.one, $$d3.any]) -> modelop("mul", [$d1, $a^2, $d2, $d3]),

/* a*a^b -> a^(1+b) */
rulematch modelop("mul", [$$d1.any, $$a.one, $$d2.any, $$a.one ^ $$b, $$d3.any]) -> modelop("mul", [$d1, $a^(1+$b), $d2, $d3]),
rulematch modelop("mul", [$$d1.any, $$a.one ^ $$b, $$d2.any, $$a.one, $$d3.any]) -> modelop("mul", [$d1, $a^(1+$b), $d2, $d3]),


/* Identity Rules */
/* a+0 -> a */
rulematch modelop("add", [$$d1.any, 0, $$d2.any]) -> modelop("add", [$d1, $d2]),

/* a*1 -> a */
rulematch modelop("mul", [$$d1.any, 1, $$d2.any]) -> modelop("mul", [$d1, $d2]),


/* -1 * -1 -> sequence */
rulematch modelop("mul", [$$d1.any, -1, $$d2.any, -1, $$d3.any]) -> modelop("mul", [$d1, $d2, $d3]),

/* a*0 -> 0 */
rulematch modelop("mul", [$$d1.any, 0, $$d2.any]) -> 0,

/* a^1 -> a */
rulematch $$a.one ^ 1 -> $a,

/* -(-a) -> a */
rulematch -(-$$a.one) -> $a,


/* Inverse Rules */
/* a+(-a) -> 0 */
rulematch modelop("add", [$$d1.any, $$a.one, $$d2.any, -($$a.one), $$d3.any]) -> modelop("add", [0, $d1, $d2, $d3]),
rulematch modelop("add", [$$d1.any, -($$a.one), $$d2.any, $$a.one, $$d3.any]) -> modelop("add", [0, $d1, $d2, $d3]),
rulematch modelop("add", [$$d1.any, $$a.one, $$d2.any, -1*($$a.one), $$d3.any]) -> modelop("add", [0, $d1, $d2, $d3]),
rulematch modelop("add", [$$d1.any, -1*($$a.one), $$d2.any, $$a.one, $$d3.any]) -> modelop("add", [0, $d1, $d2, $d3]),

/* a*a^(-1) -> 1 */
rulematch modelop("mul", [$$d1.any, $$a.one, $$d2.any, $$a.one ^ (-1), $$d3.any]) -> modelop("mul", [1, $d1, $d2, $d3]),
rulematch modelop("mul", [$$d1.any, $$a.one ^ (-1), $$d2.any, $$a.one, $$d3.any]) -> modelop("mul", [1, $d1, $d2, $d3]),

/* inline execution of constants */


/* Logical Rules */
/* a & true -> a */
rulematch modelop("and", [$$d1.any, true, $$d2.any]) -> modelop("and", [$d1, $d2]),

/* a & a -> a */
rulematch modelop("and", [$$d1.any, $$a.one, $$d2.any, $$a.one, $$d3.any]) -> modelop("and", [$d1, $a, $d2, $d3]),

/* a & ~a -> false */
rulematch modelop("and", [$$d1.any, $$a.one, $$d2.any, not ($$a.one), $$d3.any]) -> false,
rulematch modelop("and", [$$d1.any, not($$a.one), $$d2.any, $$a.one, $$d3.any]) -> false,

/* a | true -> true */
rulematch modelop("or", [$$d1.any, true, $$d2.any]) -> true,

/* a | a -> a */
rulematch modelop("or", [$$d1.any, $$a.one, $$d2.any, $$a.one, $$d3.any]) -> modelop("or", [$d1, $a, $d2, $d3]),

/* a | ~a -> true */
rulematch modelop("or", [$$d1.any, $$a.one, $$d2.any, not ($$a.one), $$d3.any]) -> true,
rulematch modelop("or", [$$d1.any, not($$a.one), $$d2.any, $$a.one, $$d3.any]) -> true,


/* a & false -> false */
rulematch modelop("and", [$$d1.any, false, $$d2.any]) -> false,

/* a | false -> a */
rulematch modelop("or", [$$d1.any, false, $$d2.any]) -> modelop("or", [$d1, $d2]),

/* a == a -> true */
rulematch modelop("eq", [$$a.one, $$a.one]) -> true,

/* ~true -> false */
rulematch modelop("not", [true]) -> false,

/* ~false -> true */
rulematch modelop("not", [false]) -> true,

/* ~(~a) -> a */
rulematch not (not $$a) -> $a



]

var expansion = [
/* Distribute Rules */
/* -(a+b) -> (-a)+(-b) */ /*REWRITE USING LAMBDA APP*/
//rulematch modelop("neg" [modelop("add", [$$a.one, $$b.one])]) -> modelop("add", [-$a, -$b])

/* a*(b+c) -> a*b+a*c */ /*REWRITE USING LAMBDA APP*/
//rulematch modelop("mul", [$$d1.any, modelop("add", [$$b

/* De Morgan's Expansion Laws */
/* ~(a & b) -> ~a | ~b */ /* REWRITE USING LAMBDA APP */
/* ~(a | b) -> ~a & ~b */ /* REWRITE USING LAMBDA APP */


]

var factoring = [
/* Factor Rules */
/* a*b+a*c -> a*(b+c) */
rulematch modelop("add", [$$d1.any, modelop("mul", [$$b1.any, $$a.one, $$b2.any]), $$d2.any, modelop("mul", [$$c1.any, $$a.one, $$c2.any]), $$d3.any]) -> modelop("add", [modelop("mul", [$a, modelop("add", [modelop("mul", [$b1, $b2]), modelop("mul", [$c1, $c2])])]), $d1, $d2, $d3]),

// a + c* a -> (c+1) * a
rulematch modelop("add", [$$d1.any, $$a.one, $$d2.any, modelop("mul", [$$c1.any, $$a.one, $$c2.any]), $$d3.any]) -> modelop("add", [modelop("mul", [$a, modelop("add", [1, modelop("mul", [$c1, $c2])])]), $d1, $d2, $d3]),

rulematch modelop("add", [$$d1.any, modelop("mul", [$$c1.any, $$a.one, $$c2.any]), $$d2.any, $$a.one, $$d3.any]) -> modelop("add", [modelop("mul", [$a, modelop("add", [1, modelop("mul", [$c1, $c2])])]), $d1, $d2, $d3]),

/* De Morgan's Factoring Laws */
/* ~a | ~b -> ~(a & b) */ 
rulematch modelop("or", [$$d1.any, not ($$a.one), $$d2.any, not ($$b.one), $$d3.any]) -> modelop("or", [$d1, $d2, $d3, modelop("not", [modelop("and", [$a, $b])])]),

/* ~a & ~b -> ~(a | b) */ 
rulematch modelop("and", [$$d1.any, not ($$a.one), $$d2.any, not ($$b.one), $$d3.any]) -> modelop("and", [$d1, $d2, $d3, modelop("not", [modelop("or", [$a, $b])])]),

/* -a + -b -> -(a+b) */
rulematch modelop("add", [$$d1.any, -$$a.one, $$d2.any, -$$b.one, $$d3.any]) -> modelop("add", [$d1, $d2, $d3, -($a + $b)])
]

var restoration = [
/* add(a) -> a */ /* a is a scalar */
rulematch modelop("add", [$$a.one]) -> $a,

/* mul(a) -> a */
rulematch modelop("mul", [$$a.one]) -> $a,

// -1*a -> -a
rulematch modelop("mul", [$$d1.any, -1, $$d2.any, $$a.one, $$d3.any]) -> modelop("mul", [$d1, $d2, -$a, $d3]),

// a+-b -> a-b
rulematch modelop("add", [$$d1.any, -$$b.one, $$d2.any]) -> modelop("sub", [modelop("add", [$d1, $d2]), $b]),

// a*b^-1 -> a/b
rulematch modelop("mul", [$$d1.any, $$b ^ -1, $$d2.any]) -> (modelop("mul", [$d1, $d2]) / $b),

// b^-1 -> 1/b
rulematch $$b ^ -1 -> 1/$b,

//TODO: these are dupes: clean this up by splitting simplification
/* -0 -> 0 */
rulematch modelop("neg", [0]) -> 0,

/* a^0 -> 1 */
rulematch $$a.one ^ 0 -> 1,

/* 0^a -> 0 */
rulematch 0 ^ ($$a.one) -> 0

]


LF addRules("simplification", simplification)
LF addRules("factoring", factoring)
LF addRules("expansion", expansion)

LF addRules("restoration", restoration)

end
