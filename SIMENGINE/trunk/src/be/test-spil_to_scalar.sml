functor TestSpilToScalar (T: SPIL_TO_SCALAR) = struct



local 
    open T.Spil
    structure Stm = Statement
    structure Ctl = Control
    structure Op = Operator
    structure A = Atom
    structure B = Block
    structure F = Function
    structure Exp = Expression
    structure Pro = Program

    val ltov = Vector.fromList

    val CDATAFORMAT = Type.gen (Type.real 32)

    val u_OFF = 0
    val w_OFF = 1

    val i_OFF = 0
    val b0_OFF = 1
    val b1_OFF = 2
    val e_OFF = 3

    val fn_u
      = B.BLOCK
	    {label= "fn_u",
	     params= ltov[],
	     body= 
	     ltov[Stm.COMMENT "(1) u' = u + (u^3)/3 + I",
		  Stm.BIND {dest= ("y",CDATAFORMAT), 
			    src= A.Address "fn_states"},
		  Stm.BIND {dest= ("u",CDATAFORMAT), 
			    src= A.Offset {base=A.Variable "y",
					   offset=u_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("I",CDATAFORMAT), 
			    src= A.Offset {base=A.Address "fn_inputs",
					   offset=i_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("dydt",CDATAFORMAT), 
			    src= A.Address "fn_next_states"},
		  Stm.GRAPH {dest= ("dudt",CDATAFORMAT),
			     src= 
			     Exp.Apply 
				 {oper= Op.Float_add,
				  args= 
				  ltov[Exp.Value (A.Variable "u"),
				       Exp.Apply
					   {oper= Op.Float_mul,
					    args= 
					    ltov[Exp.Value (A.Variable "u"),
						 Exp.Value (A.Variable "u"),
						 Exp.Value (A.Variable "u"),
						 Exp.Value (A.Literal (Real 0.3))]},
				       Exp.Value (A.Variable "I")]}},
		  Stm.MOVE {dest= A.Offset {base=A.Variable "dydt",
					    offset=u_OFF,
					    scale=1},
			    src= A.Variable "dudt"}
		 ],
	     transfer= Ctl.JUMP {block= "fn_w",
				 args= ltov[]}
	    }

    val fn_w
      = B.BLOCK
	    {label= "fn_w",
	     params= ltov[],
	     body=
	     ltov[Stm.COMMENT "(2) w' = e * (b0 + b1 * u - w)",
		  Stm.BIND {dest= ("y", CDATAFORMAT), 
			    src= A.Address "fn_states"},
		  Stm.BIND {dest= ("u", CDATAFORMAT),
			    src= A.Offset {base=A.Variable "y",
					   offset=u_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("w", CDATAFORMAT),
			    src= A.Offset {base=A.Variable "y",
					   offset=w_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("e", CDATAFORMAT),
			    src= A.Offset {base=A.Address "fn_inputs",
					   offset=e_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("b0", CDATAFORMAT),
			    src= A.Offset {base=A.Address "fn_inputs",
					   offset=b0_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("b1", CDATAFORMAT),
			    src= A.Offset {base=A.Address "fn_inputs",
					   offset=b1_OFF,
					   scale=1}},
		  Stm.BIND {dest= ("dydt", CDATAFORMAT),
			    src= A.Address "fn_next_states"},
		  Stm.GRAPH {dest= ("dwdt", CDATAFORMAT),
			     src= 
			     Exp.Apply
				 {oper= Op.Float_mul,
				  args=
				  ltov[Exp.Value (A.Variable "e"),
				       Exp.Apply 
					   {oper= Op.Float_add,
					    args= 
					    ltov[Exp.Value (A.Variable "b0"),
						 Exp.Apply 
						     {oper= Op.Float_mul,
						      args=
						      ltov[Exp.Value (A.Variable "b1"),
							   Exp.Value (A.Variable "u")]},
						 Exp.Apply 
						     {oper= Op.Float_neg,
						      args= ltov[Exp.Value (A.Variable "w")]}]}]}},
		  Stm.MOVE {dest= A.Offset {base=A.Variable "dydt",
					    offset=w_OFF,
					    scale=1},
			    src= A.Variable "dwdt"}
		  
		 ],
	     transfer= Ctl.RETURN (A.Null)
	    }

    val flow_fn
      = F.FUNCTION
	    {name= "flow_fn",
	     params= ltov[],
	     start= "fn_u",
	     blocks= ltov[fn_u,fn_w],
	     returns= CDATAFORMAT
	    }

    val main_entry
      = B.BLOCK
	    {label= "main_entry",
	     params= ltov[],
	     body= ltov[],
	     transfer= Ctl.CALL {func= "flow_fn",
				 args= ltov[],
				 return= NONE}
	    }

    val main
      = F.FUNCTION
	    {params= ltov[],
	     name= "main",
	     start= "main_entry",
	     blocks= ltov[main_entry],
	     returns= CDATAFORMAT
	    }

    val program
      = Pro.PROGRAM
	    {functions= ltov[flow_fn],
	     globals= ltov[],
	     main= main,
	     types= ltov[]     
	    }

in
val program = program
end



val lyt = T.spilToScalar program

val () = Layout.print (lyt, TextIO.print)
val () = TextIO.print ("\n")


end

structure T = TestSpilToScalar(SpilToScalar(Spil))
