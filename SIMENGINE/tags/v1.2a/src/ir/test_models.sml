structure TestModels =
struct

open ExpBuild
infix equals;

val fnmodel_exps = [initvar "u" equals (int 1),
		    initvar "w" equals (int 1),
		    diff "u" equals (plus [tvar "u",neg (power (tvar "u", int 3)), neg (tvar "w"), var "I"]),
		    diff "w" equals (times [var "e", plus [var "b0", times [var "b1", tvar "u"], neg (tvar "w")]])];


val fnmodel = fnmodel_exps

fun exp2term (Exp.TERM t) = t
  | exp2term _ = Exp.NAN

val fnmodel_eqs = [{eq_type=DOF.INITIAL_VALUE,
		    sourcepos=PosLog.NOPOS,
		    lhs=exp2term (initvar "u"),
		    rhs=int 1},
		   {eq_type=DOF.INITIAL_VALUE,
		    sourcepos=PosLog.NOPOS,
		    lhs=exp2term (initvar "w"),
		    rhs=int 1},
		   {eq_type=DOF.DERIVATIVE_EQ,
		    sourcepos=PosLog.NOPOS,
		    lhs=exp2term (diff "u"),
		    rhs=plus [tvar "u",neg (power (tvar "u", int 3)), neg (tvar "w"), var "I"]},
		   {eq_type=DOF.DERIVATIVE_EQ,
		    sourcepos=PosLog.NOPOS,
		    lhs=exp2term (diff "w"),
		    rhs=times [var "e", plus [var "b0", times [var "b1", tvar "u"], neg (tvar "w")]]}];
		   

val classproperties = {sourcepos=PosLog.NOPOS}

val inputs = map (fn(name,init)=>(Symbol.symbol name, 
				  {defaultValue=SOME (Exp.REAL init),
				   sourcepos=PosLog.NOPOS}))
		 [("b0", 2.0), ("b1", 1.5), ("e", 0.1), ("I", 2.0)]

val outputs = map (fn(name)=>{name=Symbol.symbol name,
			      contents=[tvar name],
			      condition=Exp.TERM (Exp.BOOL true)})
		  ["u", "w"]

val fn_class : DOF.class = {name=Symbol.symbol "fn",
			    properties=classproperties,
			    inputs=inputs,
			    outputs=ref outputs,
			    eqs=ref fnmodel_eqs}

val fn_inst : DOF.instance = {name=SOME (Symbol.symbol "fninst"),
			      classname=Symbol.symbol "fn"}

val sysproperties : DOF.systemproperties = {solver=Solver.FORWARD_EULER {dt=0.1}}

val fn_model : DOF.model = ([fn_class], fn_inst, sysproperties)

(* Create a test of an FN population *)
val fn_pop_class : DOF.class = {name=Symbol.symbol "fn_pop",
				properties=classproperties,
				inputs=[(Symbol.symbol "ave_I",
					 {defaultValue=SOME (Exp.REAL 2.0),
					  sourcepos=PosLog.NOPOS})],
				outputs=ref [{name=Symbol.symbol "u",
					      contents=[tvar "u1", tvar "u2", tvar "u3"],
					      condition=Exp.TERM (Exp.BOOL true)},
					     {name=Symbol.symbol "w",
					      contents=[tvar "w1", tvar "w2", tvar "w3"],
					      condition=Exp.TERM (Exp.BOOL true)}],
				eqs=ref [{eq_type=DOF.INSTANCE {name=SOME (Symbol.symbol "fn1"),
								classname=Symbol.symbol "fn"},
					  sourcepos=PosLog.NOPOS,
					  lhs=Exp.TUPLE [exp2term (tvar "u1"),
							 exp2term (tvar "w1")],
					  rhs=Exp.FUN (Symbol.symbol "fn",
						       [var "b0", var "b1", var "e", plus [var "ave_I", real (~0.5)]])},
					 {eq_type=DOF.INSTANCE {name=SOME (Symbol.symbol "fn2"),
								classname=Symbol.symbol "fn"},
					  sourcepos=PosLog.NOPOS,
					  lhs=Exp.TUPLE [exp2term (tvar "u2"),
							 exp2term (tvar "w2")],
					  rhs=Exp.FUN (Symbol.symbol "fn",
						       [var "b0", var "b1", var "e", var "ave_I"])},
					 {eq_type=DOF.INTERMEDIATE_EQ,
					  sourcepos=PosLog.NOPOS,
					  lhs=exp2term (var "e"),
					  rhs=real 0.1},
					 {eq_type=DOF.INSTANCE {name=SOME (Symbol.symbol "fn3"),
								classname=Symbol.symbol "fn"},
					  sourcepos=PosLog.NOPOS,
					  lhs=Exp.TUPLE [exp2term (tvar "u3"),
							 exp2term (tvar "w3")],
					  rhs=Exp.FUN (Symbol.symbol "fn",
						       [var "b0", var "b1", var "e", plus [var "ave_I", real 0.5]])},
					 {eq_type=DOF.INTERMEDIATE_EQ,
					  sourcepos=PosLog.NOPOS,
					  lhs=exp2term (var "b0"),
					  rhs=real 2.0},
					 {eq_type=DOF.INTERMEDIATE_EQ,
					  sourcepos=PosLog.NOPOS,
					  lhs=exp2term (var "b1"),
					  rhs=real 1.5}
					 ]
			       }

val fn_pop_inst : DOF.instance = {name=SOME (Symbol.symbol "fnpopinst"),
				  classname=Symbol.symbol "fn_pop"}

val fn_pop_model : DOF.model = ([fn_pop_class, fn_class], fn_pop_inst, sysproperties)

end

