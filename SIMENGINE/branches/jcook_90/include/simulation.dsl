/* Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
 * For more information, please visit http://simatratechnologies.com/
 */
import "integration.dsl"

namespace Simulation
  class ModelTemplate
    var modelclass
  end

  enumeration Bounds {INCLUSIVE, EXCLUSIVE}

  class Precision //TODO: make into an interface
  end

  class InfinitePrecision extends Precision
  end

  class Range extends Precision
    var low
    var high
    var step
    
    constructor (low, high, step)
      self.low = low
      self.high = high
      self.step = step
    end

    constructor (interval: Interval)
      self.low = interval.low
      self.high = interval.high
      self.step = interval.step
    end

    function tostring () = low + ":" + step + ":" + high
  end

  class SimQuantity
    var name
    hidden var precision = InfinitePrecision.new()
    var initialval
    hidden var eq    
    hidden var condEqs = []
    // hidden var currentval
    // hidden var readval // when an exp is read, replace the var with this if not undefined
    // var eq    
    hidden var hasEq = false
    // hidden var isVisible = false
    // hidden var isTunable = false
    // hidden var isIterable = false
    // hidden var isIntermediate = false

    hidden var dimensions = []

    var downsampling

    function setPrecision (p: Precision)
      precision = p
    end

    function getPrecision() = self.precision

    function setInitialValue (v)
      initialval = v
    end

    function getInitialValue()
      if not(isdefined (self.initialval)) then
        error ("Initial value for state '" + name + "' is not specified")
        0
      else
        self.initialval
      end
    end

    function getEquation()
      self.eq
    end

    function getCondEqs()
      self.condEqs
    end

    function hasEquation() = hasEq

    function setEquation(eq: Equation)
      //TODO: perform error checking here, ie if its a param, DONT allow this
//    println("adding eq to " + name)
      self.eq = eq
      self.hasEq = true
    end
    overload function setEquation(eq: EventDrivenEquation)
  //    println("adding conditional eq to " + name)
      self.condEqs.push_front (eq)
    end

    function setDimensions (dimensions: Vector)
      self.dimensions = dimensions
    end

    function getDimensions () = dimensions
 
    function getName () = self.name

    overload operator () (arg: Vector of Number)
      if arg.length() <> dimensions.length() then
        error ("Invalid vector index dimensions on " + name)
      else
        IteratorReference.new(self, arg)
      end
    end

//    function tostring() = eq.tostring()

    function tostring ()
      var str = self.class.name + "("
      str = str + "name=" + self.getName()
      str + ")"
    end

    constructor (name: String)
      self.name = name      
      self.setEquation(Equation.new(self', 0))
//      self.eq = DifferentialEquation.new(1, self, 0) 

//      reset()
    end
  end

  class MathFunction
    hidden var name

    constructor (name: String, thunk)
      self.name = name

      self.addMethod ("()", thunk)
    end    
  end  

  class Event extends SimQuantity
    var name
    var condition

    constructor (name: String, condition)
      self.name = name
      self.condition = condition
    end    

    function tostring() = name + " when " + condition.tostring()
  end  


  class Equation
    var lhs
    var rhs

    constructor (lhs, rhs)
      self.lhs = lhs
      self.rhs = rhs
    end

    function tostring() = lhs.tostring() + " = " + rhs.tostring()
  end  

  class EventDrivenEquation extends Equation
    var cond

    constructor (lhs, rhs, cond)
      super(lhs, rhs)
      self.cond = cond
    end

    function tostring() = lhs.tostring() + " = " + rhs.tostring() + " when " + cond.tostring()
  end



  class ModelOperation
    var name
    var numArgs
    var execFun
    var precisionMap
    var args
   
    function tostring ()
      //"ModelOperation(name=" + self.name + ", args=" + self.args + ")"
      LF exp2str (self)
    end

    constructor (name: String, numArgs: Number, execFun, precisionMap, args: Vector of _)
      self.name = name
      self.numArgs = numArgs
      self.execFun = execFun
      self.precisionMap = precisionMap
      self.args = args

    end

    function getDimensions ()      // hack to make derivatives work like symbols
      if (self.name == "deriv") then
        args[2].getDimensions()
      else
        error "getDimensions is not supported on this operation"
      end
    end   

    overload operator () (arg: Vector)
      if self.name == "deriv" then
      operator_deriv(args[1], IteratorReference.new(args[2], arg))
      else
        error "Cannot use iterators to index into an expression"
      end
    end


  end

  function modelop (name, args) = ModelOperation.new(name, args.length(), (lambdafun (x) = modelop (name, args)), 0, args)

  overload function not(b: ModelOperation) = ModelOperation.new ("not", 1, not, 0, [b])
  overload function not(b: SimQuantity) = ModelOperation.new ("not", 1, not, 0, [b])

  overload function operator_add(arg1: ModelOperation, arg2) = {arg1 when arg2 == 0, ModelOperation.new ("add", 2, operator_add, 0, [arg1, arg2]) otherwise}
  overload function operator_add(arg1, arg2: ModelOperation) = {arg2 when arg1 == 0, ModelOperation.new ("add", 2, operator_add, 0, [arg1, arg2]) otherwise}
  overload function operator_add(arg1: SimQuantity, arg2) = {arg1 when arg2 == 0, ModelOperation.new ("add", 2, operator_add, 0, [arg1, arg2]) otherwise}
  overload function operator_add(arg1, arg2: SimQuantity) = {arg2 when arg1 == 0, ModelOperation.new ("add", 2, operator_add, 0, [arg1, arg2]) otherwise}
   
  overload function operator_neg(arg: ModelOperation) = ModelOperation.new ("neg", 1, operator_neg, 0, [arg])
  overload function operator_neg(arg: SimQuantity) =    ModelOperation.new ("neg", 1, operator_neg, 0, [arg])

  overload function operator_subtract(arg1: ModelOperation, arg2) = {arg1 when arg2 == 0, ModelOperation.new ("sub", 2, operator_subtract, 0, [arg1, arg2]) otherwise}
  overload function operator_subtract(arg1, arg2: ModelOperation) = {-arg2 when arg1 == 0, ModelOperation.new ("sub", 2, operator_subtract, 0, [arg1, arg2]) otherwise}
  overload function operator_subtract(arg1: SimQuantity, arg2) = {arg1 when arg2 == 0, ModelOperation.new ("sub", 2, operator_subtract, 0, [arg1, arg2]) otherwise}
  overload function operator_subtract(arg1, arg2: SimQuantity) = {-arg2 when arg1 == 0, ModelOperation.new ("sub", 2, operator_subtract, 0, [arg1, arg2]) otherwise}
  
  overload function operator_add(arg1: GenericIterator, arg2: Number) = {arg1 when arg2 == 0, RelativeOffset.new (arg1, arg2) otherwise}
  overload function operator_add(arg1: Number, arg2: GenericIterator) = {arg2 when arg1 == 0, RelativeOffset.new (arg2, arg1) otherwise}
  overload function operator_add(arg1: SimIterator, arg2: Number) = {arg1 when arg2 == 0, RelativeOffset.new (arg1, arg2) otherwise}
  overload function operator_add(arg1: Number, arg2: SimIterator) = {arg2 when arg1 == 0, RelativeOffset.new (arg2, arg1) otherwise}
   
  overload function operator_subtract(arg1: SimIterator, arg2: Number) = {arg1 when arg2 == 0, IteratorOperation.new (arg1, -arg2) otherwise}
  overload function operator_subtract(arg1: GenericIterator, arg2: Number) = {arg1 when arg2 == 0, RelativeOffset.new (arg1, -arg2) otherwise}


  overload function operator_multiply(arg1: ModelOperation, arg2) = {arg2 when arg2 == 0,
                                                                     arg1 when arg2 == 1,
                                                                     -arg1 when arg2 == -1,
                                                                     ModelOperation.new ("mul", 2, operator_multiply, 0, [arg1, arg2]) otherwise}

  overload function operator_multiply(arg1, arg2: ModelOperation) = {arg1 when arg1 == 0,
                                                                     arg2 when arg1 == 1, 
								     -arg2 when arg1 == -1,
                                                                     ModelOperation.new ("mul", 2, operator_multiply, 0, [arg1, arg2]) otherwise}

  overload function operator_multiply(arg1: SimQuantity, arg2) = {arg2 when arg2 == 0,
                                                                  arg1 when arg2 == 1,
                                                                  -arg1 when arg2 == -1,
                                                                  ModelOperation.new ("mul", 2, operator_multiply, 0, [arg1, arg2]) otherwise}

  overload function operator_multiply(arg1, arg2: SimQuantity) = {arg1 when arg1 == 0,
                                                                  arg2 when arg1 == 1, 
								  -arg2 when arg1 == -1,
                                                                  ModelOperation.new ("mul", 2, operator_multiply, 0, [arg1, arg2]) otherwise}

  overload function operator_divide(arg1: ModelOperation, arg2) = {arg1 when arg2 == 1, 
                                                                   -arg1 when arg2 == -1,
                                                                   ModelOperation.new ("divide", 2, operator_divide, 0, [arg1, arg2]) otherwise}

  overload function operator_divide(arg1, arg2: ModelOperation) = {arg1 when arg1 == 0, ModelOperation.new ("divide", 2, operator_divide, 0, [arg1, arg2]) otherwise}

  overload function operator_divide(arg1: SimQuantity, arg2) = {arg1 when arg2 == 1, 
                                                                -arg1 when arg2 == -1,
                                                                ModelOperation.new ("divide", 2, operator_divide, 0, [arg1, arg2]) otherwise}

  overload function operator_divide(arg1, arg2: SimQuantity) = {arg1 when arg1 == 0, ModelOperation.new ("divide", 2, operator_divide, 0, [arg1, arg2]) otherwise}

  overload function operator_modulus(arg1: ModelOperation, arg2) = ModelOperation.new ("modulus", 2, operator_modulus, 0, [arg1, arg2])
  overload function operator_modulus(arg1, arg2: ModelOperation) = ModelOperation.new ("modulus", 2, operator_modulus, 0, [arg1, arg2])
  overload function operator_modulus(arg1: SimQuantity, arg2) = ModelOperation.new ("modulus", 2, operator_modulus, 0, [arg1, arg2])
  overload function operator_modulus(arg1, arg2: SimQuantity) = ModelOperation.new ("modulus", 2, operator_modulus, 0, [arg1, arg2])
  
  function operator_deriv(degree: Number, arg: ModelOperation) = ModelOperation.new("deriv", 2, operator_deriv, 0, [degree, arg])
  overload function operator_deriv(degree: Number, arg: SimQuantity) = ModelOperation.new("deriv", 2, operator_deriv, 0, [degree, arg])

  overload function abs(arg: ModelOperation) = ModelOperation.new ("abs", 1, abs, 0, [arg])
  overload function abs(arg: SimQuantity) = ModelOperation.new ("abs", 1, abs, 0, [arg])

  overload function power(arg1: ModelOperation, arg2) = ModelOperation.new ("pow", 2, power, 0, [arg1, arg2])
  overload function power(arg1, arg2: ModelOperation) = ModelOperation.new ("pow", 2, power, 0, [arg1, arg2])
  overload function power(arg1: SimQuantity, arg2) = ModelOperation.new ("pow", 2, power, 0, [arg1, arg2])
  overload function power(arg1, arg2: SimQuantity) = ModelOperation.new ("pow", 2, power, 0, [arg1, arg2])

  overload function sum(arg: ModelOperation) = ModelOperation.new ("reduction_add", 1, sum, 0, [arg])
  overload function sum(arg: SimQuantity) =    ModelOperation.new ("reduction_add", 1, sum, 0, [arg])

  function plus(args: Vector) = ModelOperation.new ("add", args.length(), plus, 0, args)

  function times(args: Vector) = ModelOperation.new ("mul", args.length(), times, 0, args)

  overload function prod(arg: ModelOperation) = ModelOperation.new ("reduction_mul", 1, prod, 0, [arg])
  overload function prod(arg: SimQuantity) =    ModelOperation.new ("reduction_mul", 1, prod, 0, [arg])

  overload function exp(arg: ModelOperation) = ModelOperation.new ("exp", 1, exp, 0, [arg])
  overload function exp(arg: SimQuantity) =    ModelOperation.new ("exp", 1, exp, 0, [arg])

  overload function sqrt(arg: ModelOperation) = ModelOperation.new ("sqrt", 1, sqrt, 0, [arg])
  overload function sqrt(arg: SimQuantity) = ModelOperation.new ("sqrt", 1, sqrt, 0, [arg])

  overload function ln(arg: ModelOperation) = ModelOperation.new ("log", 1, ln, 0, [arg])
  overload function ln(arg: SimQuantity) = ModelOperation.new ("log", 1, ln, 0, [arg])

  overload function log10(arg: ModelOperation) = ModelOperation.new ("log10", 1, log10, 0, [arg])
  overload function log10(arg: SimQuantity) = ModelOperation.new ("log10", 1, log10, 0, [arg])

  overload function logn(arg: ModelOperation) = ModelOperation.new ("logn", 2, logn, 0, [arg])
  overload function logn(arg: SimQuantity) = ModelOperation.new ("logn", 2, logn, 0, [arg])

  overload function deg2rad (arg: ModelOperation) = ModelOperation.new ("deg2rad", 1, deg2rad, 0, [arg])
  overload function deg2rad (arg: SimQuantity) = ModelOperation.new ("deg2rad", 1, deg2rad, 0, [arg])

  overload function rad2deg (arg: ModelOperation) = ModelOperation.new ("rad2deg", 1, rad2deg, 0, [arg])
  overload function rad2deg (arg: SimQuantity) = ModelOperation.new ("rad2deg", 1, rad2deg, 0, [arg])

  // Trigonometry

  overload function sin(arg: ModelOperation) = ModelOperation.new ("sin", 1, sin, 0, [arg])
  overload function sin(arg: SimQuantity) = ModelOperation.new ("sin", 1, sin, 0, [arg])

  overload function cos(arg: ModelOperation) = ModelOperation.new ("cos", 1, cos, 0, [arg])
  overload function cos(arg: SimQuantity) = ModelOperation.new ("cos", 1, cos, 0, [arg])
 
  overload function tan(arg: ModelOperation) = ModelOperation.new ("tan", 1, tan, 0, [arg])
  overload function tan(arg: SimQuantity) = ModelOperation.new ("tan", 1, tan, 0, [arg])

  overload function sinh(arg: ModelOperation) = ModelOperation.new ("sinh", 1, sinh, 0, [arg])
  overload function sinh(arg: SimQuantity) = ModelOperation.new ("sinh", 1, sinh, 0, [arg])

  overload function cosh(arg: ModelOperation) = ModelOperation.new ("cosh", 1, cosh, 0, [arg])
  overload function cosh(arg: SimQuantity) = ModelOperation.new ("cosh", 1, cosh, 0, [arg])

  overload function tanh(arg: ModelOperation) = ModelOperation.new ("tanh", 1, tanh, 0, [arg])
  overload function tanh(arg: SimQuantity) = ModelOperation.new ("tanh", 1, tanh, 0, [arg])

  overload function asin(arg: ModelOperation) = ModelOperation.new ("asin", 1, asin, 0, [arg])
  overload function asin(arg: SimQuantity) = ModelOperation.new ("asin", 1, asin, 0, [arg])

  overload function acos(arg: ModelOperation) = ModelOperation.new ("acos", 1, acos, 0, [arg])
  overload function acos(arg: SimQuantity) = ModelOperation.new ("acos", 1, acos, 0, [arg])

  overload function atan(arg: ModelOperation) = ModelOperation.new ("atan", 1, atan, 0, [arg])
  overload function atan(arg: SimQuantity) = ModelOperation.new ("atan", 1, atan, 0, [arg])

  overload function atan2(arg1: ModelOperation, arg2) = ModelOperation.new ("atan2", 2, atan2, 0, [arg1, arg2])
  overload function atan2(arg1, arg2: ModelOperation) = ModelOperation.new ("atan2", 2, atan2, 0, [arg1, arg2])
  overload function atan2(arg1: SimQuantity, arg2) = ModelOperation.new ("atan2", 2, atan2, 0, [arg1, arg2])
  overload function atan2(arg1, arg2: SimQuantity) = ModelOperation.new ("atan2", 2, atan2, 0, [arg1, arg2])

  overload function asinh(arg: ModelOperation) = ModelOperation.new ("asinh", 1, asinh, 0, [arg])
  overload function asinh(arg: SimQuantity) = ModelOperation.new ("asinh", 1, asinh, 0, [arg])

  overload function acosh(arg: ModelOperation) = ModelOperation.new ("acosh", 1, acosh, 0, [arg])
  overload function acosh(arg: SimQuantity) = ModelOperation.new ("acosh", 1, acosh, 0, [arg])

  overload function atanh(arg: ModelOperation) = ModelOperation.new ("atanh", 1, atanh, 0, [arg])
  overload function atanh(arg: SimQuantity) = ModelOperation.new ("atanh", 1, atanh, 0, [arg])

  overload function csc(arg: ModelOperation) = ModelOperation.new ("csc", 1, csc, 0, [arg])
  overload function csc(arg: SimQuantity) = ModelOperation.new ("csc", 1, csc, 0, [arg])

  overload function sec(arg: ModelOperation) = ModelOperation.new ("sec", 1, sec, 0, [arg])
  overload function sec(arg: SimQuantity) = ModelOperation.new ("sec", 1, sec, 0, [arg])

  overload function cot(arg: ModelOperation) = ModelOperation.new ("cot", 1, cot, 0, [arg])
  overload function cot(arg: SimQuantity) = ModelOperation.new ("cot", 1, cot, 0, [arg])

  overload function csch(arg: ModelOperation) = ModelOperation.new ("csch", 1, csch, 0, [arg])
  overload function csch(arg: SimQuantity) = ModelOperation.new ("csch", 1, csch, 0, [arg])

  overload function sech(arg: ModelOperation) = ModelOperation.new ("sech", 1, sech, 0, [arg])
  overload function sech(arg: SimQuantity) = ModelOperation.new ("sech", 1, sech, 0, [arg])

  overload function coth(arg: ModelOperation) = ModelOperation.new ("coth", 1, coth, 0, [arg])
  overload function coth(arg: SimQuantity) = ModelOperation.new ("coth", 1, coth, 0, [arg])

  overload function acsc(arg: ModelOperation) = ModelOperation.new ("acsc", 1, acsc, 0, [arg])
  overload function acsc(arg: SimQuantity) = ModelOperation.new ("acsc", 1, acsc, 0, [arg])

  overload function asec(arg: ModelOperation) = ModelOperation.new ("asec", 1, asec, 0, [arg])
  overload function asec(arg: SimQuantity) = ModelOperation.new ("asec", 1, asec, 0, [arg])

  overload function acot(arg: ModelOperation) = ModelOperation.new ("acot", 1, acot, 0, [arg])
  overload function acot(arg: SimQuantity) = ModelOperation.new ("acot", 1, acot, 0, [arg])

  overload function acsch(arg: ModelOperation) = ModelOperation.new ("acsch", 1, acsch, 0, [arg])
  overload function acsch(arg: SimQuantity) = ModelOperation.new ("acsch", 1, acsch, 0, [arg])

  overload function asech(arg: ModelOperation) = ModelOperation.new ("asech", 1, asech, 0, [arg])
  overload function asech(arg: SimQuantity) = ModelOperation.new ("asech", 1, asech, 0, [arg])

  overload function acoth(arg: ModelOperation) = ModelOperation.new ("acoth", 1, acoth, 0, [arg])
  overload function acoth(arg: SimQuantity) = ModelOperation.new ("acoth", 1, acoth, 0, [arg])



  //relational
  overload function operator_gt(arg1: ModelOperation, arg2) = ModelOperation.new ("gt", 2, operator_gt, 0, [arg1, arg2])
  overload function operator_gt(arg1, arg2: ModelOperation) = ModelOperation.new ("gt", 2, operator_gt, 0, [arg1, arg2])
  overload function operator_gt(arg1: SimQuantity, arg2) = ModelOperation.new ("gt", 2, operator_gt, 0, [arg1, arg2])
  overload function operator_gt(arg1, arg2: SimQuantity) = ModelOperation.new ("gt", 2, operator_gt, 0, [arg1, arg2])

  overload function operator_ge(arg1: ModelOperation, arg2) = ModelOperation.new ("ge", 2, operator_ge, 0, [arg1, arg2])
  overload function operator_ge(arg1, arg2: ModelOperation) = ModelOperation.new ("ge", 2, operator_ge, 0, [arg1, arg2])
  overload function operator_ge(arg1: SimQuantity, arg2) = ModelOperation.new ("ge", 2, operator_ge, 0, [arg1, arg2])
  overload function operator_ge(arg1, arg2: SimQuantity) = ModelOperation.new ("ge", 2, operator_ge, 0, [arg1, arg2])

  overload function operator_lt(arg1: ModelOperation, arg2) = ModelOperation.new ("lt", 2, operator_lt, 0, [arg1, arg2])
  overload function operator_lt(arg1, arg2: ModelOperation) = ModelOperation.new ("lt", 2, operator_lt, 0, [arg1, arg2])
  overload function operator_lt(arg1: SimQuantity, arg2) = ModelOperation.new ("lt", 2, operator_lt, 0, [arg1, arg2])
  overload function operator_lt(arg1, arg2: SimQuantity) = ModelOperation.new ("lt", 2, operator_lt, 0, [arg1, arg2])

  overload function operator_le(arg1: ModelOperation, arg2) = ModelOperation.new ("le", 2, operator_le, 0, [arg1, arg2])
  overload function operator_le(arg1, arg2: ModelOperation) = ModelOperation.new ("le", 2, operator_le, 0, [arg1, arg2])
  overload function operator_le(arg1: SimQuantity, arg2) = ModelOperation.new ("le", 2, operator_le, 0, [arg1, arg2])
  overload function operator_le(arg1, arg2: SimQuantity) = ModelOperation.new ("le", 2, operator_le, 0, [arg1, arg2])

  overload function operator_eq(arg1: ModelOperation, arg2) = ModelOperation.new ("eq", 2, operator_eq, 0, [arg1, arg2])
  overload function operator_eq(arg1, arg2: ModelOperation) = ModelOperation.new ("eq", 2, operator_eq, 0, [arg1, arg2])
  overload function operator_eq(arg1: SimQuantity, arg2) = ModelOperation.new ("eq", 2, operator_eq, 0, [arg1, arg2])
  overload function operator_eq(arg1, arg2: SimQuantity) = ModelOperation.new ("eq", 2, operator_eq, 0, [arg1, arg2])

  overload function operator_ne(arg1: ModelOperation, arg2) = ModelOperation.new ("neq", 2, operator_ne, 0, [arg1, arg2])
  overload function operator_ne(arg1, arg2: ModelOperation) = ModelOperation.new ("neq", 2, operator_ne, 0, [arg1, arg2])
  overload function operator_ne(arg1: SimQuantity, arg2) = ModelOperation.new ("neq", 2, operator_ne, 0, [arg1, arg2])
  overload function operator_ne(arg1, arg2: SimQuantity) = ModelOperation.new ("neq", 2, operator_ne, 0, [arg1, arg2])

  function operator_noop (arg) = ModelOperation.new("noop", 1, operator_noop, 0, [arg])


  function branch (condition: ModelOperation, success: () -> _, failure: () -> _)
    multifunction
      optbranch (ift, iff) = ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff])

      /* Literal boolean consequents may be short-circuited. */
      optbranch (ift: _, iff: Boolean) = { ModelOperation.new("and",2,(lambdafun (a,b) = {b when a, false otherwise}),0,[condition,ift]) when not(iff),
        ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff]) otherwise
      }

      optbranch (ift: Boolean, iff: _) = { ModelOperation.new("or",2,(lambdafun (a,b) = {a when a, b otherwise}),0,[condition,iff]) when ift,
        ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff]) otherwise
      }

      optbranch (ift: Boolean, iff: Boolean) = { true when ift and iff,
        false when not(ift or iff),
        condition when ift and not(iff),
        ModelOperation.new("not",1,not,0,[condition]) otherwise
      }
    end
    
    optbranch(success(), failure())
  end

  overload function branch (condition: SimQuantity, success: () -> _, failure: () -> _)
    multifunction
      optbranch (ift, iff) = ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff])

      /* Literal boolean consequents may be short-circuited. */
      optbranch (ift: _, iff: Boolean) = { ModelOperation.new("and",2,(lambdafun (a,b) = {b when a, false otherwise}),0,[condition,ift]) when not(iff),
        ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff]) otherwise
      }

      optbranch (ift: Boolean, iff: _) = { ModelOperation.new("or",2,(lambdafun (a,b) = {a when a, b otherwise}),0,[condition,iff]) when ift,
        ModelOperation.new("if",3,(lambdafun (c,a,b) = {a when c, b otherwise}),0,[condition,ift,iff]) otherwise
      }

      optbranch (ift: Boolean, iff: Boolean) = { true when ift and iff,
        false when not(ift or iff),
        condition when ift and not(iff),
        ModelOperation.new("not",1,not,0,[condition]) otherwise
      }
    end
    
    optbranch(success(), failure())
  end

  class Intermediate extends SimQuantity
    property output_frequency
      set (frequency)
        downsampling = Downsampling.minmax(frequency)
      end
    end

    constructor (name: String)
      super(name)
    end

    overload operator () (arg: Vector)
      IteratorReference.new(self, arg)
    end
  end

  class State extends SimQuantity
    var downsampling
    hidden var h_iter

    property iter
      get = h_iter
      set (i)
        h_iter = i
	if h_iter.isContinuous then
	  self.eq = Equation.new(self', 0)
	else
	  self.eq = Equation.new(self[h_iter], self[h_iter-1])
	end
      end
    end

    property output_frequency
      set (frequency)
        downsampling = Downsampling.minmax(frequency)
      end
    end

    constructor (name: String)
      super(name)
    end

    overload operator () (arg: Vector)
      IteratorReference.new(self, arg)
    end

  end

  class RandomValue extends SimQuantity
      var normal = false
  end

  class Random extends State
//      var mean = 1
//      var stddev = 0.1
      var seed = 1
//      var high = 1
//      var low = 0
      var isNormal = false
      var isUniform = true
      hidden var h_high = 1
      hidden var h_low = 0
      hidden var h_mean = 1
      hidden var h_stddev = 0.1

      constructor (name)
	  super(name)
      end      

      property iter
	  get = h_iter
	  set (i)
	      h_iter = i
	      self.eq = getEq()
	  end
      end

      property high
	  get = h_high
	  set (h)
	      h_high = h
	      self.eq = getEq()
	  end
      end

      property low
	  get = h_low
	  set (l)
	      h_low = l
	      self.eq = getEq()
	  end
      end

      property mean
	  get = h_mean
	  set (h)
	      h_mean = h
	      self.eq = getEq()
	  end
      end

      property stddev
	  get = h_stddev
	  set (l)
	      h_stddev = l
	      self.eq = getEq()
	  end
      end

      property normal
	  get = isNormal
	  set (c)
	      isNormal = c
	      if isNormal then
		  isUniform = false
	      end		  
	      self.eq = getEq()
	  end
      end

      property uniform
	  get = isUniform
	  set (c)
	      isUniform = c
	      if isUniform then
		  isNormal = false
	      end		  
	      self.eq = getEq()
	  end
      end

      function getInitialValue()
         // we want to grab the rhs, but this is not supported, so let's just make it zero
         //getEq().rhs
         0
      end

      function getEq()
//        Equation.new(self[h_iter], ModelOperation.new ("add", 2, plus, 0, [ModelOperation.new ("mul", 2, operator_multiply, 0, [high-low, rand]), low]))
        var r
	r = RandomValue.new()
        if isNormal then
	    r.normal = true
            Equation.new(self[h_iter], stddev * r + mean)
	else
            // Equation.new(self[h_iter], ModelOperation.new ("mul", 2, operator_multiply, 0, [high-low, RandomValue.new()]) + low)
            Equation.new(self[h_iter], (high-low) * r + low)
	end
      end
  end

  class Symbol extends State 
    constructor (name)
      super(name)
    end    
  end

  class SymbolPattern extends State
    var min = 1
    var max = 1

    property one
      get 
        min=1
        max=1
        self
      end
    end

    property some
      get 
        min=1
        max=-1
        self
      end
    end

    property any
      get 
        min=0
        max=-1
        self
      end
    end

    function exactly (num)
      min=num
      max=num
      self
    end

    function between (range: Interval)
      min=range.low
      max=range.high
      self
    end

    constructor (name)
      super(name)
    end    
  end



  class IteratorReference extends SimQuantity
    var referencedQuantity
    var indices

    constructor (rquant, indices)
      referencedQuantity = rquant
      self.indices = indices      
    end
    function getName() = referencedQuantity.getName() + indices
  end

  class RelativeOffset extends ModelOperation
    var simIterator
    var step
    
    constructor (simIterator: GenericIterator, step: Number)
      self.simIterator = simIterator
      self.step = step

      self.name = "add"
      self.numArgs = 2
      self.execFun = operator_add
      self.precisionMap = 0
      self.args = [simIterator, step]
    end    
  end

  class GenericIterator extends SimQuantity
    constructor (name: String)
      super(name)
    end
  end

  // down here because it needs to see definition of + with model operations
  class SimIterator extends GenericIterator
    var value
    constructor (name: String)
      super(name)
    end 

    function setValue(v:Interval)
      value = v
    end
    function getValue() = value

    property low
      get = value.low
    end
    property high
      get = value.high
    end

  end

  class TimeIterator extends GenericIterator
    var isContinuous
    var sample_frequency = 1
    constructor (name: String)
      super(name)
    end 

    hidden var solver_obj

    property solver
      get
        if (not (isdefined solver_obj)) then
          warning "No solver specified for continuous iterator '" + name + "', using default ode45 solver"
          ode45
        else
          solver_obj
        end        
      end

      set(s)
        solver_obj = s
      end
    end

    property sample_period 
      get = 1 / sample_frequency
      set(p)
        sample_frequency = 1/p
      end
    end

    function setValue(v)
      error "Temporal iterators cannot have assigned ranges of values"
    end

    property continuous
      get = isContinuous
      set (c)
        isContinuous = c
      end
    end

    property discrete
      get = not isContinuous
      set (d)
        isContinuous = not d
      end
    end

    overload operator () (arg: Vector of Number)
      PreviousTimeIterator.new(self, arg)
    end

  end

  class PreviousTimeIterator extends TimeIterator
    var timeIterator
    var index

    constructor (titer, indices: Vector of Number)
      self.timeIterator = titer

      self.isContinuous = titer.isContinuous
      self.sample_frequency = titer.sample_frequency
      
      self.name = titer.name

      if indices.length() <> 1 then
        error "References to previous values of Temporal Iterator " + timeIterator + " must be 1 dimensional"
      else
        self.index = indices[1]
      end
    end
    function getName() = timeIterator.getName() + "[" + index + "]"
  end

  function makeIterator (name, table)
    var iter
    if (objectContains (table, "continuous") and table.continuous) or (objectContains (table, "discrete") and table.discrete) then
      iter = TimeIterator.new(name)
      iter table
    else
      iter = SimIterator.new(name)
      iter table
    end
  end

  class Output
    var name
    var contents
    var condition
    var iter = ()

    function setIter(i)
      iter = i
      self
    end

    constructor (name, contents/*, condition*/)
      self.name = name  
      self.contents = contents
      self.condition = true
    end
  end

  class OutputBinding extends SimQuantity
    var name
    var instanceName
    var outputDef

    constructor (name, def)
      self.name = name
      outputDef = def
    end

    function setInstanceName(instanceName)
      self.instanceName = instanceName
    end
  end

  class InputBinding 
    var inputDef
    var inputValue

    constructor (def)
      inputDef = def
      inputValue = NaN
    end

    property inputVal
      get
        {inputDef.default when istype (type Number, inputValue) and inputValue == NaN,
         inputValue otherwise}        
      end
      set(iv)
        inputValue = iv
      end
    end

    function setInputVal(val)
      inputVal = val
    end

    function tostring ()
      var str = self.class.name + "("
      str = str + "name=" + self.inputDef.getName()
      str = str + ", value=" + self.inputVal
      str + ")"
    end

  end

  class Input extends SimQuantity
    var name
    var defaultValue

    constructor (name)
      self.name = name
      defaultValue = NaN
    end

    property default
      get
        defaultValue
      end
      set(dv)
        if (dv == NaN) then
          error ("Cannot set default value of input "+name+" to NaN")
        else
          defaultValue = dv
        end
      end
    end
    
  end

  class ModelInstance
    var inputs = []
    var outputs = []
    var dimensions = []
    constructor ()
    end

    var name

    function getDimensions () = dimensions

    function setName(name)
      self.name = name
      foreach out in outputs do
        out.setInstanceName name
      end
    end
  end

  class Model

    function tostring()
      var s = "Model " + name + "\n"
      s = s + "  Submodels:\n"
      foreach sm in submodels do
        s = s + "    " + sm.tostring() + "\n"
      end      
      s = s + "  Equations:\n"
      foreach q in quantities do
        s = s + "    " + q.tostring() + "\n"
      end
      
      s = s + "end model " + name + "\n"
      s
    end

    hidden var userMembers = []
    overload function addConst (name: String, value)
      if exists m in userMembers suchthat m == name then
        error ("Model member " + name + " has already been defined.")
      else
        LF addconst (self, name, value)
	userMembers.push_back(name)
      end
    end

    var quantities = []
    var iterators = []
    var submodels = []
    var outputs = {}
    hidden var outputDefs = {} // a placeholder to throw outputs into as we compute them.  outputs will be populated by this after the model stms are run

    hidden function buildOutput(name)
      if exists out in outputs.keys suchthat out == name then
        error ("Duplicate output " + name + ". Each output must have a unique name.")
      elseif exists out in outputDefs.keys suchthat out == name then
        outputs.add(name, outputDefs.getValue(name) ())
      elseif objectContains(self, name) then
        outputs.add(name, self.getMember(name))
      else
        error ("No quantity found matching output in header: " + name + ".  Available outputs are: " + (outputDefs.keys))
      end
    end

    function getSpatialIterators ()
      function isSpatialIterator (x) = false
      overload function isSpatialIterator (x: SimIterator) = true

      [iter foreach iter in iterators when isSpatialIterator iter]
    end

    function getTemporalIterators()
      function isTemporalIterator (x) = false
      overload function isTemporalIterator (x: TimeIterator) = true
     
      var localiters = [iter foreach iter in iterators when isTemporalIterator iter]

      var iters = localiters

      foreach sub in submodels do
        foreach iter in sub.modeltemplate.getTemporalIterators() do
	  if exists i in iters suchthat iter.name == i.name then
	    // TODO: verify global uniqueness per name
	  else
	    iters.push_back iter
	  end
	end
      end

      iters
    end

    var inputs = []

    //initialization
    var t
    var n

    property solver 
      set (s)
        //warning "Setting the solver per model is deprecated.  Please specify a solver for a temporal iterator directly."
	t.solver = s
      end
      get = t.solver
    end

    constructor ()
      t = TimeIterator.new("t") {isContinuous=true}
      n = TimeIterator.new("n") {isContinuous=false}
      iterators.push_front n
      iterators.push_front t
    end

    // helper functions

    function getLocalQuantities()
      quantities
    end

    function makeEquation (name, dimensions, lhs, rhs, cond, condvalid)
      if objectContains(self, name) then
        var q = self.getMember name
        if istype(type SimQuantity, q) then
//	  println ("  it's optcond = " + optcond)
	  if not condvalid then
       	    q.setEquation (Equation.new(lhs q, rhs q))
          else
	    q.setEquation (EventDrivenEquation.new(lhs q, rhs q, cond))
          end
        else
          error "Cannot create equation for non-quantity: " + name
        end
      else
//        println ("Adding a new equation for: " + name)
	var q = Intermediate.new(name)
        self.addConst(name, q)
	if not (istype (type Vector of SimIterator, dimensions)) then
	   error "Indexes on an intermediate equation must be iterators"
        else
          q.dimensions = [d.name foreach d in dimensions]
        end
	
	self.quantities.push_back (q)
	if not condvalid then
          self.getMember(name).setEquation(Equation.new(lhs q, rhs q))        
        else
  	  self.getMember(name).setEquation(EventDrivenEquation.new(lhs q, rhs q, cond))        
        end
      end
    end

    overload function makeEquation (name, dimensions, lhs, rhs, cond)
      makeEquation (name, dimensions, lhs, rhs, cond, true)
    end

    overload function makeEquation (name, dimensions, lhs, rhs)
      makeEquation (name, dimensions, lhs, rhs, (), false)
    end


    function getSubModels()
      submodels
    end

    function instantiateSubModel (mod, name:String, table: Table, dimensions: Vector)
      var m = mod.instantiate()
      self.addConst (name, m)

      m.setName(name)
      m.dimensions = dimensions

      submodels.push_back (m)


      //set inputs
      foreach k in table.keys do
        if not (objectContains (m, k)) then
          error ("submodel " + name + " does not contain input or property " + k)
        else
          var value = m.getMember(k)
          if not (istype (type InputBinding, value)) then
            //ignore for now
          else
            value.setInputVal(table.getValue(k))
          end
        end
      end
    end
  end
  
  class Rule 
      var find
      var conds
      var replacement
      constructor (find, conds, replacement)
        self.find = find
        self.conds = conds
        self.replacement = replacement
      end      
  end

  import "downsampling.dsl"

  function applyRewriteExp (rule: Rule, exp) = LF applyRewriteExp (rule, exp)
  function applyRewritesExp (rule: Vector of Rule, exp) = LF applyRewritesExp (rule, exp)
  function repeatApplyRewriteExp (rule: Rule, exp) = LF repeatApplyRewriteExp (rule, exp)
  function repeatApplyRewritesExp (rule: Vector of Rule, exp) = LF repeatApplyRewritesExp (rule, exp)

end

open Simulation

import "translate.dsl"
import "simcompile.dsl"

function compile2 (filename: String, settings: Table)
  if "dsl" <> LF path_ext (filename) then
      error ("Unknown type of file " + filename)
  end
  var dir = LF path_dir (filename)
  var file = LF path_file (filename)
  var modtime = LF modtime (filename)
  var simfile = (LF path_base file) + ".sim"
  var simmodtime = LF modtime simfile

  if LF isfile (simfile) then
      if modtime > simmodtime or Sys.buildTime > simmodtime then
	  var mod = LF loadModel (filename)
	  mod.template.settings = settings
	  compile mod
      else
	  var simsettings = LF simfileSettings simfile
	  if not (settings.target == simsettings.target and
		  settings.precision == simsettings.precision and
		  settings.num_models == simsettings.num_models) then
	      var mod = LF loadModel (filename)
	      mod.template.settings = settings
	      compile mod
	  else
	      "Compilation Finished Successfully"
	  end
      end
  else
      var mod = LF loadModel (filename)
      mod.template.settings = settings
      compile mod
  end
end

function compile (mod)
  var name = mod.template.name
  var settings = mod.template.settings

  var target

  if "cpu" == settings.target then
      target = SimCompile.TargetCPU.new()
  elseif "openmp" == settings.target then
      target = SimCompile.TargetOpenMP.new()
  elseif "parallelcpu" == settings.target then
      target = SimCompile.TargetOpenMP.new()
  elseif "cuda" == settings.target then
      target = SimCompile.TargetCUDA.new()
      target.emulate = settings.emulate
  elseif "gpu" == settings.target then
      target = SimCompile.TargetCUDA.new()
      target.emulate = settings.emulate
  else
      error ("Unknown target " + settings.target)
  end

  target.debug = settings.debug
  target.profile = settings.profile

  target.num_models = settings.num_models
  target.precision = settings.precision

  var stat = LF compile (Translate.model2forest (mod.instantiate()))

  var cc
  if "gpu" <> settings.target and "cuda" <> settings.target then
      cc = target.compile(name + "_parallel.o", [name + "_parallel.c"])
  else
      SimCompile.shell("ln", ["-s", name + "_parallel.c", name + "_parallel.cu"])
      cc = target.compile(name + "_parallel.o", [name + "_parallel.cu"])
  end


  var ld = target.link(name + ".sim", name + ".sim", [name + "_parallel.o"])

  if settings.debug then
      println(cc(1) + " '" + (join("' '", cc(2))) + "'")
  end
  var ccp = Process.run(cc(1),cc(2))
  var ccout = 1 //Process.read(ccp)
  var ccerr = 0 //Process.readerr(ccp)
  var ccstat = Process.reap(ccp)
  if () <> ccstat then
      println ("STDOUT:" + join("", ccout))
      println ("STDERR:" + join("", ccerr))
      error ("OOPS! Compiler returned non-zero exit status " + ccstat)
  end

  if settings.debug then
      println(ld(1) + " '" + (join("' '", ld(2))) + "'")
  end
  var ldp = Process.run(ld(1), ld(2))
  var ldout = Process.read(ldp)
  var lderr = Process.readerr(ldp)
  var ldstat = Process.reap(ldp)
  if () <> ldstat then
      println (join("", ldout))
      println (join("", lderr))
      error ("OOPS! Linker returned non-zero exit status " + ldstat)
  end

  stat
end

