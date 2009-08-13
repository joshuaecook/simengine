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

    hidden var dimensions = []

    var downsampling

    function setPrecision (p: Precision)
      precision = p
    end

    function getPrecision() = self.precision

    function setInitialValue (v)
/*      function checkDimensions (v: Number, dim) = dim.length() == 0
      overload function checkDimensions (v: Vector, dim) = dim(1) == v.length() and forall elem in v suchthat checkDimensions (elem, dim.rest())

      if not (checkDimensions (v, dimensions)) then
        error ("Vector/Matrix dimensions of initial value for " + name + " do not match value of those given for this quantity")
      end*/
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
//      println ("get equation for " + name)
      self.eq
    end

    function hasEquation() = isdefined(getEquation())

    function setEquation(eq)
      //TODO: perform error checking here, ie if its a param, DONT allow this
//      println ("setting equation for "+ name + " to " + (eq.getExp().tostring()))      
      self.eq = eq
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


    function tostring ()
      var str = self.class.name + "("
      str = str + "name=" + self.getName()
      str + ")"
    end

    constructor (name: String)
      self.name = name
      self.eq = Equation.new(self', 0)
//      self.eq = DifferentialEquation.new(1, self, 0)
    end
  end

  class MathFunction
    hidden var name

    constructor (name: String, thunk)
      self.name = name

      self.addMethod ("()", thunk)
    end    
  end  


  class Equation
    var lhs
    var rhs

    constructor (lhs, rhs)
      self.lhs = lhs
      self.rhs = rhs
    end
  end  



  class ModelOperation
    var name
    var numArgs
    var execFun
    var precisionMap
    var args
   
    function tostring ()
//      "ModelOperation(name=" + self.name + ", args=" + self.args + ")"
      LF exp2str (self)
    end

    constructor (name: String, numArgs: Number, execFun, precisionMap, args: Vector of _)
      self.name = name
      self.numArgs = numArgs
      self.execFun = execFun
      self.precisionMap = precisionMap
      self.args = args
    end

  end

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

  class RelativeOffset
    var simIterator
    var step
    
    constructor (simIterator: GenericIterator, step: Number)
      self.simIterator = simIterator
      self.step = step
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

  end

  class TimeIterator extends GenericIterator
    var isContinuous
    constructor (name: String, isContinuous)
      super(name)
      self.isContinuous = isContinuous
    end 

  end

  class Output
    var name
    var contents
    var condition
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

    hidden var solver_obj

    property solver
      get
        if (not (isdefined solver_obj)) then
          warning "No solver specified, using default ode45 solver"
          ode45
        else
          solver_obj
        end        
      end

      set(s)
        solver_obj = s
      end
    end

    

    var quantities = []
    var iterators = []
    var submodels = []
    var outputs = {}
    hidden var outputDefs = {} // a placeholder to throw outputs into as we compute them.  outputs will be populated by this after the model stms are run

    hidden function buildOutput(name)
      if exists out in outputDefs.keys suchthat out == name then
        outputs.add(name, outputDefs.getValue(name) ())
      elseif objectContains(self, name) then
        outputs.add(name, self.getMember(name))
      else
        error ("No quantity found matching output in header: " + name + ".  Available outputs are: " + (outputDefs.keys))
      end
    end

    var inputs = []

    //initialization
    var t
    var n

    constructor ()
      t = TimeIterator.new("t", true)
      n = TimeIterator.new("n", false)
    end

    // helper functions

    function getLocalQuantities()
      quantities
    end

    function makeEquation (name, dimensions, lhs, rhs)
      if objectContains(self, name) then
        var q = self.getMember name
        if istype(type SimQuantity, q) then
      	  q.setEquation (Equation.new(lhs q, rhs q))
        else
          error "Cannot create equation for non-quantity: " + name
        end
      else
	var q = Intermediate.new(name)
        self.addConst(name, q)
	if not (istype (type Vector of SimIterator, dimensions)) then
	   error "Indexes on an intermediate equation must be iterators"
        else
          q.dimensions = [d.name foreach d in dimensions]
        end
	
	self.quantities.push_back (q)
        self.getMember(name).setEquation(Equation.new(lhs q, rhs q))        
      end
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

function compile (mod)//: Model)  
  LF compile (Translate.model2forest (mod.instantiate()))
end
