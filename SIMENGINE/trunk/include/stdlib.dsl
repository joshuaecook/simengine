namespace Text
function print (s) = LF print (s.tostring())
function println (s) = print (s.tostring() + "\n")
function warning (s) = LF warning (s.tostring())
function notice (s) = LF notice (s.tostring())
function failure (s) = LF failure (s.tostring())
end 


namespace Operations

// Booleans

function not (b: Boolean) = {false when b, true otherwise}

// Numbers
constant pi = LF pi_value ()
constant e = LF e_value ()

function operator_add (a: Number, b: Number) = LF add(a,b)
function operator_subtract (a: Number, b: Number) = LF subtract(a,b)
function operator_multiply (a: Number, b: Number) = LF multiply(a,b)
function operator_divide (a: Number, b: Number) = LF divide(a,b)
function operator_modulus (a: Number, b: Number) = LF modulus(a,b)
function operator_neg (a: Number) = LF neg a

function invert (arg) = 1 / arg
		   

function abs (x: Number) = x.abs()

function exp (x: Number) = LF exp (x)

function sqrt (x: Number) = LF sqrt (x)

function ln (x: Number) = LF ln (x)
function log10 (x: Number) = LF log10 (x)
function logn (x: Number, y: Number) = {ln(x) when y == e, 
                                        log10(x) when y == 10, 
                                        ln(x) / ln(y) otherwise}


function power (x: Number, y: Number) = exp(y * ln(x))

function sum (x: Vector of Number) = foldl (lambdafun(a,b) = a + b) 0 x
function prod (x: Vector of Number) = foldl (lambdafun(a,b) = a * b) 1 x

function deg2rad (x) = x / (180 / pi)
function rad2deg (x) = x * (180 / pi)

function sin (x: Number) = LF sin (x)
function cos (x: Number) = LF cos (x)
function tan (x: Number) = LF tan (x)

function sinh (x: Number) = LF sinh (x)
function cosh (x: Number) = LF cosh (x)
function tanh (x: Number) = LF tanh (x)

function asin (x: Number) = LF asin (x)
function acos (x: Number) = LF acos (x)
function atan (x: Number) = LF atan (x)

function atan2 (x: Number, y: Number) = LF atan2 (x, y)

function asinh (x: Number) = ln(x + power((x*x) + 1, 0.5))
function acosh (x: Number) = ln(x + power((x*x) - 1, 0.5))
function atanh (x: Number) = 0.5 * ln((1+x) / (1-x))

function csc (x: Number) = 1.0 / (sin x)
function sec (x: Number) = 1.0 / (cos x)
function cot (x: Number) = 1.0 / (tan x)

function csch (x: Number) = 1.0 / (sinh x)
function sech (x: Number) = 1.0 / (cosh x)
function coth (x: Number) = 1.0 / (tanh x)

function acsc (x: Number) = asin (1.0 / x)
function asec (x: Number) = acos (1.0 / x)
function acot (x: Number) = atan (1.0 / x)

function acsch (x: Number) = asinh (1.0 / x)
function asech (x: Number) = acosh (1.0 / x)
function acoth (x: Number) = atanh (1.0 / x)

//constant rand = LF rand ()


// Strings
overload function operator_add (a: String, b) = LF strconcat (a, b.tostring())
overload function operator_add (a, b: String) = LF strconcat (a.tostring(), b)
overload function operator_add (a: String, b: String) = LF strconcat (a,b)

overload function operator_multiply (s: String, n: Number) = {"" when n <= 0, s when n == 1, s + (s * (n-1)) otherwise}


// Vectors
class Wildcard
end

function operator_exists (predicate, vector)
  if vector.isempty() then
    false
  else
    predicate (vector[1]) or operator_exists(predicate, vector.rest())
  end
end

function operator_forall (predicate, vector)
  if vector.isempty() then
    true
  else
    predicate (vector[1]) and operator_forall(predicate, vector.rest())
  end
end


  function foldl (f)
    function init (aggregate)
      function compute (vector: Vector)
        if vector.isempty() then
          aggregate
        else
          foldl f (f(vector.first(), aggregate)) (vector.rest())
        end
      end
      compute
    end
    init
  end


function vector_index (athing, args: Vector)
  if args.isempty() then
    athing
  else
    error "Subscripts applied to non-vector.  Perhaps too many dimensions are specified."
  end
end

overload function vector_index (vector: Vector, args: Vector)

  if args.isempty() then
    vector
  else
    function applyArg (slice:Interval)
      (vector.slice (slice.low, slice.high)).map (lambdafun(e) = vector_index (e, args.rest()))
    end

    overload function applyArg (all:Wildcard)
      vector.map (lambdafun(e) = vector_index (e, args.rest()))
    end

    overload function applyArg (subset:Vector of Number)
      error "set selection from a vector is not supported yet"
    end

    overload function applyArg (position:Number)
      vector_index(vector.at(position), args.rest())
    end

    applyArg (args.first())
  end

end


function objectContains (obj, member) = exists m in obj.members.tovector() suchthat m==member //exists((lambdafun(m) = m == member), obj.members)

function isRegexpMatch (pattern: String, str: String) = LF ismatch (pattern, str)
function getRegexpMatches (pattern: String, str: String) = LF getMatches (pattern, str)

function join (sep: String, v: Vector)
  var n = v.length()

  if 0 == n then ""
  elseif 1 == n then v[1].tostring()
  else
    var joined = v[1].tostring()
    foreach el in v.rest() do
      joined = joined + sep + el.tostring()
    end
    joined
  end
end

function center (str: String, width: Number, fill: String)
  var padn = (width - str.length ()) / 2
  var filln = fill.length ()

  if 0 >= padn then
    str
  elseif 0 == filln then
    center (str, width, " ")
  else
    var lpadn = padn.floor ()
    var rpadn = padn.ceil ()
    var lmod = lpadn % filln
    var rmod = rpadn % filln
    var lfilln = (lpadn / filln).floor()
    var rfilln = (rpadn / filln).floor()
  
    var lfill = {(fill * lfilln) + (fill.substring (1, lmod)) when 0 < lmod,
                 fill * lfilln otherwise}
    var rfill = {(fill * rfilln) + (fill.substring (1, rmod)) when 0 < rmod,
                 fill * rfilln otherwise}
  
    lfill + str + rfill
  end
end

// Vectors

function zip(v1: Vector of _, v2: Vector of _)  
  if v1.length() <> v2.length() then
    error "cannot zip vectors of unequal lengths"
  else
    function zipper(v1,v2) = {[] when v1.isempty() or v2.isempty(), zipper(v1.rest(), v2.rest()).push_front([v1.first(), v2.first()]) otherwise}
    zipper(v1,v2)
  end
end


overload function operator_add (a: Vector of _, b: Vector of _) = LF vecconcat (a, b)


function app (v, f)  
  function apply (v)
    if v.isempty() then
      ()
    else
      f(v.first())
      apply (v.rest())
    end
  end

  if LF istype (type Vector, v) then
    apply(v)
  elseif objectContains(v, "tovector") then
    apply(v.tovector())
  else
    error ("Cannot iterate over non-iterable quantity: " + v)
  end
end

multifunction
  flatten (x) = [x]
  flatten (v: Vector of _)
       if v.isempty() then
         []
       else
          (flatten (v.first())) + (flatten (v.rest()))
       end
     end
end

function filter(f, v) = filter(f, v.tovector())
overload function filter(f, v: Vector) =
  {v when v.isempty(),
   filter(f, v.rest()).push_front(v.first()) when f(v.first()),
   filter(f, v.rest()) otherwise}

// Types
function istype (typ, quant) = LF istype (typ, quant)

// Testing
function test_pass(pass) = LF sys_exit ({1 when not pass, 0 otherwise})

end // namespace Operations

namespace Types
  class Enum
    var name
    var enumName
    var index

    constructor (name: String, enumName: String, index: Number)
      self.name = name
      self.enumName = enumName
      self.index = index
    end
 
    function tostring() = name
  end

  class Pattern
    hidden var mask = ""

    function tostring() = "|" + mask + "|"

    hidden function match_pattern(pattern: String, str: String)
      if pattern == "" and str == "" then
        true
      elseif pattern == "*" then
        true
      elseif str == "" then
        false
      elseif pattern == "" then
        false
      elseif pattern[1] == "?" then
        match_pattern (pattern.rest(), str.rest())
      elseif pattern[1] == "*" then
        match_pattern (pattern, str.rest()) or match_pattern (pattern.rest(), str) or match_pattern (pattern.rest(), str.rest())
      elseif pattern[1] == str[1] then
        match_pattern (pattern.rest(), str.rest())
      else
        false
      end
    end    

    function match (str:String) = match_pattern (mask, str)


    constructor (mask: String)
      self.mask = mask
    end
  end
 
  class Table
    var keys = []

    constructor (entries: Vector of (_, _))
      foreach entry in entries do
        add(entry(1), entry(2))
      end
    end

    function add(name)
      addVar(name)
      keys.push_back(name)
      self
    end

    overload function add(name, exp)
      addVar(name, exp)
      keys.push_back(name)
      self
    end

    function getValue(name) 
      if exists k in keys suchthat k == name then 
        getMember(name)
      else
        error ("Invalid key: " + name)
      end
    end

    property contents
      get
        [[key, self.getMember(key)] foreach key in keys]
      end
    end

    property values
      get
        [self.getMember(key) foreach key in keys]
      end
    end

    function tostring() = "a table with keys: " + (", ".join keys)
  end

  class Interval
    var low
    var high
    var step=1

    //TODO: make this work more like the colon operator in matlab (which reduces rounding error in adding step)
    hidden function tabulate (a:Number, b: Number) = LF tabulate (a,b)

    function tovector() = tabulate(low, high)

    function tostring() = "(" + low + ":" + {step + ":" when step <> 1, "" otherwise} + high + ")"

    constructor (low, high)
      self.low = low
      self.high = high
    end

    constructor (low, step, high)
      self.low = low
      self.step = step
      self.high = high
    end    
  end

  function operator_tabulate (a:Number, b: Number) = Interval.new(a,b)
  overload function operator_tabulate (low:Number, step: Number, high: Number) = Interval.new(low, step, high)

  class Set
    hidden var values = []

    function add (v)
      {values.push_front(v) when [x foreach x in values when x == v].isempty(), values otherwise}
      self
    end
    overload function add (v: Vector of _)
      foreach x in v do
        add(x)
      end
      self
    end
    overload function add(v: Interval)
      foreach x in v.tovector() do
        add(x)
      end
      self
    end    
    function tovector() = values.clone()
  end

  function initObjWithTable(obj, t: Table)
    foreach key in t.keys do
      obj.setMember(key, t.getMember(key))
    end
    obj
  end

  class FixptPrecision
    var sign
    var bits
    var frac
 
    constructor (sign, bits, frac)
      self.sign = sign
      self.bits = bits
      self.frac = frac
    end

    function tostring() = sign + ":" + bits + ":" + frac

  end

  class Setting
    var name
    var description
    hidden var h_value
    var valType
    
    function getValue() = value //consider repolling

    function setValue(value/*: valType*/)    //TODO: include this when this works
      self.value = value
      LF setSetting (name, value)
    end

    function tostring() = name + "(" + value + ") - " + description

    property value
      get = h_value
      set(v)
        self.h_value = v
        LF setSetting (name, v)
      end
    end

    constructor (name, value, valType, description)
      self.name = name
      self.h_value = value
      self.valType = valType
      self.description = description
    end
  end
end


namespace Relational
  function operator_gt (a: Number, b: Number) = LF gt(a,b)
  function operator_ge (a: Number, b: Number) = LF ge(a,b)
  function operator_lt (a: Number, b: Number) = LF lt(a,b)
  function operator_le (a: Number, b: Number) = LF le(a,b)
  function operator_eq (a,b) = LF eq(a,b)
  function operator_ne (a, b) = LF neq(a,b)
end

namespace Process
  /* Blocks until a line of text is available on stdout. */
  function readline (process) = LF preadline (process)
  /* Blocks until a line of text is available on stderr. */
  function readerrline (process) = LF preaderrline (process)
  /* Blocks until a line of text is available on stderr or stdout, 
   * returns a pair containing the line of text for each corresponding stream. 
   * The pair may contain () for either element if the corresponding stream 
   * has no available data. Returns ((),()) when both streams are ended.
   */
  function readOutAndErrLine (process) = LF preadOutAndErrLine (process)

  /* Reads the entire stdout and stderr buffers and returns two 
   * vectors of lines of text. */
  function readAll (process)
    var out = []
    var err = []
    var reading = true

    while reading do
      var lines = Process.readOutAndErrLine(process)
      if () <> lines(1) then
        out.push_back(lines(1))
      end
      if () <> lines(2) then
        err.push_back(lines(2))
      end
      reading = () <> lines(1) or () <> lines(2)
    end

    (out, err)
  end
    
  /* Reads the entire stdout buffer and returns a vector of lines of text,
   * discarding any text on stderr. */
  function read (process)
    var all = Process.readAll(process)
    all(1)
  end

  /* Reads the entire stdout buffer and returns a vector of lines of text,
   * discarding any text on stdout. */
  function readerr (process)
    var all = Process.readAll(process)
    all(2)
  end

  function write (process, text) = LF pwrite (process, text)

  function run (name: String, args: Vector) = LF popen (name, args)
  overload function run (name: String) = LF popen (name, [])

  // Always returns () regardless of process exit status,
  // due to a bug in MLton's subprocess libraries.
  function reap (process) = LF preap (process)
end

namespace File

  // text file io

  class TextFileIn
    var instream
    constructor (instream)
      self.instream = instream
    end

    // all functions return UNIT when eof (done) is reached.

    function getline() = LF getline instream
    function getchar() = LF getchar instream

    // returns up to n characters
    function getchars(n: Number) = LF getchars (instream, n)
    function getall() = LF getall instream
    function close() = LF close instream
    function done() = LF endofstream instream
  end

  function openTextIn (filename: String)
    TextFileIn.new(LF openTextIn filename)
  end

  class TextFileOut
    var outstream
    constructor (outstream)
      self.outstream = outstream
    end
    function putstr (str: String) = LF outputstring (outstream, str)
    function flush () = LF flush(outstream)
    function close() = LF close outstream
  end

  function openTextOut (filename: String)
    TextFileOut.new(LF openTextOut filename)
  end

  function openTextAppend (filename: String)
    TextFileOut.new(LF openTextAppend filename)
  end


  // binary file io
  // placeholder

end

namespace FileSystem
  function pwd () = LF pwd ()
  function chmod (file, permission) = LF chmod (file, permission)
  function getPermissions(file) = LF getPermissions (file)
  function setPermissions(file, permission) = chmod (file, permission)
  function rmfile (file) = LF rmfile file
  function rmdir (dir) = LF rmdir dir
  function chdir (dir) = LF chdir dir
  function ls () = LF ls ()
  function isdir (dir) = LF isdir dir
  function isfile (file) = LF isfile file
  function mkdir (dir) = LF mkdir dir
  function realpath (file) = LF realpath file
  function modtime (file) = LF modtime file
end

namespace Path
  function dir (path) = LF path_dir path
  function file (path) = LF path_file path
  function join (dir, file) = LF path_join (dir, file)
  function base (path) = LF path_base path
  function ext (path) = LF path_ext path
end

namespace Time
  function timestampInSeconds() = LF timestampSeconds ()
  function timestampString() = LF timestampString ()
end

//namespace Profile
  // the time function invokes fcn applied with the passed args and returns the normal result. When the --profile flag
  // is set, then the system will return the time spent in that function
  function profileTime(string_id, fcn, args) = LF profileTime (string_id, fcn, args)
//end
/*
namespace Licensing
  function loadLicense () 
    //logic to pull in license goes here
    var license = ""

    LF applyLicense license
  end

  function verify ()
    LF verifyLicense ()
  end

  // licensing check performed at startup
  function startupCheck ()
    Licensing.loadLicense()
    Licensing.verify()
  end

end

/*    function isFunctionidVersion (major, minor) = 

    function isExpired () = LF
    function isTrial () = 
    function isBasic () = 
    function isStandard () = 
    function isProfessional () = 
    function isDevelopment () = 

    (* verify that the license is functionid by checking the restriction, returning an error if the restriction is not met *)
    function verifyNotRestricted : unit -> unit
    function verifyFunctionidVersion : (int * int) -> unit
    function verifyExpired : unit -> unit
*/


open Operations
open Relational
open Text

open Types

import "sys.dsl"
import "environment.dsl"
import "archive.dsl"
import "json.dsl"
