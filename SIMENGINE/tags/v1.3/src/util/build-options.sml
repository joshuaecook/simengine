(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

signature BUILD_OPTIONS = sig
val allowSWBackend: bool
val allowFPBackend: bool
val allowHWBackend: bool

val build: string
val buildDate: int
val buildTime: Time.time
val buildNumber: int

val version: string
val majorVersion: int
val minorVersion: int
val versionRevision : string
val architecture: string

val devVersion: bool

end

structure BuildOptions:> BUILD_OPTIONS = struct

fun invalid name = raise Fail ("Build options contains invalid data for " ^ name)

local val optionsFile = OS.Path.mkAbsolute {path=OS.Path.fromUnixPath "data/build-options.json",
					    relativeTo=getSIMENGINE()}
in
val options = ParseJSON.parseFile optionsFile
val _ = if JSON.isObject options then ()
	else invalid "all"
end

val allowSWBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowSWBackend", {default=JSON.bool true}))

val allowFPBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowFPBackend", {default=JSON.bool false}))

val allowHWBackend =
    JSON.boolVal (JSON.memberDefault (options, "allowHWBackend", {default=JSON.bool false}))

val build =
    case JSON.memberValue (options, "build", JSON.toString)
     of SOME s => s | _ => invalid "build"

val buildDate =
    case JSON.memberValue (options, "buildDate", JSON.toString)
     of SOME s => (case Real.fromString s of
		       SOME r => Real.floor (r/(3600.0*24.0))
		     | _ => invalid "buildDate")
      | _ => invalid "buildDate"

val buildNumber :int =
    case JSON.memberValue (options, "buildRevision", JSON.toInt)
     of SOME z => Int.fromLarge z
      | _ => invalid "buildNumber"

val buildTime =
    case JSON.memberValue (options, "buildTime", JSON.toInt)
     of SOME z => Time.fromSeconds (IntInf.toLarge z) | _ => invalid "buildTime"

val version =
    case JSON.memberValue (options, "version", JSON.toString)
     of SOME s => s | _ => invalid "version"

val majorVersion =
    case JSON.memberValue (options, "majorVersion", JSON.toInt)
     of SOME i => IntInf.toInt i | _ => invalid "majorVersion"

val minorVersion =
    case JSON.memberValue (options, "minorVersion", JSON.toInt)
     of SOME i => IntInf.toInt i | _ => invalid "minorVersion"

val versionRevision =
    case JSON.memberValue (options, "versionRevision", JSON.toString)
     of SOME s => s | _ => invalid "versionRevision"

val architecture =
    case JSON.memberValue (options, "architecture", JSON.toString)
     of SOME s => s | _ => invalid "architecture"

val devVersion =
    JSON.boolVal (JSON.memberDefault (options, "devVersion", {default=JSON.bool false}))

end
