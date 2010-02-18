var systemDependencies = Dependency.getDependencies()
import "devices.dsl"
import "command_line.dsl"

var compiler_settings = CommandLine.parseCommandLine()

if validateCompilerSettings(compiler_settings) then
  compile2(compiler_settings)
end
