#!/usr/bin/ruby

require 'tempfile'

class MLBTempfile < Tempfile
    def make_tmpname(basename, n)
      # force tempfile to use basename's extension if provided
      ext = File::extname(basename)
      # force hyphens instead of periods in name
      sprintf('%s%d-%d%s', File::basename(basename, ext), $$, n, ext)
    end
end # class


tests = `find .. | grep [.]sml$ | xargs realpath`.split '\n'

puts tests.class.name 
for test in tests do

  puts "handling #{test}"
  # create /tmp mlb
  f = MLBTempfile.new('smlunittest.mlb', Dir::tmpdir)

  f.puts("local
  (* import Basis Library *)
  $(SML_LIB)/basis/basis.mlb
  $(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb
  $(SML_LIB)/basis/sml-nj.mlb
  $(SML_LIB)/basis/mlton.mlb
  $(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
  $(SML_LIB)/smlnj-lib/RegExp/regexp-lib.mlb
  #{`pwd`.strip}/../../../src/cool.mlb
in")
  
  f.puts test
  f.puts 'end'

  f.close

  # compile mlb to test
  puts `pwd`
  puts "mlton -output #{f.path}.out #{f.path}"
  `mlton -output #{f.path}.out #{f.path}`

  # run test and gather output
  output = `#{f.path}.out`
  # check output for UNITTESTRESULT data
  puts output
  # output test entry
end
