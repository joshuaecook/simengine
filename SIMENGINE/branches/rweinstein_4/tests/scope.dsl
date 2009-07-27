quantity q := 4;

task t()
  5
end;

class test
  task t()
    q
  end

  quantity q:= 3
end;

quantity obj := test.new();

LF print (obj.q);

LF print (obj.t());
