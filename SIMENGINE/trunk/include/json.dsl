namespace JSON
  function encode (value) = LF jsonEncode value
  function decode (json) = LF jsonDecode json
  function concat (a, b) = LF jsonConcat (a, b)
end
