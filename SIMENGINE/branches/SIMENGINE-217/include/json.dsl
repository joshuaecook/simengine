namespace JSON
  function encode (value) = LF jsonEncode value
  function decode (json) = LF jsonDecode json
  function concat (a, b) = LF jsonConcat (a, b)
  function addMember (obj, name, elem) = LF jsonAddMember (obj, name, elem)
end
