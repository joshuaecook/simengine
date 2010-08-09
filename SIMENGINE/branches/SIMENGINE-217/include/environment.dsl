namespace Environment
  function getVar (name) = LF getEnv name
  function hasVar (name) = isdefined (getVar name)
  function setVar (name, value) = LF setEnv (name, value)
end