model Test
  state x

  model Internal
    state y
    state z
  end

  submodel a = Internal.new()
  submodel b = Internal.new()
end
