namespace Downsampling
  namespace Samplers
    model FrameCounter
      // TODO: can this be more efficient reformulated to use a decrement and a <0 comparison?
      constant initial = 1
      constant increment = 1
      //parameter limit (0 to 1000000 by 1) = 10
      constant limit = 10

      state count (1 to 1000000 by 1) = initial
      equation count[n+1] = {count[n] + increment when count[n] < limit, initial otherwise}
    end

    class Downsampler
      var frequency

      constructor (frequency)
        self.frequency = frequency
      end
    end

    class MinMax extends Downsampler
      hidden var frameCounter

      function initialize (dt: Number, m: Model, s: SimQuantity)
        var name = s.getName()
        var wasDecreasing
        var vis

        var factor = 1 / (frequency * dt)

        if not(isdefined(frameCounter)) then
          frameCounter = FrameCounter.new()
	  frameCounter.limit.setInitialValue(factor.floor())
          m.addSubModel(name + ".frameCounter", frameCounter)
        end

        wasDecreasing = State.new(name + ".wasDecreasing") { initialval = 0, precision = Range.new(0,1,1) }
        wasDecreasing.setEquation(Equation.new(wasDecreasing, s' < 0))
        m.addVar(name + ".wasDecreasing", wasDecreasing)

        // show the output at the beginning of each frame
        vis = frameCounter.count == frameCounter.initial	
        // and show the output at any local minima or maxima
        vis = ((s' < 0 and not(wasDecreasing)) or (not(s' < 0) and wasDecreasing)) or vis
        // but only when the previously stated visibility condition is satisfied
        vis = s.getIsVisible() and vis
        s.setIsVisible(vis)
      end

      constructor (frequency)
        super(frequency)
      end
    end
  end // namespace Samplers

  function minmax (factor) = Samplers.MinMax.new(factor)
end // namespace Downsampling
