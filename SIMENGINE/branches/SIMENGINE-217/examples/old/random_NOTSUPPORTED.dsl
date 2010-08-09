/* Constructs an entropy source.
 * The first parameter must be a SimQuantity, generally the iterator "n".
 * The second and third parameters specify the upper and lower bounds of the generated values.
 * The fourth parameter specifies the granularity of the generated values.
 */
function rng (iter, low, high, step) =
  ModelOperation.new("rng",4,rng,0,[iter,low,high,step])

/* Constructs a seeded entropy source which always generates the same sequence of values for a given seed.
 * The first four parameters are the same as those given to rng().
 * The fifth parameter specifies the initial seed.
 */
function srng (iter, low, high, step, seed) =
  ModelOperation.new("srng",4,srng,0,[iter,low,high,step,seed])

model RandomWalk
   state price (0 to 100000 by 0.00125) = 8000

   equations
     entropy = rng(n,-0.00125,0.00125,0.00125)
     price' = entropy
   end

   setVisible |price|
   solver = forwardeuler(1)
end