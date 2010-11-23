model (c) = float (init)
/* Shows the effect of simple precision errors in floating-point arithmetic.
 * For certain values of a, the output value c will different from the expected 1.0.
 * MATLAB's printing routines may not show the precision errors, 
 * and SIMPLOT may not always show them either,
 * but they are evidenced as seen in STD(OUT.c(:,2)) or 1.0 == OUT.c(:,2).
 */
input init with {default= 0}
state a = init

equation a' = 0.01
equation b = 10 * a - 10
equation c = a - 0.1 * b

output c = (c,a)

solver = forwardeuler {dt=0.01}
end
