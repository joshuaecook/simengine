%We generate 500 Monte Carlo option price estimates and reseach their statistical properties.
%These prices are computed with 10,000 sample paths. We assume the maturity
%is 30-days and strike is 111.11. This is exactly what the paper "Empirical Martingale Simulation for Asset Prices"
%did in page 1228 Table 5.
%The mean we get is 0.0071; std is 0.0019. They are very close to the
%author's results: 0.0070 and 0.0020.

sample = zeros(1,500);
for i = 1:500
    s=GARCH_simulation(30,10000,111.11,0.1);
    sample(i)=s(end);
end
mean(sample)
std(sample)

    