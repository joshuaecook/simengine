%Simulates log asset returns for process following Garch Model
%as described in Jin-Chuan Duan's paper
%THE GARCH OPTION PRICING MODEL
function [S] = GARCH(S0,nend, r)      
%S0: initial price;  
%nend: number of steps;  
%r: constant one-period risk free rate
%------  Enter parameters --------------
lambda = 0.01;     %constant unit risk premium
alpha0 = 0.00001;  %constant intercept of model
alpha1 = 0.2;      % coefficient of GARCH term
beta1 = 0.7;       % coefficient of ARCH term
%--------------------------------------

%validate condition for stationarity of process
test=beta1+alpha1*(1+lambda^2);
if test>1
	fprintf('process will not be stationary\n');
	return;
end

Z=randn(1,nend);     %normally distributed random numbers
h=zeros(1,nend);     %conditional variance       
S=zeros(1,nend);     %underlying asset price

%Equations (2.3) as mentioned on page 17 of source paper
h(1) = alpha0/(1-alpha1-beta1);                                           %assume long run variance as starting point
S(1) = S0*exp(r -0.5*h(1) + sqrt(h(1)) * Z(1));
for i =2:nend
        h(i) = alpha0+alpha1*(Z(i-1)-lambda)^2*h(i-1) + beta1*h(i-1);     %GARCH process for volatilities
        S(i) = S(i-1)*exp(r -0.5*h(i) + sqrt(h(i))*Z(i));                 %lognormal process for stock prices
end


