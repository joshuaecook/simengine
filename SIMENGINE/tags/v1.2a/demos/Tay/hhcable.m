function dy = hhcable(t,y, Im, N, d, Ie)
%this function calculates the derivative given the current states in y
%the 3rd argument is the current injected into the cell

% Just setting some coefficients
cm = 1;
gk  = 25;
gna = 120;
gl = 0.3;
ek  = -104.58;
ena = 78.56;
el  = -49.0;
dx = 0.001; %step length along cable in cm (10 um); 
dx2 = dx^2;
a = 0.0004; %radius in cm (this is 1 um)
R = 10; %Ohms/cm
r = a/2*R;%20e-8; %a/2*R;

sigma=0.00179; %This is the resistance of the medium S/cm

x = ((0:N-1)-N/2)*dx;
Vout = Ie./(4*pi*sigma*sqrt(d^2+x.^2));

% Renaming the input vector so that the equations are readable
Vin = y(1:4:end);
Vm = Vin-Vout';
n  = y(2:4:end);
m  = y(3:4:end);
h  = y(4:4:end);

% Now calculate the Alpha's and Beta's and infinte time solutions and time
% constants
am=-0.1*(Vm+35)./(exp(-.1*(Vm+35))-1);
bm=4*exp((-Vm-60)/(18));
ah=0.07*exp(-.05*(Vm+60));
bh=1./(1+exp(-.1*(Vm+30)));
an=-0.01*(Vm+50)./(exp(-.1*(Vm+50))-1);
bn=0.125*exp(-0.0125*(Vm+60));

ninf  = an./(an+bn);
tninf = 1./(an+bn);

minf  = am./(am+bm);
tminf = 1./(am+bm);

hinf  = ah./(ah+bh);
thinf = 1./(ah+bh);

dndt = (ninf-n)./tninf;
dmdt = (minf-m)./tminf;
dhdt = (hinf-h)./thinf;

% Now calculate all the derivatives

dvdt = -(1/cm).*(gk*n.^4.*(Vm-ek)+gna.*m.^3.*h.*(Vm-ena)+gl.*(Vm-el));

if N > 1
    dvdt(1) = dvdt(1)+(1/cm).*(r.*((Vin(2)-Vin(1))/dx2) + Im);
    dvdt(2:N-1) =  dvdt(2:N-1)+(1/cm).*(r.*((Vin(1:N-2)+Vin(3:N)-2*Vin(2:N-1))/dx2));
    dvdt(N) = dvdt(N)+(1/cm).*(r.*((Vin(N-1)-Vin(N))/dx2));
else
    dvdt = dvdt + Im;
end
%dvdt(N/2) = dvdt(N/2)+Im;


% Merge all the derivatives into one vector to return.
dy = reshape([dvdt, dndt, dmdt, dhdt]',N*4,1);