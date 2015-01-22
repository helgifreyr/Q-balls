from scipy import genfromtxt
from pylab import *

data1 = genfromtxt('m=1.0-total.dat')
data2 = genfromtxt('m=2.0-total.dat')
data3 = genfromtxt('m=3.0-total.dat')

mu = sqrt(1.1)

w1 = data1[:,4]
W1 = w1/mu
rh1 = data1[:,5]
ct1 = data1[:,6]
w2 = data2[:,4]
W2 = w2/2.0/mu
rh2 = data2[:,5]
ct2 = data2[:,6]
w3 = data3[:,4]
W3 = w3/3.0/mu
rh3 = data3[:,5]
ct3 = data3[:,6]

M1 = []
for i,j in zip(rh1,ct1):
    M1.append(mu*0.5*(i-2*j))
M2 = []
for i,j in zip(rh2,ct2):
    M2.append(mu*0.5*(i-2*j))
M3 = []
for i,j in zip(rh3,ct3):
    M3.append(mu*0.5*(i-2*j))

W4 = linspace(min([min(W2),min(W3)]),max([max(W1),max(W2)]),100)
M4 = lambda w: 1/(2*w)


xlabel(r'$\Omega/\mu$')
ylabel(r'$M\mu$')
plot(W1,M1,'b.',label=r'$m=1.0$',markersize=2.5)
plot(W2,M2,'g.',label=r'$m=2.0$',markersize=2.5)
plot(W3,M3,'y.',label=r'$m=3.0$',markersize=2.5)
plot(W4,M4(W4),'k')
legend()
curve1 = genfromtxt('m=1.0/curve.dat')
curve1_w = curve1[:,0]
curve1_M = curve1[:,1]
plot(curve1_w,curve1_M,'r')
curve2 = genfromtxt('m=2.0/curve.dat')
curve2_w = curve2[:,0]
curve2_M = curve2[:,1]
plot(curve2_w,curve2_M,'r')
curve3 = genfromtxt('m=3.0/curve.dat')
curve3_w = curve3[:,0]
curve3_M = curve3[:,1]
plot(curve3_w,curve3_M,'r')
savefig('m=1,2.png')
