from pylab import *
from scipy import genfromtxt
import sys

data = genfromtxt('area.dat')

area = sys.argv[1]
if '.' in sys.argv[1]:
    prec = len(area.split('.')[1])
else:
    prec = 0

ws = []
Ms = []
for w,rh,ct,ar,M in data:
    if round(ar,prec) == float(area):
        ws.append(w)
        Ms.append(M)

plot(ws,Ms)
xlabel(r'$\omega$')
ylabel(r'$M$')
title(r'Area=$'+area+'$')
show()
