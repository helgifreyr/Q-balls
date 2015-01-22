from pylab import *
import sys
from os import listdir, chdir
from scipy import linspace,genfromtxt

dirs = []
for dir in listdir('.'):
    if 'w=' in dir:
        dirs.append(dir)

if len(sys.argv)>1:
    limit = int(sys.argv[1])+1
else:
    limit = 100

quartet = []
for dir in sort(dirs):
    w = float(dir[2:])
    for dir1 in sort(listdir(dir+'/1st/')):
        if len(str(w))<=limit:
            if 'rh=' in dir1:
                file = open(dir+'/1st/'+dir1+'/rhct.txt')
                rh = float(file.readline().replace('\n',''))
                ct = float(file.readline().replace('\n',''))
                M = 0.5 * (rh - 2*ct)
                quartet.append([w,rh,ct,M])
    for dir2 in sorted(listdir(dir+'/2nd/'),reverse=True):
        if len(str(w))<=limit:
            if 'rh=' in dir2:
                file = open(dir+'/2nd/'+dir2+'/rhct.txt')
                rh = float(file.readline().replace('\n',''))
                ct = float(file.readline().replace('\n',''))
                M = 0.5 * (rh - 2*ct)
                quartet.append([w,rh,ct,M])

mu = sqrt(1.1)

# for w,rh,ct,M in quartet:
#     print w,rh,ct,M
ws = [row[0]/mu for row in quartet][::2] 
Ms = [row[3]*mu for row in quartet][::2] 

# actual_ws = []
# for i in range(1,len(ws)):
#     if ws[i] != ws[i-1]:
#         actual_ws.append(ws[i-1])
# actual_ws.append(ws[-1])

ws2 = linspace(min(ws),max(ws),100)
# there's no *mu here because I am plotting M*mu as a function of w/mu, not w
Ms2 = lambda w: 1/(2*w)
# for w in actual_ws:
#     text(w,Ms2(w),r'$\omega='+'%3.2f'%w+'$',rotation='vertical',va='bottom',ha='center',size=10)

curve = genfromtxt('curve.dat')
curve_w = curve[:,0]
curve_M = curve[:,1]

plot(curve_w,curve_M,'b')
plot(ws,Ms,'g.',markersize=2.5)
plot(ws2,Ms2(ws2),'k')
xlabel(r'$\Omega_H/\mu$')
ylabel(r'$M_{BH}\mu$')
xlim(min(ws)-0.01,max(ws)+0.01)
title(r'$m=1.0$')
savefig('w-vs-M.png')
savefig('w-vs-M.eps')
