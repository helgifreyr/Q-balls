from data import fixed_ws_data
from pylab import plot,savefig,xlabel,ylabel,title,legend
from subprocess import check_output
from os import chdir, listdir

def plot_1st_2nd(dir):
    ws1, Ms1, Js1, cts1, rhs1 = fixed_ws_data(dir+'/1st')
    ws2, Ms2, Js2, cts2, rhs2 = fixed_ws_data(dir+'/2nd')
    w = check_output(['pwd']).split('/')[8][2:6]
    plot(rhs1+rhs2,Ms1+Ms2,label=w)

xlabel(r'$r_h$')
ylabel(r'$M$')
for dir in listdir('.'):
    if 'w' in dir:
        plot_1st_2nd(dir)

legend()
savefig('rh-vs-M-all.png')
