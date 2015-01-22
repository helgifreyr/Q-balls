from data import fixed_ws_data
from pylab import plot,savefig,xlabel,ylabel,title,legend
from subprocess import check_output

ws1, Ms1, Js1, cts1, rhs1 = fixed_ws_data('1st')
ws2, Ms2, Js2, cts2, rhs2 = fixed_ws_data('2nd')

xlabel(r'$r_h$')
ylabel(r'$M$')

w = check_output(['pwd']).split('/')[8][2:6]

title(r'$\omega='+w+'$')

plot(rhs1,Ms1,label='1st branch')
plot(rhs2,Ms2,label='2nd branch')
legend()
savefig('rh-vs-M.png')
