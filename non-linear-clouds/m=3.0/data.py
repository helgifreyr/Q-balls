import os
from pylab import *

def fixed_ws_data(path):
    os.chdir(path)
    folders = sorted(os.listdir('.'))
    ws = []
    Ms = []
    Js = []
    cts = []
    rhs = []
    for folder in folders:
        if 'rh=' in folder:
            os.chdir(folder)
            data = ''
            for line in open('tmp.txt'):
                line = line.replace(r'{','')
                line = line.replace(r'}','')
                line = line.replace('\n','')
                line = line.replace(r' ','')
                line = line.replace(r'*^','e')
                data += line
            data = data.split(',')
            w = float(data[4])
            ws.append(w)
            rh = float(data[5])
            rhs.append(rh)
            ct = float(data[6])
            cts.append(ct)
            M = float(data[7])
            Ms.append(M)
            J = float(data[8])
            Js.append(9)
            # print rh, M, J
            os.chdir('..')
    os.chdir('..')
    return ws, Ms, Js, cts, rhs
