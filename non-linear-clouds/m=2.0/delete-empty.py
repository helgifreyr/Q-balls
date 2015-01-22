from pylab import *
from os import listdir, chdir, system

dirs = []
for dir in listdir('.'):
    if 'w=' in dir:
        dirs.append(dir)


quartet = []
for dir in sort(dirs):
    w = float(dir[2:])
    for dir1 in sort(listdir(dir+'/1st/')):
        if 'rh=' in dir1:
            files1 = listdir(dir+'/1st/'+dir1)
            if len(files1) == 0:
                system('rmdir '+dir+'/1st/'+dir1)
    for dir2 in sorted(listdir(dir+'/2nd/'),reverse=True):
        if 'rh=' in dir2:
            files2 = listdir(dir+'/2nd/'+dir2)
            if len(files2) == 0:
                system('rmdir '+dir+'/2nd/'+dir2)
