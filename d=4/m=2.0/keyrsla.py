import sys,os
from subprocess import call,check_output
from scipy import arange

first = float(sys.argv[1])
last = float(sys.argv[2])
jump = float(sys.argv[3])

pwd = check_output(['pwd']).split('/')
m = pwd[8][2:]

folders = arange(first,last,jump)
print folders
if len(sys.argv)>4:
    folders = reversed(folders)
for w in folders:
    print w
    w = float(w)
    w_style = '%4.3f'
    folder = 'w='+w_style%w
    if os.path.isdir(folder) == False:
        os.mkdir(folder)
    os.chdir(folder)
    os.system('cp ../fid-c .')
    os.system('cp ../grid* .')
    if len(sys.argv)==4:
        old_folder = '../w='+w_style%(w-jump)+'/'
    else:
        old_folder = '../w='+w_style%(w+jump)+'/'
    os.system('cp '+old_folder+'funct.dat .')
    functf = open('functf.dat','w')
    for line in open('funct.dat'):
        if len(line)>1:
            functf.write(line.split()[2]+'\n')
    functf.close()
    mw = open('mw','w')
    mw.write(m+'\n')
    mw.write(w_style%w)
    mw.close()
    output = open('output.txt','w')
    mw = open('mw')
    call(['./fid-c'],stdin=mw,stdout=output)
    output.close()
    for line in open('output.txt'):
        print line.replace('\n','')
    os.chdir('..')
