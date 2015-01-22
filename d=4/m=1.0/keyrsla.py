import sys,os
from subprocess import call
from scipy import arange

first = sys.argv[1]
last = sys.argv[2]
jump = float(sys.argv[3])

folders = arange(float(first),float(last),jump)
print folders

length = len(first)
format = '%'+str(length-1)+'.'+str(length-2)+'f'

for w in folders:
    print w
    w = float(w)
    folder = 'w='+format%w
    old_folder = 'w='+format%(w-jump)+'/funct.dat'
    print old_folder
    os.system('rm funct.dat')
    os.system('cp '+old_folder+' .')
    functf = open('functf.dat','w')
    for line in open('funct.dat'):
        if len(line)>1:
            functf.write(line.split()[2]+'\n')
    functf.close()
    output = open('output.txt','w')
    call(['sh','run.sh',format%w],stdout=output)
    output.close()
    for line in open('output.txt'):
        print line.replace('\n','')
