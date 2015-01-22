import os
import sys
from subprocess import check_output,call
from scipy import arange

first = sys.argv[1]
last = sys.argv[2]

pwd = check_output(['pwd'])
freq = pwd.split('/')[9][2:]
branch = pwd.split('/')[10][0]

jump = float(sys.argv[3])

folders = arange(float(first),float(last),jump)
if branch == '2':
    folders = folders[::-1]

length = len(first)
format = '%'+str(length-1)+'.'+str(length-2)+'f'
print format

print folders

for rh in folders:
    folder = 'rh='+format%(float(rh))
    print folder
    if os.path.isdir(folder) == False:
        os.mkdir(folder)
    os.chdir(folder)
    os.system('cp ../script/* .')
    if branch == '1':
        old_folder = '../rh='+format%(float(rh)-jump)+'/funct.dat'
    elif branch == '2':
        old_folder = '../rh='+format%(float(rh)+jump)+'/funct.dat'
    os.system('cp '+old_folder+' .')
    functf = open('functf.dat','w')
    for line in open('funct.dat'):
        if len(line)>1:
            functf.write(line.split()[2]+'\n')
    functf.close()
    # os.system('pwd')
    # print './run.sh '+folder[-5:]+' '+freq
    rhw = open('rhw.txt','w')
    rhw.write(folder[-5:]+'\n')
    rhw.write(freq)
    rhw.close()
    call(['math','-script','get-parameters-Kerr.m'])
    output = file('output.txt','w')
    call(['./fid-c5'],stdout=output)
    output.close()
    for line in open('output.txt'):
        print line.replace('\n','')
    call(['math','-script','extract-data.m'])
    os.chdir('..')
