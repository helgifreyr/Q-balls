import os
from subprocess import call,check_output

dirs1 = os.listdir('.')

for dir1 in dirs1:
    if 'w=' in dir1:
        os.chdir(dir1)
        os.chdir('1st')
        dirs2 = os.listdir('.')
        for dir2 in dirs2:
            if 'rh=' in dir2:
                os.chdir(dir2)
                os.system('cp ../../../extract-data.m .')
                os.system('rm tmp.txt')
                call(['math','-script','extract-data.m'])
                os.chdir('..')
        os.chdir('../2nd')
        dirs2 = os.listdir('.')
        for dir2 in dirs2:
            if 'rh=' in dir2:
                os.chdir(dir2)
                os.system('cp ../../../extract-data.m .')
                os.system('rm tmp.txt')
                call(['math','-script','extract-data.m'])
                os.chdir('..')
        os.chdir('../../')
