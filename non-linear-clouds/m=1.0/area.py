import os,sys
from scipy import pi

def get_data(dir):
    datas = []
    os.chdir(dir)
    for branch in ['1st','2nd']:
        os.chdir(branch)
        rh_dirs = sorted(os.listdir('.'))
        if branch == '2nd':
            rh_dirs.reverse()
        for rh_dir in rh_dirs:
            if 'rh=' in rh_dir:
                data = ''.join(open(rh_dir+'/tmp.txt').readlines()).replace('\n','').replace('{','').replace('}','').replace(' ','').replace('*^','e').split(',')
                datas.append(data)
        os.chdir('..')
    os.chdir('..')
    return datas

data = open('area.dat','w')
data.write('#w       rh        ct        area     M\n')
data.write('#----------------------------------------\n')

for w_dir in sorted(os.listdir('.')):
    if 'w=' in w_dir:
        print w_dir
        datas = get_data(w_dir)
        for set in datas:
            c1 = set[0]
            c2 = set[1]
            c3 = set[2]
            m = set[3]
            w = '%5.4f'%(round(-float(set[4]),4))
            rh = '%5.4f'%(round(float(set[5]),4))
            ct = '%5.4e'%float(set[6])
            M = '%6.3f'%(round(float(set[7]),3))
            if float(M)>1000:
                print w_dir, set
            J = '%6.3f'%(round(float(set[8]),3))
            maxZ = '%6.5f'%(float(set[9][:-1]))
            area = '%6.5f'%round(4*pi*(float(rh)-float(ct))*(float(rh)-2*float(ct)),4)
            data.write(w+' '+rh+' '+ct+' '+area+' '+M+'\n')
