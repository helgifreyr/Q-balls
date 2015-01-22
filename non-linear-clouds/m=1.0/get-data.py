import os,sys

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
                # data.append(open(rh_dir+'/sup.dat').readline().split()[1])
                datas.append(data)
        os.chdir('..')
    os.chdir('..')
    return datas

all_data = open('total-data.dat','w')
all_data.write('#c1  c2   c3   m     w      rh         ct         M       J       maxZ    posZ\n')
all_data.write('#-------------------------------------------------------------------------------\n')

for w_dir in sorted(os.listdir('.')):
    if 'w=' in w_dir:
        print w_dir
        datas = get_data(w_dir)
        output = open(w_dir+'/'+w_dir+'.dat','w')
        output.write('#c1  c2   c3   m     w      rh         ct         M       J       maxZ    posZ\n')
        output.write('#-------------------------------------------------------------------------------\n')
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
            posZ = '%6.4f'%(float(set[11]))
            output.write(c1+'  '+c2+'  '+c3+'  '+m+'  '+w+'  '+rh+'  '+ct+' '+M+'  '+J+'  '+maxZ+' '+posZ+'\n')
            all_data.write(c1+'  '+c2+'  '+c3+'  '+m+'  '+w+'  '+rh+'  '+ct+'  '+M+'  '+J+'  '+maxZ+' '+posZ+'\n')
        output.close()
