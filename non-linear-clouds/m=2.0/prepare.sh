mkdir -p w=$1/1st/rh=0.001
mkdir -p w=$1/1st/rh=$2
mkdir -p w=$1/2nd/rh=$2
ln -s ../../keyrsla.py w=$1/1st/keyrsla.py
ln -s ../../ftoff.py w=$1/1st/ftoff.py
ln -s ../../keyrsla.py w=$1/2nd/keyrsla.py
ln -s ../../ftoff.py w=$1/2nd/ftoff.py
cp ../../d=4/m=2.0/w=$1/funct.dat w=$1/1st/rh=0.001/
cp -r script1 w=$1/1st/script
cp -r script2 w=$1/2nd/script
cd w=$1/1st/rh=0.001
python ../ftoff.py
cp ../script/* .
./run.sh 0.001 $1
mkdir ../rh=0.005 && cd ../rh=0.005
cp ../rh=0.001/funct.dat .
python ../ftoff.py
cp ../script/* .
./run.sh 0.005 $1
cd ..
python keyrsla.py 0.010 $2 0.005
cd rh=$2
# first two are for $2=X.XXXX and the second two are for $2=X.XX5
var1=$( echo "$2*1000.0/5.0*5" | bc)
var2=0$(echo "scale=3;$var1/1000"| bc -l)
# var2=$(echo "scale=3;$var1/1000"| bc -l)
# var2=0$(echo "scale=3;$2-0.005"| bc -l)
cp ../rh=$var2/funct.dat .
python ../ftoff.py
cp ../script/* .
./run.sh $2 $1
# if there are two branches, uncomment appropriate section here
cp funct.dat ../../2nd/rh=$2
cd ../../2nd/rh=$2
python ../ftoff.py
cp ../script/* .
./run.sh $2 $1
cd ..
# this is for when there are two branches
mkdir rh=$var2
cd rh=$var2
cp ../rh=$2/funct.dat .
python ../ftoff.py
cp ../script/* .
./run.sh $var2 $1
cd ..
python keyrsla.py $3 $var2 0.005
# and here's for when there are two but the second is very small
# python keyrsla.py $3 $2 0.0001
