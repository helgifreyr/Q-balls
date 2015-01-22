mkdir -p w=$1/1st/rh=0.001
mkdir -p w=$1/2nd/
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
