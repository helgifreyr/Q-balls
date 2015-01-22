rm frequency
# echo $2 > frequency
echo $1"d0" >> frequency
./fid-c < frequency | tee output.txt 
dir="w=$1"
mkdir $dir
cp *.dat $dir
cp *.txt $dir
