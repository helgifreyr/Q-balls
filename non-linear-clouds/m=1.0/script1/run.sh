echo $1 > rhw.txt
echo $2 >> rhw.txt
math -script get-parameters-Kerr.m
./fid-c5 | tee output.txt
math -script extract-data.m
