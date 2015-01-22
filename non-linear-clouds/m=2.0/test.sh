var1=$( echo "$1*1000.0/5.0*5" | bc)
var2=0$(echo "scale=3;$var1/1000"| bc -l)
echo $var2
