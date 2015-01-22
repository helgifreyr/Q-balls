var1=$( echo "$1*1000.0/10.0*10" | bc)
echo $var1
var2=$(echo "scale=3;$var1/1000"| bc -l)

echo $var2
