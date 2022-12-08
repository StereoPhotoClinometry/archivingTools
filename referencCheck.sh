# Eric E. Palmer - 8 Dec 2022
# referenceCheck - checks an image to see if each of its landmarks has that image


pict=$1

if [ "$pict" == "" ]
then
	echo "Usage:  $0 <imagename>"
	echo "Do not use a path or sufix (12 char only)"
	exit
fi


file="SUMFILES/$pict.SUM"
if [ ! -e $file ] 
then
	echo "Can't find file:  $file"
	exit
fi

mkdir -p tmpDir

awk -f /opt/local/spc/bin/getLmk.awk $file > tmpDir/tmp2

list=`cat tmpDir/tmp1`

for item in $list
do

	awk -f /opt/local/spc/bin/getPicts.awk LMKFILES/$item.LMK > tmpDir/tmp2
	ans=`grep $pict tmpDir/tmp2`
	num=`grep $pict tmpDir/tmp2 | wc -l`

	if [ "$num" != "       1" ]
	then
		echo "$pict ($num)  $item"
	fi


done
