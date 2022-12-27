# Eric E. Palmer - 8 Dec 2022
# referenceCheck - checks an image to see if each of its landmarks has that image
# JRW Mod to start with LMK and check images

lmk=$1

if [ "$lmk" == "" ]
then
	echo "Usage:  $0 <lmkname>"
	echo "Do not use a path or sufix (6 char only)"
	exit
fi


file="LMKFILES/$lmk.LMK"
if [ ! -e $file ] 
then
	echo "Can't find file:  $file"
	exit
fi

mkdir -p tmpDir

awk -f /opt/local/spc/bin/getPicts.awk $file > tmpDir/tmp1

list=`cat tmpDir/tmp1`

for item in $list
do

	awk -f /opt/local/spc/bin/getLmk.awk SUMFILES/$item.SUM > tmpDir/tmp2
	num=`grep $lmk tmpDir/tmp2 | wc -l`

	if [ "$num" != "       1" ]
	then
		echo "$lmk ($num)  $item"
	fi


done
