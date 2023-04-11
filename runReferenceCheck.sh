# John R. Weirich - 8 Dec 2022
# Loop referencCheck.sh (sic) over all images in a file
# Usage: sh runReferenceCheck.sh plist

plist=$1

if [ ! -e $plist ]
then
	echo "File not found"
	exit
fi

list=`cat $plist`

for item in $list
do
	sh /usr/local/spc/bin/referencCheck.sh $item > $item.chk
done 
