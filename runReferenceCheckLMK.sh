# John R. Weirich - 8 Dec 2022
# Loop ReferenceCheckLMK.sh over all landmarks in a file
# Usage: sh runReferenceCheckLMK.sh lmklist

lmklist=$1

if [ ! -e $lmklist ]
then
	echo "File not found"
	exit
fi

list=`cat $lmklist`

for item in $list
do
	sh /Users/JW/PDART20/JRWRhea/lsupport/ReferenceCheckLMK.sh $item > $item.chk
done 
