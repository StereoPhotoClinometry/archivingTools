# Eric E. Palmer - 10 May 2024
# spcReplace - carefully moves files when a whole list of things
#			need to be changed out (such as murphy or similar)

source=$1
file=$2


if [ "$source" == "" ]
then
	echo "Usage:  $0 <path> <list>"
	exit
fi


if [ "$file" == "" ]
then
	echo "Usage:  $0 <path> <list>"
	exit
fi


if [ ! -e  $source ]
then
	echo "$new path cannot be found from `pwd`"
	exit
fi


if [ ! -e $file ]
then
	echo "$file does not exist"
	exit
fi

# Confirm
echo
echo "Current: `pwd`"
echo "Path to source: $source"
cnt=`wc -l $file`
echo "$cnt files in $file"
list=`cat $file`

read -p "Continue (y/n)?" CONT
if [ "$CONT" = "y" ]; then
	echo "continuing";
else
	echo "Exiting"
	exit
fi

# Make documentation

mkdir -p delLater
date >> delLater/README
pwd >> delLater/README
echo "From here, we are moving $file from $source" >> delLater/README

# Run the command
for item in $list
do

	work=$source/$item
	if [ -e $work ]
	then
		cp -fv $item delLater/
		cp -v $work .
	else
		echo "Skipping:  $work not found"
	fi


done







