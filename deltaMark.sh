#!/bin/bash
# Eric E. Palmer - 12 Dec 2022
# deltaMark.sh -- compares what was saved with setMark.sh
#		with the current direction and provides key 
#		(and signficant) statistics

who=`whoami | cut -c 1-2`

dir="../$who""Mark/"

# Use single maplet if on command line.
# If nothing provided, then do the entire directory
lmkList=$1
if [ "$lmkList" == "" ]
then
	lmkList=`ls $dir/LMKFILES | cut -c 1-6`
else
	doOver=1
fi

total=`echo $lmkList | wc -w`
cnt=0

echo "item     total_delta        Components (xyz)   oldR   newR"
for item in $lmkList
do

	if [ ! -e $dir/LMKFILES/$item.LMK ]
	then
		continue;
	fi

	if [ ! -e LMKFILES/$item.LMK ]
	then
		continue;
	fi

	new=`grep VLM LMKFILES/$item.LMK | tr D E`
	old=`grep VLM $dir/LMKFILES/$item.LMK | tr D E`

	#echo $old
	#echo $new

	# Compute delta vector
	ox=`echo $old | awk '{print $1*1000}'`
	nx=`echo $new | awk '{print $1*1000}'`
	#echo "X:  " $ox $nx
	dx=`echo $ox $nx | awk  '{ print ($1 - $2)  }' `

	oy=`echo $old | awk '{print $2*1000}'`
	ny=`echo $new | awk '{print $2*1000}'`
	#echo "Y:  " $oy $ny
	dy=`echo $oy $ny | awk  '{ print ($1 - $2)  }' `

	oz=`echo $old | awk '{print $3*1000}'`
	nz=`echo $new | awk '{print $3*1000}'`
	#echo "Z:  " $oz $nz
	dz=`echo $oz $nz | awk  '{ print ($1 - $2)  }' `


	mag=`echo sqrt \( $dx \* $dx + $dy \* $dy + $dz \* $dz \) | bc | cut -c -5`
	oMag=`echo sqrt \( $ox \* $ox + $oy \* $oy + $oz \* $oz \) | bc | cut -c -5`
	nMag=`echo sqrt \( $nx \* $nx + $ny \* $ny + $nz \* $nz \) | bc | cut -c -5`

	ans=`echo $mag | awk  '{ if ($0 > .001) print $0 }' `

	if [ "$ans" != "" ]
	then
a=1
		echo "$item Delta:  $mag m   ( $dx $dy $dz m)  $oMag  $nMag"
	fi

	if [ "$doOver" == "1" ]
	then
		awk -f /opt/local/spc/bin/getOverlaps.awk LMKFILES/$item.LMK > tmpRun/new
		awk -f /opt/local/spc/bin/getOverlaps.awk $dir/LMKFILES/$item.LMK > tmpRun/old
		ans=`diff -b tmpRun/old tmpRun/new`
		if [ "$ans" != "" ]
		then
			echo "Overlaps: $item     Old                                        New"
			diff -y --suppress-common-lines tmpRun/old tmpRun/new
		fi
	fi
	cnt=`echo $cnt + 1 | bc`

done

echo "Total: $total.  Processed $cnt"
