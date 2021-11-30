# Eric E. Palmer - 23 Nov 2021
# Parses through residuals to extract out data needed for quiver plot
#    limit is the highest resolution to include
# Plot using quiv.gpi
# If running on some other machine, fix the path 

image=$1
ex=/Users/epalmer/Documents/GitHub/archivingTools/


if [ "$image" == "" ]
then
	echo "Usage: $0 <imagename>"
	exit
fi

if [ ! -e "RESIDUALS.TXT" ]
then
	echo "Error:  RESIDUALS.TXT not found"
	exit
fi

limit=.01

awk -f $ex/parseResiduals.awk -v limit=$limit -v file=$image RESIDUALS.TXT > quiv.txt



