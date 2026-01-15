#!/bin/bash
# 07 Jan 2026 - John R. Weirich
# Wrapper to generate map_coverage_p_low products for multiple sets of 
# image lists. Had AI help get it working so you'll see some fancy programming.

# Check if at least one argument is provided
if [ $# -lt 2 ]; then
    echo "Usage: $0 <bigmap> <file1> [file2 ...]"
    exit 1
fi

bigmap=$1 	# First arg: the bigmap
shift		# Shift so "$@" now contains only filenames


echo "10" > tinJRW
echo $bigmap >> tinJRW
echo "n" >> tinJRW


# Loop through all command-line arguments
for file in "$@"; do

    echo $file
    basename="${file##*/}"  # Trim everything left of last "/"
    echo "$basename"
   
    cp -f $file coverage_p.in

/usr/local/spc/bin/map_coverage_p_low < tinJRW

mv ${bigmap}-res.txt ${bigmap}-${basename}-res.txt
    
done
