#!/bin/sh
# 12 Dec 2022 - Eric E. Palmer
# Saves the LMK and SUM files at a specific time to do an
#		evaluation about how they changed
# Use deltaMark.sh to see the difference

arg=$1


who=`whoami | cut -c 1-2`

p=`pwd`


dir="../$who""Mark/"
mkdir -p $dir

d=`date`

echo "Mark --- $d --- $p" >> $dir/readme.txt
echo "Mark --- $d --- $p" > $dir/recent.txt

rsync -hapvP SUMFILES $dir/ | tee -a $dir/recent.txt
rsync -hapvP LMKFILES  $dir/ | tee -a $dir/recent.txt

