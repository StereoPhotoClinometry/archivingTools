#/bin/sh



min=-5.2
max=10.2667


bigmap=$1
width=1001
exPath="/opt/local/spc/bin"
altwg=""
altwg="/Users/epalmer/Dropbox/SPC-ORex/eval_tools/altwg-2020.08.13-macosx-x64/bin/"

if [ "$bigmap" == "" ]
then
	echo "Usage:  $0 <bigmap name>"
	exit
fi

curr=`pwd`
outPath="$curr/$bigmap/"
outFile="$outPath/index.html"

prePath=$curr/pre1
postPath=$curr/post1
basePata=$curr

mkdir -p $bigmap

##############################################################
# Make header
##############################################################
echo "<H1>$bigmap</H1><BR><BR> " > $outFile

##############################################################
# Create PRE 
##############################################################
cd $prePath
echo $bigmap | flatMapVec
/bin/mv $bigmap.TXT $outPath/${bigmap}PreTopo.TXT

val=`echo $bigmap | dumpMapHeaders | head -2 | tail -1 | cut -c 15-`
width=`echo $val + $val + 1 | bc`
echo ">>$width<<"


echo $bigmap | showmap
convert  $bigmap.pgm $outPath/${bigmap}Pre.jpg

line=`tail -2 SIGMAS.TXT | head -1`
convert -border 0x40 -gravity North -pointsize 25 -annotate +0+0 "$line" SIGMAS.pgm $outPath/sigmaPre.jpg

echo "FlatMapVect"
echo $bigmap | $exPath/flatMapVec

echo "topoCube"
ascii2isis from=$bigmap.TXT to=$outPath/preTopo.cub samples=$width lines=$width

echo "Starting view_map"
echo "$bigmap" > tmp
echo "45 45 45 45" >> tmp
view_map_rgb < tmp
convert view.ppm $outPath/${bigmap}RGB-Pre.jpg

echo "Starting view_map_stereo"
echo $bigmap | view_map_stereo
convert view.pgm $outPath/${bigmap}stereo-Pre.jpg

echo "Making OBJ"
#theep - clean up
#$altwg/Maplet2FITS MAPFILES/$bigmap.MAP tmp.fits >> $outPath/log
#$altwg/FITS2OBJ --local tmp.fits $outPath/${bigmap}Pre.obj >> $outPath/log
#outPre1=`$altwg/PrintShapeModelStatistics $outPath/${bigmap}Post.obj `
outPre2=`echo $bigmap | dumpMapHeaders`


##############################################################
# Create POST 
##############################################################
cd $postPath
echo $bigmap | flatMapVec
/bin/mv $bigmap.TXT $outPath/${bigmap}PostTopo.TXT

echo $bigmap | showmap
convert $bigmap.pgm $outPath/${bigmap}Post.jpg


line=`tail -2 SIGMAS.TXT | head -1`
convert -border 0x40 -gravity North -pointsize 25 -annotate +0+0 "$line" SIGMAS.pgm $outPath/sigmaPost.jpg


echo $bigmap | $exPath/flatMapVec
ascii2isis from=$bigmap.TXT to=$outPath/postTopo.cub samples=$width lines=$width

echo "Starting view_map"
echo "$bigmap" > tmp
echo "45 45 45 45" >> tmp
view_map_rgb < tmp
convert view.ppm $outPath/${bigmap}RGB-Post.jpg

echo "Starting view_map_stereo"
echo $bigmap | view_map_stereo
convert view.pgm $outPath/${bigmap}stereo-Post.jpg

echo "Making OBJ"
#theep - clean up
#$altwg/Maplet2FITS MAPFILES/$bigmap.MAP tmp.fits >> $outPath/log
#$altwg/FITS2OBJ --local tmp.fits $outPath/${bigmap}Post.obj >> $outPath/log
#outPost1=`$altwg/PrintShapeModelStatistics $outPath/${bigmap}Post.obj `
outPost2=`echo $bigmap | dumpMapHeaders`

#-------------------------------------------------------------
##############################################################
# Create and display GLOBAL 
##############################################################
cd $outPath

scale1=`isis2std from=preTopo.cub to=preTopo.png`
scale2=`isis2std from=postTopo.cub to=postTopo.png`

shade from=preTopo.cub to=tmp.cub azimuth=270 zenith=60 PIXELRESOL=1.00
isis2std from=tmp.cub to=tmp.png
convert tmp.png -border 0x32 -gravity North -pointsize 25 -annotate +0+0 "Pre" preShade.png

shade from=postTopo.cub to=tmp.cub azimuth=270 zenith=60 PIXELRESOL=1.00
isis2std from=tmp.cub to=tmp.png
convert tmp.png -border 0x32 -gravity North -pointsize 25 -annotate +0+0 'Post' postShade.png

convert -adjoin  -delay 100 preShade.png postShade.png blink.gif
echo "Shaded Relief<BR><img src=blink.gif width=500> <BR><BR><HR>" >> $outFile

# Calculate the difference
algebra from=postTopo.cub from2=preTopo.cub to=diff.cub 
isis2std from=diff.cub to=diff.png > tmp

min=`grep Minimum tmp | cut -c 19-`
max=`grep Maximum tmp | cut -c 19-`
min=`echo $min / 1 | bc`
max=`echo $max / 1 | bc`
if [ "$min" -lt "0" ]
then
  min=`echo -1 \* $min | bc`
fi
if [ "$max" -lt "0" ]
then
  max=`echo -1 \* $max | bc`
fi
cbRange=`echo $(( $min > $max ? $min : $max ))`
echo cbrange $cbRange

echo "Delta<BR><img src=diff.png width=500> <img src=diffC.png width=700><BR><BR><HR>" >> $outFile
isis2ascii header=no from=diff.cub to=diff.txt
sed -i '' 's/-/ -/g' diff.txt

echo "-----"
echo min $min
echo max $max

echo set yr [*:*] reverse > tmpPlt
echo set pm3d map >> tmpPlt
echo unset key >> tmpPlt
echo set output \'diffC.png\' >> tmpPlt
echo set terminal png size 1024,1024 >> tmpPlt
echo set palette defined \(-50 \"blue\", 0 \"white\", 50 \"red\" \) >> tmpPlt
echo set cbrange [-$cbRange:$cbRange] >> tmpPlt
echo splot \'diff.txt\' matrix >> tmpPlt
gnuplot < tmpPlt




valA=`echo $width "* .25" | bc`
valA1=`echo $valA - 5 | bc`
valA2=`echo $valA + 5 | bc`
echo val=$valA > tmp
echo set ytic 250 >> tmp
echo set yr [*:*] reverse >> tmp
echo set xlabel \'meters\' >> tmp
echo set ylabel \'pixels\' >> tmp
echo set output \'transA.png\' >> tmp
echo set term png size 512,1024  >> tmp
echo set style data lines >> tmp
echo set title \'Transit at $valA\' >> tmp
echo plot \'${bigmap}PreTopo.TXT\' u val:0 lc -1 lw 2 title \'Pre\', \'${bigmap}PostTopo.TXT\' u val:0  lc 7 title \'Post\'>> tmp
gnuplot tmp

#val=1000
valB=`echo $width "* .5" | bc`
valB1=`echo $valB - 5 | bc`
valB2=`echo $valB + 5 | bc`
echo val=$valB > tmp
echo set ytic 250 >> tmp
echo set xlabel \'meters\' >> tmp
echo set yr [*:*] reverse >> tmp
echo set output \'transB.png\' >> tmp
echo set term  png size 512,1024  >> tmp
echo set style data lines >> tmp
echo set title \'Transit at $valB\' >> tmp
echo plot \'${bigmap}PreTopo.TXT\' u val:0 lc -1 lw 2title \'Pre\', \'${bigmap}PostTopo.TXT\' u val:0  lc 6 title \'Post\'>> tmp
gnuplot tmp

valC=`echo $width "* .75" | bc`
valC1=`echo $valC - 5 | bc`
valC2=`echo $valC + 5 | bc`
echo val=$valC > tmp
echo set ytic 250 >> tmp
echo set xlabel \'meters\' >> tmp
echo set yr [*:*] reverse >> tmp
echo set output \'transC.png\' >> tmp
echo set term  png size 512,1024  >> tmp
echo set style data lines >> tmp
echo set title \'Transit at $valC\' >> tmp
echo plot \'${bigmap}PreTopo.TXT\' u val:0 lc -1 lw 2 title \'Pre\', \'${bigmap}PostTopo.TXT\' u val:0  lc 9 title \'Post\'>> tmp
gnuplot tmp

endWidth=`echo $width + 32 | bc`
hack1="line $valA,32 $valA,$endWidth     line $valA1,500 $valA2,500     line $valA1,1000 $valA2,1000     line $valA1,1500 $valA2,1500  line $valA1,2000 $valA2,2000"
hack2="line $valB,32 $valB,$endWidth     line $valB1,500 $valB2,500     line $valB1,1000 $valB2,1000     line $valB1,1500 $valB2,1500  line $valB1,2000 $valB2,2000"
hack3="line $valC,32 $valC,$endWidth     line $valC1,500 $valC2,500     line $valC1,1000 $valC2,1000     line $valC1,1500 $valC2,1500  line $valC1,2000 $valC2,2000"
convert -stroke red -draw "$hack1"  -stroke blue -draw "$hack2" -stroke purple -draw "$hack3" blink.gif trans.gif
#convert -stroke blue -draw "$hack2"  hold.png trans.gif


##############################################################
echo "<HR>" >> $outFile
echo "<table> <tr><td> " >> $outFile
##############################################################
# Go back to PRE and make those items
##############################################################
cd $prePath
echo "<Center>Pre</Center>" >> $outFile


echo "showmap <BR><img src=${bigmap}Pre.jpg  width=500><BR>" >> $outFile
echo "view_map_rgb <BR><img src=${bigmap}RGB-Pre.jpg  ><BR>" >> $outFile
echo "view_map_stereo <BR><img src=${bigmap}stereo-Pre.jpg  ><BR>" >> $outFile
echo "Hill Shade <BR><img src=preShade.png  width=500><BR>" >> $outFile
echo "<BR><BR>"
echo "Topo <BR><pre>$scale1</pre><BR><img src=preTopo.png  width=500><BR>" >> $outFile

echo "Links<UL>" >> $outFile
echo "<LI><a href=${bigmap}PreTopo.TXT>$bigmap in ASCII format</a>" >> $outFile
echo "<LI><a href=preTopo.cub>$bigmap in ISIS format</a>" >> $outFile
echo "<LI><a href=${bigmap}Pre.obj>$bigmap in OBJ format</a>" >> $outFile
echo "</UL>" >> $outFile
echo "<BR><HR><BR>" >> $outFile
echo "The first number is the maximum sigma value for the brighest pixel.  The second number is the average.  Units are in km.<BR>" >> $outFile
echo "SIGMAS.pgm <BR><img src=sigmaPre.jpg  width=500><BR>" >> $outFile

echo "<HR><H3>Statistics</H3><BR>" >> $outFile
echo "<pre>$outPre2</pre><BR><HR>" >> $outFile
echo "<pre>$outPre1</pre><BR>" >> $outFile


##############################################################
# Go back to Post and make those
##############################################################
echo "</td><td>" >> $outFile

cd $postPath
echo "<Center>Post</Center>" >> $outFile
echo "showmap <BR><img src=${bigmap}Post.jpg  width=500><BR>" >> $outFile
echo "view_map_rgb <BR><img src=${bigmap}RGB-Post.jpg  ><BR>" >> $outFile
echo "view_map_stereo <BR><img src=${bigmap}stereo-Post.jpg  ><BR>" >> $outFile
echo "Hill Shade <BR><img src=postShade.png  width=500><BR>" >> $outFile
echo "<BR><BR>"
echo "Topo <BR><pre>$scale2</pre><BR><img src=postTopo.png  width=500><BR>" >> $outFile


echo "Links<UL>" >> $outFile
echo "<LI><a href=${bigmap}PreTopo.TXT>$bigmap in ASCII format</a>" >> $outFile
echo "<LI><a href=preTopo.cub>$bigmap in ISIS format</a>" >> $outFile
echo "<LI><a href=${bigmap}Pre.obj>$bigmap in OBJ format</a>" >> $outFile
echo "</UL>" >> $outFile
echo "<BR><HR><BR>" >> $outFile
echo "The first number is the maximum sigma value for the brighest pixel.  The second number is the average.  Units are in km.<BR>" >> $outFile
echo "SIGMAS.pgm <BR><img src=sigmaPost.jpg  width=500><BR>" >> $outFile

echo "<HR><H3>Statistics</H3><BR>" >> $outFile
echo "<pre>$outPost2</pre><BR><HR>" >> $outFile
echo "<pre>$outPost1</pre><BR>" >> $outFile

echo "</td></tr></table>" >> $outFile

##############################################################
# Transits
##############################################################
echo "<HR>" >> $outFile

echo "<H3>Transits</H3> <BR>" >> $outFile

echo "Black is the baseline topography for each transit.  The transit goes from top to bottom.  Y units are pixels.  X units is meters" >> $outFile
echo "<BR><img src=transA.png width=500> " >> $outFile
echo "<img src=transB.png width=500> " >> $outFile
echo "<img src=transC.png width=500> " >> $outFile
echo "<BR><BR><HR>" >> $outFile
echo "<BR><img src=trans.gif width=1000> Tick every 500 pixels" >> $outFile
echo "<BR><BR><HR>" >> $outFile


##############################################################
# Photometric Cubes
##############################################################
echo "<HR>" >> $outFile

echo "<H3>PhotoCubes</H3> -- Following this are the photmetric cubes for bigmap $bigmap<BR><BR>" >> $outFile





