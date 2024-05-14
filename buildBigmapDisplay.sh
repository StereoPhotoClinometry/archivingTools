#/bin/sh

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

mkdir $bigmap

##############################################################
# Make header
##############################################################
echo "<H1>$bigmap</H1><BR><BR> " > $outFile

##############################################################
# Create PRE 
##############################################################
cd $prePath
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
$altwg/Maplet2FITS MAPFILES/$bigmap.MAP tmp.fits >> $outPath/log
$altwg/FITS2OBJ --local tmp.fits $outPath/${bigmap}Pre.obj >> $outPath/log
outPre1=`$altwg/PrintShapeModelStatistics $outPath/${bigmap}Post.obj `


##############################################################
# Create POST 
##############################################################
cd $postPath
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
$altwg/Maplet2FITS MAPFILES/$bigmap.MAP tmp.fits >> $outPath/log
$altwg/FITS2OBJ --local tmp.fits $outPath/${bigmap}Post.obj >> $outPath/log
outPost1=`$altwg/PrintShapeModelStatistics $outPath/${bigmap}Post.obj `

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

algebra from=preTopo.cub from2=postTopo.cub to=diff.cub
isis2std from=diff.cub to=diff.png
echo "Delta<BR><img src=diff.png width=500> <BR><BR><HR>" >> $outFile



##############################################################
echo "<HR>" >> $outFile
echo "<table> <tr style="vertical-align:top"><td> " >> $outFile
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
echo "<pre>$outPost1</pre><BR>" >> $outFile

echo "</td></tr></table>" >> $outFile

##############################################################
# Photometric Cubes
##############################################################
echo "<HR>" >> $outFile

echo "<H3>PhotoCubes</H3> -- Following this are the photmetric cubes for bigmap $bigmap<BR><BR>" >> $outFile





