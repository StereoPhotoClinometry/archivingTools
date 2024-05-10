#/bin/sh

bigmap=$1
width=1001
exPath="/opt/local/spc/bin"
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

############################################################33
# Make header
############################################################33
echo "<H1>$bigmap</H1><BR><BR> " > $outFile

############################################################33
# Create PRE stuff
############################################################33
cd $prePath


echo $bigmap | showmap
convert  $bigmap.pgm $outPath/${bigmap}Pre.jpg

echo "FlatMapVect"
echo $bigmap | flatMapVec

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

#echo $bigmap | $exPath/phasei
#ascii2isis from=$bigmap-a.TXT to=a.cub samples=$width lines=$width
#ascii2isis from=$bigmap-e.TXT to=e.cub samples=$width lines=$width
#ascii2isis from=$bigmap-i.TXT to=i.cub samples=$width lines=$width
#
#ascii2isis from=$bigmap-x.TXT to=x.cub samples=$width lines=$width
#ascii2isis from=$bigmap-y.TXT to=y.cub samples=$width lines=$width
#ascii2isis from=$bigmap-z.TXT to=z.cub samples=$width lines=$width
#ascii2isis from=$bigmap-lat.TXT to=lat.cub samples=$width lines=$width
#ascii2isis from=$bigmap-lon.TXT to=lon.cub samples=$width lines=$width
#echo "x.cub" > tmpList
#echo "y.cub" >> tmpList
#echo "z.cub" >> tmpList
#cubeit from=tmpList to=$outPath/xyz1.cub
#echo "lat.cub" > tmpList
#echo "lon.cub" >> tmpList
#cubeit from=tmpList to=$outPath/lat.cub

#rm a.cub e.cub i.cub x.cub y.cub z.cub lat.cub lon.cub
#rm $bigmap-a.TXT # EEP - continue



############################################################33
# Create POST stuff
############################################################33
cd $postPath

echo $bigmap | showmap
convert $bigmap.pgm $outPath/${bigmap}Post.jpg


echo $bigmap | flatMapVec
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

############################################################33
# Create and display global stuff
############################################################33
cd $outPath

shade from=preTopo.cub to=tmp.cub azimuth=270 zenith=60 PIXELRESOL=1.00
isis2std from=tmp.cub to=preShade.png

shade from=postTopo.cub to=tmp.cub azimuth=270 zenith=60 PIXELRESOL=.001
isis2std from=tmp.cub to=postShade.png

convert -adjoin  -delay 100 preShade.png postShade.png blink.gif
echo "showmap<BR><img src=blink.gif size=50%> <BR><BR><HR>" >> $outFile

algebra from=preTopo.cub from2=postTopo.cub to=diff.cub
isis2std from=diff.cub to=diff.png
echo "Delta<BR><img src=diff.png> <BR><BR><HR>" >> $outFile



############################################################33
echo "<HR>" >> $outFile
echo "<table> <tr><td> " >> $outFile
############################################################33
# Go back to PRE and make those items
############################################################33
cd $prePath
echo "<Center>Pre</Center>" >> $outFile

/bin/mv $bigmap.TXT $outPath/${bigmap}PreTopo.TXT

echo "showmap <BR><img src=${bigmap}Pre.jpg ><BR>" >> $outFile
echo "view_map_rgb <BR><img src=${bigmap}RGB-Pre.jpg ><BR>" >> $outFile
echo "view_map_stereo <BR><img src=${bigmap}stereo-Pre.jpg ><BR>" >> $outFile
echo "Links<UL>" >> $outFile
echo "<LI><a href=${bigmap}PreTopo.TXT>$bigmap in ASCII format</a>" >> $outFile
echo "<LI><a href=preTopo.cub>$bigmap in ISIS format</a>" >> $outFile
echo "<LI><a href=${bigmap}Pre.obj>$bigmap in OBJ format</a>" >> $outFile
echo "</UL>" >> $outFile



############################################################33
# Go back to Post and make those
############################################################33
echo "</td><td>" >> $outFile

cd $postPath
echo "<Center>Post</Center>" >> $outFile
echo "showmap <BR><img src=${bigmap}Post.jpg ><BR>" >> $outFile
echo "view_map_rgb <BR><img src=${bigmap}RGB-Post.jpg ><BR>" >> $outFile
echo "view_map_stereo <BR><img src=${bigmap}stereo-Post.jpg ><BR>" >> $outFile

echo "Links<UL>" >> $outFile
echo "<LI><a href=${bigmap}PreTopo.TXT>$bigmap in ASCII format</a>" >> $outFile
echo "<LI><a href=preTopo.cub>$bigmap in ISIS format</a>" >> $outFile
echo "<LI><a href=${bigmap}Pre.obj>$bigmap in OBJ format</a>" >> $outFile
echo "</UL>" >> $outFile

echo "</td></tr></table>" >> $outFile

############################################################33
# Photometric Cubes
############################################################33
echo "<HR>" >> $outFile

echo "<H3>PhotoCubes</H3> -- Following this are the photmetric cubes for bigmap $bigmap<BR><BR>" >> $outFile





