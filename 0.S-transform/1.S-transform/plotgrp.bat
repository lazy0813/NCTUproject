rem gawk "{print $1,$2}" %1 | minmax -I0.1
rem gawk "{print $1,$2}" %2 | minmax -I0.1
rem gawk "{print $3}" %2 | minmax -I1.0 -C

REM INPUT ARGUMENT
REM %1 RAW TIME-SERIES FILE NAME
REM %2 STFT OUTPUT FILE NAME
REM %3 DATA RANGE OF %1 -RXMIN/XMAX/-1/1
REM %4 DATA RANGE OF %2 -RXMIN/XMAX/fMIN/fMAX
REM %5 DATA RANGE OF stft AMPLITUDE -TAMIN/AMAX FOR COLOR PALETTE
REM %6 GRID SIZE OF X-AXIS
REM %7 GRID SIZE OF Y-AXIS
REM %8 POSTCRIPT FILE NAME
REM %9 DISPERSION FILE NAME WITH SADDLE OR CONST. FRE MAXIMA
REM --------------

REM SET PARAMETER 
set B1=-Ba5f5::wS
set B2=-Ba0.1f0.1:"Frequency [Hz]":/f0.1a0.5:"Group velocity [km/s]"::.:WSne
set J1=-JX7./1.0
set J2=-JX7./3.
set rm=0.01
REM -------------

gmtset X_ORIGIN 0.8i Y_ORIGIN 8i
gmtset PLOT_DEGREE_FORMAT +ddd:mm:ssF                                                                                               
gmtset BASEMAP_TYPE plain                                                                                              
gmtset ANNOT_FONT_SIZE 10p ANNOT_OFFSET_PRIMARY 0.075i LABEL_OFFSET 0.085i LABEL_FONT_SIZE 10p FRAME_WIDTH 0.035i TICK_LENGTH 0.000i
gmtset ANNOT_FONT_SIZE 10p ANNOT_OFFSET_PRIMARY 0.075i LABEL_OFFSET 0.085i LABEL_FONT_SIZE 10p FRAME_WIDTH 0.035i TICK_LENGTH 0.000i
gmtset COLOR_BACKGROUND 0/0/200
gmtset COLOR_FOREGROUND 255/204/243
gmtset COLOR_NAN       0/0/0

REM  MAKE NORMALIED GRID-FILE
REM echo gridding ...
gawk "{print $2,$4,$3}" %2 | xyz2grd -Ggrd.grd -I%6/%7 %4 -N0. -V -f
REM -------------------------
grdinfo grd.grd

REM MAKE COLOR PALETTE
makecpt -Cseis %5/0.05 -Z -I -N0. -V > cpt.cpt
REM makecpt -Csealand %5/0.5 -Z -I -N0. -V > cpt.cpt
REM echo 0.0 0  0 205    0.7 0   250 250 > cpt.cpt
REM echo 0.7 0  250 250  0.8 250  250 0 >> cpt.cpt
REM echo 0.8 250  250 0  1.0 200 0 0 >> cpt.cpt
REM echo 0.5 200 0 0 1.0 100 0 0 >> cpt.cpt
REM echo B 200	0	 0  >> cpt.cpt
REM echo F 200 0 0 >> cpt.cpt
REM echo N 200 0 0 >> cpt.cpt 
REM ------------------

REM PLOT RAW TIME-SERIES
gawk "{print $1,$2}" %1 | psxy %3 %B1% %J1% -W1 -K -V -P > %8
REM --------------------

REM PLOT TIME-FREQUENCY  
psbasemap %4 %B2% %J2%  -V -K -O -Y-4 -P >> %8
grdimage grd.grd -V -O -K -R -J  -Ccpt.cpt >> %8
REM gawk "{print $1,$2,$3,%6,%7}" %2 | psxy  -R -J -Sr1.0 -Ccpt.cpt  -K -O -V  >> %8
gawk "$4> %rm% {print $2,$3}" %9 | psxy -R -J -Sc0.1 -K -O -V >> %8
REM -------------------

REM PLOT COLOR PALETTE
psscale -Ccpt.cpt -D3.5/3.5/6.c/.5ch -O -K -B0.2:""::.: >> %8
REM ------------------

REM gsview32 %8
del .gmt* *.grd *.cpt