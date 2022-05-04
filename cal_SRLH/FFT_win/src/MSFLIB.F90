$freeform
MODULE msflib
!* fgraph.fi - declare constants and functions for graphics library
!*
!*   Copyright (c) 1987-1995 Microsoft Corporation.  All rights reserved.
!*
!* Purpose:
!*   This file declares the graphics library functions and
!*   the constants that are used with them.
!*

!* NOTE: When modifying this file, you must follow the following rules to
!* ensure compatibility with fixed form and free form source:
!*   1) All comments must start with '!'
!*   2) All code must be within columns 7 to 72
!*   3) For continuation, place a '&' in column 73,
!*      and a '&' in column 6 of the continuation line

$if .not. defined (_MSFORTRAN_)
$define _MSFORTRAN_ = 100
$endif

$if .not. defined($MSFLIB$ProgramUnitNumber)
$define $MSFLIB$ProgramUnitNumber = -1
$endif
$if .not. defined($MSFLIB$FIProgUnitNum) 
$define $MSFLIB$FIProgUnitNum = -2
$endif

$if .not. defined (__MSFLIB_FLIB_INCLUDE)


!     User-visible declarations for FORTRAN Graphics Library

      INTERFACE
        FUNCTION arc(x1,y1,x2,y2,x3,y3,x4,y4)
          INTEGER*2 arc[C,ALIAS:"__arc"],x1,y1,x2,y2,x3,y3,x4,y4
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION arc_w(wx1,wy1,wx2,wy2,wx3,wy3,wx4,wy4)
          INTEGER*2 arc_w[C,ALIAS:"__arc_w"]
          DOUBLE PRECISION wx1,wy1,wx2,wy2,wx3,wy3,wx4,wy4
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getarcinfo(lpstart,lpend,lppaint)
          INTEGER*2 getarcinfo[C,ALIAS:"__getarcinfo"]
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/lpstart[REFERENCE]
          RECORD/xycoord/lpend[REFERENCE]
          RECORD/xycoord/lppaint[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE clearscreen[C,ALIAS:"__FQclearscreen"](area)
          INTEGER*2 area
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION displaycursor(toggle)
          INTEGER*2 displaycursor[C,ALIAS:"__FQdisplaycursor"],toggle
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION ellipse(control,x1,y1,x2,y2)
          INTEGER*2 ellipse[C,ALIAS:"__ellipse"],control,x1,y1,x2,y2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION ellipse_w(control,wx1,wy1,wx2,wy2)
          INTEGER*2 ellipse_w[C,ALIAS:"__ellipse_w"],control
          DOUBLE PRECISION wx1,wy1,wx2,wy2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION floodfill(x,y,boundary)
          INTEGER*2 floodfill[C,ALIAS:"__floodfill"],x,y,boundary
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION floodfillrgb(x,y,bcolor)
          INTEGER*2 floodfillrgb[C,ALIAS:"__floodfillrgb"],x,y
          INTEGER*4 bcolor
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION floodfill_w(wx1,wy1,boundary)
          INTEGER*2 floodfill_w[C,ALIAS:"__floodfill_w"],boundary
          DOUBLE PRECISION wx1,wy1
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION floodfillrgb_w(wx,wy,bcolor)
          INTEGER*2 floodfillrgb_w[C,ALIAS:"__floodfillrgb_w"]
          DOUBLE PRECISION wx,wy
          INTEGER*4 bcolor
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getactivepage()
          INTEGER*2 getactivepage[C,ALIAS:"__getactivepage"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getbkcolor()
          INTEGER*4 getbkcolor[C,ALIAS:"__getbkcolor"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getcolor()
          INTEGER*2 getcolor[C,ALIAS:"__getcolor"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE getcurrentposition[ALIAS:"__f_getcurrentposition@4"](s)
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getcurrentposition_w[ALIAS:"__f_getcurrentposition_wxy@4"](s)
          STRUCTURE/wxycoord/
              DOUBLE PRECISION wx
              DOUBLE PRECISION wy
          END STRUCTURE
          RECORD/wxycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getfillmask[C,ALIAS:"__getfillmask"](mask)
          INTEGER*1 mask[REFERENCE](8)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getimage[C,ALIAS:"__getimage"](x1,y1,x2,y2,image)
          INTEGER*2 x1,y1,x2,y2
          INTEGER*1 image[REFERENCE](*)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getimage_w[C,ALIAS:"__getimage_w"](wx1,wy1,wx2,wy2,image)
          DOUBLE PRECISION wx1,wy1,wx2,wy2
          INTEGER*1 image[REFERENCE](*)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION getlinestyle()
          INTEGER*2 getlinestyle[C,ALIAS:"__getlinestyle"]
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE getphyscoord[ALIAS:"__f_getphyscoord@12"](x,y,s)
          INTEGER*2 x,y
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE GETPHYSCOORD
      SUBROUTINE $$MSFLIB$GETPHYSCOORD$22                               &
     & [alias:'__f_getphyscoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETPHYSCOORD$24                               &
     & [alias:'__f_getphyscoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETPHYSCOORD$42                               &
     & [alias:'__f_getphyscoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETPHYSCOORD$44                               &
     & [alias:'__f_getphyscoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif



      INTERFACE
        FUNCTION getpixel(x,y)
          INTEGER*2 getpixel[C,ALIAS:"__getpixel"],x,y
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getpixel_w(wx,wy)
          INTEGER*2 getpixel_w[C,ALIAS:"__getpixel_w"]
          DOUBLE PRECISION wx,wy
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE getpixels[C, ALIAS:"__getpixels"](n, x, y, c)
          INTEGER*4 n                 ! input : size of arrays
          INTEGER*2 x[REFERENCE](*)   ! input : x coordinates
          INTEGER*2 y[REFERENCE](*)   ! input : y coordinates
          INTEGER*2 c[REFERENCE](*)   ! input : palette indices
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION gettextcolor()
          INTEGER*2 gettextcolor[C,ALIAS:"__gettextcolor"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION gettextcursor()
          INTEGER*2 gettextcursor[C,ALIAS:"__gettextcursor"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE gettextposition[ALIAS:"__fq_gettextposition@4"](s)
          STRUCTURE/rccoord/
              INTEGER*2 row
              INTEGER*2 col
          END STRUCTURE
          RECORD/rccoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE gettextwindow[C,ALIAS:"__gettextwindow"](r1,c1,r2,c2)
          INTEGER*2 r1[REFERENCE],c1[REFERENCE]
          INTEGER*2 r2[REFERENCE],c2[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getvideoconfig[C,ALIAS:"__getvideoconfig"](s)
          STRUCTURE/videoconfig/
              INTEGER*2 numxpixels       ! number of pixels on X axis
              INTEGER*2 numypixels       ! number of pixels on Y axis
              INTEGER*2 numtextcols      ! number of text columns available
              INTEGER*2 numtextrows      ! number of text rows available
              INTEGER*2 numcolors        ! number of actual colors
              INTEGER*2 bitsperpixel     ! number of bits per pixel
              INTEGER*2 numvideopages    ! number of available video pages
              INTEGER*2 mode             ! current video mode
              INTEGER*2 adapter          ! active display adapter
              INTEGER*2 monitor          ! active display monitor
              INTEGER*2 memory           ! adapter video memory in K bytes
          END STRUCTURE
          RECORD/videoconfig/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE getviewcoord[ALIAS:"__f_getviewcoord@12"](x,y,s)
          INTEGER*2 x,y
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE GETVIEWCOORD
      SUBROUTINE $$MSFLIB$GETVIEWCOORD$22                               &
     & [alias:'__f_getviewcoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETVIEWCOORD$24                               &
     & [alias:'__f_getviewcoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETVIEWCOORD$42                               &
     & [alias:'__f_getviewcoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETVIEWCOORD$44                               &
     & [alias:'__f_getviewcoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif


      INTERFACE
        SUBROUTINE getviewcoord_w[ALIAS:"__f_getviewcoord_w@12"](wx,wy,s)
          DOUBLE PRECISION wx,wy
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION getvisualpage()
          INTEGER*2 getvisualpage[C,ALIAS:"__getvisualpage"]
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE getwindowcoord[ALIAS:"__f_getwindowcoord@12"](x,y,s)
          INTEGER*2 x,y
          STRUCTURE/wxycoord/
            DOUBLE PRECISION wx
            DOUBLE PRECISION wy
          END STRUCTURE
          RECORD/wxycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE GETWINDOWCOORD
      SUBROUTINE $$MSFLIB$GETWINDOWCOORD$22                             &
     & [alias:'__f_getwindowcoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*2 y
      STRUCTURE/wxycoord/
         DOUBLE PRECISION wx
        DOUBLE PRECISION wy
      END STRUCTURE
      RECORD/wxycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETWINDOWCOORD$24                             &
     & [alias:'__f_getwindowcoord@12'](x,y,s)
      INTEGER*2 x
      INTEGER*4 y
      STRUCTURE/wxycoord/
         DOUBLE PRECISION wx
        DOUBLE PRECISION wy
      END STRUCTURE
      RECORD/wxycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETWINDOWCOORD$42                             &
     & [alias:'__f_getwindowcoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*2 y
      STRUCTURE/wxycoord/
        DOUBLE PRECISION wx
        DOUBLE PRECISION wy
      END STRUCTURE
      RECORD/wxycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$GETWINDOWCOORD$44                             &
     & [alias:'__f_getwindowcoord@12'](x,y,s)
      INTEGER*4 x
      INTEGER*4 y
      STRUCTURE/wxycoord/
        DOUBLE PRECISION wx
        DOUBLE PRECISION wy
      END STRUCTURE
      RECORD/wxycoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif



      INTERFACE
        FUNCTION getwritemode()
          INTEGER*2 getwritemode[C,ALIAS:"__getwritemode"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION grstatus()
          INTEGER*2 grstatus[C,ALIAS:"__grstatus"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION imagesize(x1,y1,x2,y2)
          INTEGER*4 imagesize[C,ALIAS:"__imagesize"]
          INTEGER*2 x1,y1,x2,y2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION imagesize_w(wx1,wy1,wx2,wy2)
          INTEGER*4 imagesize_w[C,ALIAS:"__imagesize_w"]
          DOUBLE PRECISION wx1,wy1,wx2,wy2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION lineto(x,y)
          INTEGER*2 lineto[C,ALIAS:"__lineto"],x,y
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION lineto_w(wx,wy)
          INTEGER*2 lineto_w[C,ALIAS:"__lineto_w"]
          DOUBLE PRECISION wx,wy
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION loadimage[ALIAS:"__f_loadimage@16"](fname,x,y)
          CHARACTER*(*) fname[REFERENCE]
          INTEGER*4 x,y
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION loadimage_w[C, ALIAS:"__loadimage_w"](fname,wx,wy)
          CHARACTER*(*) fname[REFERENCE]
          DOUBLE PRECISION wx, wy
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE moveto[ALIAS:"__f_moveto@12"](x,y,s)
          INTEGER*2 x,y
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE MOVETO
      SUBROUTINE $$MSFLIB$MOVETO$22[alias:'__f_moveto@12'](x,y,s)
      INTEGER*2 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$MOVETO$24[alias:'__f_moveto@12'](x,y,s)
      INTEGER*2 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$MOVETO$42[alias:'__f_moveto@12'](x,y,s)
      INTEGER*4 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$MOVETO$44[alias:'__f_moveto@12'](x,y,s)
      INTEGER*4 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif

      INTERFACE
        SUBROUTINE moveto_w[ALIAS:"__f_moveto_w@12"](wx,wy,s)
          DOUBLE PRECISION wx,wy
          STRUCTURE/wxycoord/
              DOUBLE PRECISION wx
              DOUBLE PRECISION wy
          END STRUCTURE
          RECORD/wxycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE outtext[ALIAS:"__fq_outtext@8"](text)
          CHARACTER*(*) text[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION pie(i,x1,y1,x2,y2,x3,y3,x4,y4)
          INTEGER*2 pie[C,ALIAS:"__pie"],i,x1,y1,x2,y2,x3,y3,x4,y4
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION pie_w(i,wx1,wy1,wx2,wy2,wx3,wy3,wx4,wy4)
          INTEGER*2 pie_w[C,ALIAS:"__pie_w"],i
          DOUBLE PRECISION wx1,wy1,wx2,wy2,wx3,wy3,wx4,wy4
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION polygon(control,lppoints,cpoints)
          INTEGER*2 polygon[C,ALIAS:"__polygon"],control,cpoints
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/lppoints[REFERENCE](*)
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION polygon_w(control,lppoints,cpoints)
          INTEGER*2 polygon_w[C,ALIAS:"__polygon_w"],control,cpoints
          STRUCTURE/wxycoord/
              DOUBLE PRECISION wx
              DOUBLE PRECISION wy
          END STRUCTURE
          RECORD/wxycoord/lppoints[REFERENCE](*)
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE putimage[C,ALIAS:"__putimage"](x,y,image,action)
          INTEGER*2 x,y,action
          INTEGER*1 image[REFERENCE](*)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE putimage_w[C,ALIAS:"__putimage_w"](wx,wy,image,action)
          DOUBLE PRECISION wx,wy
          INTEGER*1 image[REFERENCE](*)
          INTEGER*2 action
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION rectangle(control,x1,y1,x2,y2)
          INTEGER*2 rectangle[C,ALIAS:"__rectangle"]
          INTEGER*2 control,x1,y1,x2,y2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION rectangle_w(control,wx1,wy1,wx2,wy2)
          INTEGER*2 rectangle_w[C,ALIAS:"__rectangle_w"],control
          DOUBLE PRECISION wx1,wy1,wx2,wy2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION remappalette(index,color)
          INTEGER*4 remappalette[C,ALIAS:"__remappalette"],color
          INTEGER*2 index
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION remappalettergb(index,color)
          INTEGER*4 remappalettergb[C,ALIAS:"__remappalettergb"],color
          INTEGER*2 index
        END FUNCTION
      END INTERFACE


      INTERFACE
        FUNCTION remapallpalette(colors)
          INTEGER*2 remapallpalette[C,ALIAS:"__remapallpalette"]
          INTEGER*4 colors[REFERENCE](*)
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION remapallpalettergb(colors)
          INTEGER*2 remapallpalettergb[C,ALIAS:"__remapallpalettergb"]
          INTEGER*4 colors[REFERENCE](*)
        END FUNCTION
      END INTERFACE


      INTERFACE
        INTEGER*4 FUNCTION saveimage[ALIAS:"__f_saveimage@24"](fname,x1,y1,x2,y2)
          CHARACTER*(*) fname[REFERENCE]
          INTEGER*4 x1,y1,x2,y2
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION saveimage_w[C, ALIAS:"__saveimage_w"](fname,wx1,wy1,wx2,wy2)
          CHARACTER*(*) fname[REFERENCE]
          DOUBLE PRECISION wx1,wy1,wx2,wy2
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE scrolltextwindow[C,ALIAS:"__FQscrolltextwindow"](rows)
          INTEGER*2 rows
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION selectpalette(number)
          INTEGER*2 selectpalette[C,ALIAS:"__selectpalette"],number
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setactivepage(page)
          INTEGER*2 setactivepage[C,ALIAS:"__setactivepage"],page
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setbkcolor(color)
          INTEGER*4 setbkcolor[C,ALIAS:"__setbkcolor"],color
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE setcliprgn[C,ALIAS:"__setcliprgn"](x1,y1,x2,y2)
          INTEGER*2 x1,y1,x2,y2
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION setcolor(color)
          INTEGER*2 setcolor[C,ALIAS:"__setcolor"]
          INTEGER*2 color
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE setfillmask[C,ALIAS:"__setfillmask"](mask)
          INTEGER*1 mask[REFERENCE](8)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE setlinestyle[C,ALIAS:"__setlinestyle"](mask)
          INTEGER*2 mask
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION setpixel(x,y)
          INTEGER*2 setpixel[C,ALIAS:"__setpixel"],x,y
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setpixel_w(wx,wy)
          INTEGER*2 setpixel_w[C,ALIAS:"__setpixel_w"]
          DOUBLE PRECISION wx,wy
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE setpixels[C, ALIAS:"__setpixels"](n, x, y, c)
          INTEGER*4 n                 ! input : size of arrays
          INTEGER*2 x[REFERENCE](*)   ! input : x coordinates
          INTEGER*2 y[REFERENCE](*)   ! input : y coordinates
          INTEGER*2 c[REFERENCE](*)   ! input : palette indices
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION settextcolor(index)
          INTEGER*2 settextcolor[C,ALIAS:"__settextcolor"],index
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION settextcursor(attr)
          INTEGER*2 settextcursor[C,ALIAS:"__settextcursor"],attr
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE settextposition[ALIAS:"__fq_settextposition@12"](row, col, s)
          INTEGER*2 row,col
          STRUCTURE/rccoord/
              INTEGER*2 row
              INTEGER*2 col
          END STRUCTURE
          RECORD/rccoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE SETTEXTPOSITION
      SUBROUTINE $$MSFLIB$SETTEXTPOSITION$22                            &
     & [alias:'__fq_settextposition@12'](row, col, s)
      INTEGER*2 row
      INTEGER*2 col
      STRUCTURE/rccoord/
          INTEGER*2 row
          INTEGER*2 col
      END STRUCTURE
      RECORD/rccoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETTEXTPOSITION$24                            &
     & [alias:'__fq_settextposition@12'](row, col, s)
      INTEGER*2 row
      INTEGER*4 col
      STRUCTURE/rccoord/
          INTEGER*2 row
          INTEGER*2 col
      END STRUCTURE
      RECORD/rccoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETTEXTPOSITION$42                            &
     & [alias:'__fq_settextposition@12'](row, col, s)
      INTEGER*4 row
      INTEGER*2 col
      STRUCTURE/rccoord/
          INTEGER*2 row
          INTEGER*2 col
      END STRUCTURE
      RECORD/rccoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETTEXTPOSITION$44                            &
     & [alias:'__fq_settextposition@12'](row, col, s)
      INTEGER*4 row
      INTEGER*4 col
      STRUCTURE/rccoord/
          INTEGER*2 row
          INTEGER*2 col
      END STRUCTURE
      RECORD/rccoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif


      INTERFACE
        FUNCTION settextrows(rows)
          INTEGER*2 settextrows[C,ALIAS:"__settextrows"],rows
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE settextwindow[C,ALIAS:"__settextwindow"](r1,c1,r2,c2)
          INTEGER*2 r1,c1,r2,c2
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION setvideomode(mode)
          INTEGER*2 setvideomode[C,ALIAS:"__setvideomode"],mode
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setvideomoderows(mode,rows)
          INTEGER*2 setvideomoderows[C,ALIAS:"__setvideomoderows"]
          INTEGER*2 mode,rows
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .lt. 300
      INTERFACE
        SUBROUTINE setvieworg[ALIAS:"__f_setvieworg@12"](x,y,s)
          INTEGER*2 x,y
          STRUCTURE/xycoord/
              INTEGER*2 xcoord
              INTEGER*2 ycoord
          END STRUCTURE
          RECORD/xycoord/s[REFERENCE]
        END SUBROUTINE
      END INTERFACE
$else
      INTERFACE SETVIEWORG
      SUBROUTINE $$MSFLIB$SETVIEWORG$22                                 &
     & [alias:'__f_setvieworg@12'](x,y,s)
      INTEGER*2 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETVIEWORG$24                                 &
     & [alias:'__f_setvieworg@12'](x,y,s)
      INTEGER*2 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETVIEWORG$42                                 &
     & [alias:'__f_setvieworg@12'](x,y,s)
      INTEGER*4 x
      INTEGER*2 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      SUBROUTINE $$MSFLIB$SETVIEWORG$44                                 &
     & [alias:'__f_setvieworg@12'](x,y,s)
      INTEGER*4 x
      INTEGER*4 y
      STRUCTURE/xycoord/
          INTEGER*2 xcoord
          INTEGER*2 ycoord
      END STRUCTURE
      RECORD/xycoord/s[REFERENCE]
      END SUBROUTINE
      END INTERFACE
$endif

      INTERFACE
        SUBROUTINE setviewport[C,ALIAS:"__setviewport"](x1,y1,x2,y2)
          INTEGER*2 x1,y1,x2,y2
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION setvisualpage(page)
          INTEGER*2 setvisualpage[C,ALIAS:"__setvisualpage"],page
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setwindow(finvert,wx1,wy1,wx2,wy2)
          INTEGER*2 setwindow[C,ALIAS:"__setwindow"]
          LOGICAL*2 finvert
          DOUBLE PRECISION wx1,wy1,wx2,wy2
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setwritemode(wmode)
          INTEGER*2 setwritemode[C,ALIAS:"__setwritemode"],wmode
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION wrapon(option)
          INTEGER*2 wrapon[C,ALIAS:"__wrapon"],option
        END FUNCTION
      END INTERFACE

! FONTS

      INTERFACE
        FUNCTION getfontinfo(fi)
          INTEGER*2 getfontinfo[ALIAS:"__f_getfontinfo@4"]
          STRUCTURE/fontinfo/
              INTEGER*4 type        ! b0 set = vector,clear = bit map
              INTEGER*4 ascent      ! pix dist from top to baseline
              INTEGER*4 pixwidth    ! character width in pixels, 0=prop
              INTEGER*4 pixheight   ! character height in pixels
              INTEGER*4 avgwidth    ! average character width in pixels
              CHARACTER*81 filename ! file name including path
              CHARACTER*32 facename ! font name
              LOGICAL*1 italic      ! .true. - font  is italic
              LOGICAL*1 emphasized  ! .true. - font is  emphasized (bold)
              LOGICAL*1 underline   ! .true. - font is underline
          END STRUCTURE
          RECORD/fontinfo/fi[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getgtextextent(text)
          INTEGER*2 getgtextextent[ALIAS:"__f_getgtextextent@8"]
          CHARACTER*(*) text[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE getgtextvector[ALIAS:"__f_getgtextvector@8"](x,y)
          INTEGER*2 x[REFERENCE],y[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE outgtext[ALIAS:"__f_outgtext@8"](text)
          CHARACTER*(*) text[REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION registerfonts(filename)
          INTEGER*2 registerfonts[ALIAS:"__f_registerfonts@8"]
          CHARACTER*(*) filename[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION initializefonts()
          INTEGER*2 initializefonts[ALIAS:"__initializefonts"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setfont(options)
          INTEGER*2 setfont[ALIAS:"__f_setfont@8"]
          CHARACTER*(*) options[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE settextfont[ALIAS:"__f_settextfont@4"] (fontname)
          CHARACTER*(*) fontname[REFERENCE]
        END SUBROUTINE
      END INTERFACE


      INTERFACE
        SUBROUTINE setgtextvector[C,ALIAS:"__setgtextvector"](x,y)
          INTEGER*2 x,y
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE unregisterfonts[C,ALIAS:"__unregisterfonts"]()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE setgtextrotation[C,ALIAS:"__setgtextrotation"](degrees)
          INTEGER*4 degrees
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION getgtextrotation()
          INTEGER*4 getgtextrotation[C,ALIAS:"__getgtextrotation"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getcolorrgb()
          INTEGER*4 getcolorrgb[C,ALIAS:"__getcolorrgb"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getbkcolorrgb()
          INTEGER*4 getbkcolorrgb[C,ALIAS:"__getbkcolorrgb"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION gettextcolorrgb()
          INTEGER*4 gettextcolorrgb[C,ALIAS:"__gettextcolorrgb"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getpixelrgb(x,y)
          INTEGER*4 getpixelrgb[C,ALIAS:"__getpixelrgb"]
          INTEGER*2 x
          INTEGER*2 y
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION getpixelrgb_w(wx, wy)
          INTEGER*4 getpixelrgb_w[C,ALIAS:"__getpixelrgb_w"]
          REAL*8          wx
          REAL*8          wy
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE setpixelsrgb[C,ALIAS:"__setpixelsrgb"](n,x,y,color)
          INTEGER*4 n
          INTEGER*2 x [REFERENCE](*)
          INTEGER*2 y [REFERENCE](*)
          INTEGER*4 color [REFERENCE](*)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE getpixelsrgb[C,ALIAS:"__getpixelsrgb"](n,x,y,color)
          INTEGER*4 n
          INTEGER*2 x [REFERENCE](*)
          INTEGER*2 y [REFERENCE](*)
          INTEGER*4 color [REFERENCE](*)
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION setcolorrgb(color)
          INTEGER*4 setcolorrgb[C,ALIAS:"__setcolorrgb"]
          INTEGER*4 color
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setbkcolorrgb(color)
          INTEGER*4 setbkcolorrgb[C,ALIAS:"__setbkcolorrgb"]
          INTEGER*4 color
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setpixelrgb(x,y,color)
          INTEGER*4 setpixelrgb[C,ALIAS:"__setpixelrgb"]
          INTEGER*2 x
          INTEGER*2 y
          INTEGER*4 color
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION setpixelrgb_w(x,y,color)
          INTEGER*4 setpixelrgb_w[C,ALIAS:"__setpixelrgb_w"]
          REAL*8          x
          REAL*8          y
          INTEGER*4       color
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION settextcolorrgb(color)
          INTEGER*4 settextcolorrgb[C,ALIAS:"__settextcolorrgb"]
          INTEGER*4 color
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION rgbtointeger(red,green,blue)
          INTEGER*4 rgbtointeger[C,ALIAS:"__rgbtointeger"]
          INTEGER*4 red,green,blue
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE integertorgb[C,ALIAS:"__integertorgb"] (rgb, red, green, blue)
          INTEGER*4 rgb
          INTEGER*4 red    [REFERENCE]
          INTEGER*4 green  [REFERENCE]
          INTEGER*4 blue   [REFERENCE]
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE setdblclkinterval[C,ALIAS:"_setdblclkintervalqq"](interval)
          INTEGER interval
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        FUNCTION unregistermouseevent(Unit,MouseEvents)
          INTEGER unregistermouseevent[C,ALIAS:"_unregistermouseeventqq"]
          INTEGER Unit
          INTEGER MouseEvents
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION registermouseevent(Unit,MouseEvents,CallBackRoutine)
          INTEGER registermouseevent[C,ALIAS:"_registermouseeventqq"]
          INTEGER Unit
          INTEGER MouseEvents
          EXTERNAL CallBackRoutine
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION waitonmouseevent(MouseEvents,KeyState,x,y)
          INTEGER waitonmouseevent[C,ALIAS:"_waitonmouseeventqq"]
          INTEGER MouseEvents
          INTEGER KeyState [REFERENCE]
          INTEGER x [REFERENCE]
          INTEGER y [REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE setframewindow[C,ALIAS:"__setframewindow"](X,Y,width,height)
          INTEGER X
          INTEGER Y
          INTEGER width
          INTEGER height
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE setmessageqq[C,ALIAS:"__f_setmessageqq"](msg, id)
          CHARACTER*(*) msg[reference]
          INTEGER*4     id
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE setstatusmessage[C,ALIAS:"__f_setstatusmessage@8"](msg, id)
          CHARACTER*(*) msg[reference]
          INTEGER*4     id
        END SUBROUTINE
      END INTERFACE

! We only include the following code once inside the same program unit.
! This code is shared with flib.fi.
$if _MSFORTRAN_ .lt. 300   ! if using /4fps1, DO NOT include the code below
$undefine $MSFLIB$FIProgUnitNum
$define $MSFLIB$FIProgUnitNum = -1
$endif
$if $MSFLIB$FIProgUnitNum .ne. $MSFLIB$ProgramUnitNumber
$undefine $MSFLIB$FIProgUnitNum
$define $MSFLIB$FIProgUnitNum = $MSFLIB$ProgramUnitNumber

      INTERFACE
        FUNCTION INCHARQQ()
          INTEGER*2 INCHARQQ[C,ALIAS:"__inchar"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGOPENQQ(name)
          INTEGER*4 WGOPENQQ[C,ALIAS:"__wgopen"]
          CHARACTER*(*) name
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGCLOSEQQ(handle)
          INTEGER*4 WGCLOSEQQ[C,ALIAS:"__wgclose"], handle
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGSETACTIVEQQ(handle)
          INTEGER*4 WGSETACTIVEQQ[C,ALIAS:"__wgsetactive"], handle
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGGETACTIVEQQ()
          INTEGER*4 WGGETACTIVEQQ[C,ALIAS:"__wggetactive"]
        END FUNCTION
      END INTERFACE
$endif


$endif  ! $if .not. defined (__MSFLIB_FLIB_INCLUDE)
!****************************** Module Header ******************************
!*
!* Copyright (c) 1990-1995  Microsoft Corporation
!*
!* Module Name: FLIB.FI
!*
!* This module provides interface descriptions for subprograms found
!* in the various Fortran libraries.  
!*
!* The functional areas covered are the following.
!*
!*    Math error support
!*    Signal support
!*    Coprocessor Control
!*    Time and Date Support
!*    Command Line Arguments
!*    Random Numbers
!*    Running Programs and System Commands
!*    Drives and Directories
!*    Files
!*    Keyboard
!*    Errors
!*    Environment
!*    Beep and Sleep
!*    Sorting and Searching Arrays
!*    Configuration
!*    QuickWin Support
!*    Access to Windows Handles for QuickWin components
!*    QuickWin Default Menu Support
!*    
!*
!***************************************************************************

!* NOTE: When modifying this file, you must follow the following rules to
!* ensure compatibility with fixed form and free form source:
!*   1) All comments must start with '!'
!*   2) All code must be within columns 7 to 72
!*   3) For continuation, place a '&' in column 73,
!*      and a '&' in column 6 of the continuation line

$if .not. defined (_MSFORTRAN_)
$define _MSFORTRAN_ = 100
$endif

$if .not. defined($MSFLIB$ProgramUnitNumber)
$define $MSFLIB$ProgramUnitNumber = -1
$endif
$if .not. defined($MSFLIB$FIProgUnitNum)
$define $MSFLIB$FIProgUnitNum = -2
$endif

$if .not. defined (__MSFLIB_FGRAPH_INCLUDE)


! -----------------------------------------------------------------
! Math Error Support
! -----------------------------------------------------------------
! MATHERRQQ -- user callbacks cannot be in the module MSFLIB

! -----------------------------------------------------------------
! Signal support
! -----------------------------------------------------------------
      INTERFACE
        INTEGER*4 FUNCTION SIGNALQQ[c,alias:'_signal'](SIGNAL, HANDLER)
          INTEGER*4 SIGNAL[value]
          integer*4 handler
          EXTERNAL HANDLER
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION RAISEQQ[c,alias:'_raise'](SIGNAL)
          INTEGER*4 SIGNAL[value]
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Coprocessor Control
! -----------------------------------------------------------------

      INTERFACE
        SUBROUTINE LCWRQQ(CONTROL)
          INTEGER*2 CONTROL
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE SCWRQQ(CONTROL)
          INTEGER*2 CONTROL
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE SSWRQQ(STATUS)
          INTEGER*2 STATUS
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE SETCONTROLFPQQ[alias:'_LCWRQQ@4'](CONTROL)
          INTEGER*2 CONTROL
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE GETCONTROLFPQQ[alias:'_SCWRQQ@4'](CONTROL)
          INTEGER*2 CONTROL
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE GETSTATUSFPQQ[alias:'_SSWRQQ@4'](STATUS)
          INTEGER*2 STATUS
        END SUBROUTINE
      END INTERFACE

! -----------------------------------------------------------------
! Time and Date Support
! -----------------------------------------------------------------
      INTERFACE
        SUBROUTINE GETTIM(IHR, IMIN, ISEC, I100TH)
          INTEGER*2 IHR, IMIN, ISEC, I100TH
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        LOGICAL FUNCTION SETTIM(IHR, IMIN, ISEC, I100TH)
          INTEGER*2 IHR, IMIN, ISEC, I100TH
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE GETDAT(IYR, IMON, IDAY)
          INTEGER*2 IYR, IMON, IDAY
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        LOGICAL FUNCTION SETDAT(IYR, IMON, IDAY)
          INTEGER*2 IYR, IMON, IDAY
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Command Line Arguments
! -----------------------------------------------------------------
      INTERFACE
        INTEGER*4 FUNCTION NARGS()
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .LT. 300
      INTERFACE
        SUBROUTINE GETARG(N, BUFFER, STATUS)
          INTEGER*2 N
          CHARACTER*(*) BUFFER
          INTEGER*2 STATUS          
        END SUBROUTINE
      END INTERFACE

      ! Note that EXIT does not have an interface for FORTRAN 77 compiles.  
      ! Thus, EXIT can be used with or without an argument
      ! (so long as it is used consistently within a program unit).
$else
      INTERFACE
        SUBROUTINE GETARG(N, BUFFER, STATUS)
          INTEGER*2 N
          CHARACTER*(*) BUFFER
          INTEGER*2, OPTIONAL :: STATUS      
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE EXIT(EXITVALUE)
          INTEGER*4, OPTIONAL :: EXITVALUE    
        END SUBROUTINE
      END INTERFACE
$endif

! -----------------------------------------------------------------
! Random Numbers
! -----------------------------------------------------------------

$if _MSFORTRAN_ .LT. 300
      INTERFACE
        SUBROUTINE RANDOM(ARG)
          REAL*4 ARG
        END SUBROUTINE
      END INTERFACE
$else
! Random is a generic interface for use with the RANDOM in portlib
      INTERFACE RANDOM
         SUBROUTINE $$MSFLIB$RANDOM(ARG)
!MS$ATTRIBUTES ALIAS:'_RANDOM@4' :: $$MSFLIB$RANDOM
         REAL*4 ARG
         END SUBROUTINE
      END INTERFACE
$endif

      INTERFACE
        SUBROUTINE SEED(ARG)
          INTEGER*4 ARG
        END SUBROUTINE
      END INTERFACE

! -----------------------------------------------------------------
! Running Programs and System Commands
! -----------------------------------------------------------------
      INTERFACE
        INTEGER*4 FUNCTION RUNQQ(FILENAME, COMMANDLINE)
          CHARACTER*(*) FILENAME, COMMANDLINE
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION SYSTEMQQ(COMMANDLINE)
          CHARACTER*(*) COMMANDLINE
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Drives and Directories
! -----------------------------------------------------------------

      INTERFACE
        LOGICAL*4 FUNCTION GETDRIVESIZEQQ(DRIVE, TOTAL,AVAIL)
          CHARACTER*(*) DRIVE
          INTEGER*4 TOTAL, AVAIL
        END FUNCTION
      END INTERFACE

      INTERFACE
        CHARACTER*26 FUNCTION GETDRIVESQQ()
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION GETDRIVEDIRQQ(BUFFER)
          CHARACTER*(*) BUFFER
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION CHANGEDRIVEQQ(DRIVE)
          CHARACTER*(*) DRIVE
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION CHANGEDIRQQ(DIR)
          CHARACTER*(*) DIR
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION MAKEDIRQQ(DIR)
          CHARACTER*(*) DIR
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION DELDIRQQ(DIR)
          CHARACTER*(*) DIR
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION FULLPATHQQ(NAME, FULLPATH)
          CHARACTER*(*) NAME, FULLPATH
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION SPLITPATHQQ(PATH, DRIVE, DIR,NAME, EXT)
          CHARACTER*(*) PATH, DRIVE, DIR, NAME, EXT
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Files
! -----------------------------------------------------------------

      INTERFACE
        INTEGER*4 FUNCTION GETFILEINFOQQ(FILES, BUFFER,HANDLE)
          CHARACTER*(*) FILES
          STRUCTURE / FILE$INFO /
            INTEGER*4       CREATION          ! Creation time (-1 on FAT)
            INTEGER*4       LASTWRITE         ! Last write to file
            INTEGER*4       LASTACCESS        ! Last access (-1 on FAT)
            INTEGER*4       LENGTH            ! Length of file
            INTEGER*2       PERMIT            ! File access mode
            CHARACTER*255   NAME              ! File name
          END STRUCTURE
          RECORD / FILE$INFO / BUFFER
          INTEGER*4 HANDLE
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION SETFILETIMEQQ(NAME, TIMEDATE)
          CHARACTER*(*) NAME
          INTEGER*4 TIMEDATE
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION SETFILEACCESSQQ(NAME, ACCESS)
          CHARACTER*(*) NAME
          INTEGER*4 ACCESS
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION DELFILESQQ(FILES)
          CHARACTER*(*) FILES
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION RENAMEFILEQQ(OLDNAME, NEWNAME)
          CHARACTER*(*) OLDNAME, NEWNAME
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION FINDFILEQQ(FILE, ENV, BUF)
          CHARACTER*(*) FILE, ENV, BUF
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE PACKTIMEQQ(TIMEDATE, IYR, IMON, IDAY,IHR, IMIN, ISEC)
          INTEGER*4 TIMEDATE
          INTEGER*2 IYR, IMON, IDAY, IHR, IMIN, ISEC
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE UNPACKTIMEQQ(TIMEDATE, IYR, IMON, IDAY,IHR, IMIN, ISEC)
          INTEGER*4 TIMEDATE
          INTEGER*2 IYR, IMON, IDAY, IHR, IMIN, ISEC
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION COMMITQQ(UNIT)
          INTEGER*4 UNIT
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Keyboard
! -----------------------------------------------------------------

      INTERFACE
        CHARACTER*1 FUNCTION GETCHARQQ()
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION PEEKCHARQQ()
        END FUNCTION
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION GETSTRQQ(BUFFER)
          CHARACTER*(*) BUFFER
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Errors
! -----------------------------------------------------------------

      INTERFACE
        INTEGER*4 FUNCTION GETLASTERRORQQ()
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE SETERRORMODEQQ(PROMPT)
          LOGICAL*4 PROMPT
        END SUBROUTINE
      END INTERFACE

! -----------------------------------------------------------------
! Environment
! -----------------------------------------------------------------

      INTERFACE
        INTEGER*4 FUNCTION GETENVQQ(NAME, VALUE)
          CHARACTER*(*) NAME, VALUE
        END FUNCTION
      END INTERFACE

      INTERFACE
        LOGICAL*4 FUNCTION SETENVQQ(NAMVAL)
          CHARACTER*(*) NAMVAL
        END FUNCTION
      END INTERFACE


! -----------------------------------------------------------------
! Beep and Sleep
! -----------------------------------------------------------------

      INTERFACE
        SUBROUTINE SLEEPQQ(DURATION)
          INTEGER*4 DURATION
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE BEEPQQ(FREQUENCY, DURATION)
          INTEGER*4 FREQUENCY, DURATION
        END SUBROUTINE
      END INTERFACE

! ----------------------------------------------------------------
! Sorting and Searching Arrays
! ----------------------------------------------------------------

      INTERFACE
        SUBROUTINE SORTQQ(ADRARRAY, LENGTH, SIZE)
          INTEGER*4 ADRARRAY, LENGTH, SIZE
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        INTEGER*4 FUNCTION BSEARCHQQ(ADRKEY, ADRARRAY,LENGTH, SIZE)
          INTEGER*4 ADRKEY, ADRARRAY, LENGTH, SIZE
        END FUNCTION
      END INTERFACE

! ----------------------------------------------------------------
! Configuration
! ----------------------------------------------------------------

      INTERFACE
        INTEGER*4 FUNCTION GETCONFIGQQ()
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! QuickWin Support
! -----------------------------------------------------------------
$PACK:1

      INTERFACE
        FUNCTION GETWINDOWCONFIG(wc)
          LOGICAL GETWINDOWCONFIG[ALIAS:"__f_getwindowconfig@4"]
          STRUCTURE/windowconfig/
          INTEGER*2 numxpixels
          INTEGER*2 numypixels
          INTEGER*2 numtextcols
          INTEGER*2 numtextrows
          INTEGER*2 numcolors
          INTEGER*4 fontsize
          CHARACTER*(80) title
          INTEGER*2 bitsperpixel
          INTEGER*2 numvideopages
          INTEGER*2 mode
          INTEGER*2 adapter
          INTEGER*2 monitor
          INTEGER*2 memory
          INTEGER*2 environment
          CHARACTER*(32) extendfontname
          INTEGER*4 extendfontsize
          INTEGER*4 extendfontattributes
          END STRUCTURE
          RECORD/windowconfig/wc[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION SETWINDOWCONFIG(wc)
          LOGICAL SETWINDOWCONFIG[ALIAS:"__f_setwindowconfig@4"]
          STRUCTURE/windowconfig/
          INTEGER*2 numxpixels
          INTEGER*2 numypixels
          INTEGER*2 numtextcols
          INTEGER*2 numtextrows
          INTEGER*2 numcolors
          INTEGER*4 fontsize
          CHARACTER*(80) title
          INTEGER*2 bitsperpixel
          INTEGER*2 numvideopages
          INTEGER*2 mode
          INTEGER*2 adapter
          INTEGER*2 monitor
          INTEGER*2 memory
          INTEGER*2 environment
          CHARACTER*(32) extendfontname
          INTEGER*4 extendfontsize
          INTEGER*4 extendfontattributes
          END STRUCTURE
          RECORD/windowconfig/wc[REFERENCE]
        END FUNCTION
      END INTERFACE
$PACK

      INTERFACE
        FUNCTION APPENDMENUQQ(menuID,flags,text,routine)
          LOGICAL APPENDMENUQQ[C]
          INTEGER*4 menuID,flags
          CHARACTER*(*) text[REFERENCE]
          EXTERNAL routine
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION INSERTMENUQQ(menuID,itemID,flags,text,routine)
          LOGICAL INSERTMENUQQ[C]
          INTEGER*4 menuID,itemID,flags
          CHARACTER*(*) text[REFERENCE]
          EXTERNAL routine
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION DELETEMENUQQ(menuID,itemID)
          LOGICAL DELETEMENUQQ[C]
          INTEGER*4 menuID,itemID
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION MODIFYMENUFLAGSQQ(menuID,itemID,flags)
          LOGICAL MODIFYMENUFLAGSQQ[C]
          INTEGER*4 menuID,itemID,flags
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION MODIFYMENUSTRINGQQ(menuID,itemID,text)
          LOGICAL MODIFYMENUSTRINGQQ[C]
          INTEGER*4 menuID,itemID
          CHARACTER*(*) text[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION MODIFYMENUROUTINEQQ(menuID,itemID,routine)
          LOGICAL MODIFYMENUROUTINEQQ[C]
          INTEGER*4 menuID,itemID
          EXTERNAL routine
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION SETWINDOWMENUQQ(menuID)
          LOGICAL SETWINDOWMENUQQ[C]
          INTEGER*4 menuID
        END FUNCTION
      END INTERFACE

      INTERFACE
        SUBROUTINE YIELDQQ()
        END SUBROUTINE
      END INTERFACE

! We only include the following code once inside the same program unit.
! This code is shared with fgraph.fi.
$if _MSFORTRAN_ .lt. 300    ! if using /4fps1, always include the code below
$undefine $MSFLIB$FIProgUnitNum
$define $MSFLIB$FIProgUnitNum = -2
$endif
$if $MSFLIB$FIProgUnitNum .ne. $MSFLIB$ProgramUnitNumber
$undefine $MSFLIB$FIProgUnitNum
$define $MSFLIB$FIProgUnitNum = $MSFLIB$ProgramUnitNumber

      INTERFACE
        FUNCTION INCHARQQ()
          INTEGER*2 INCHARQQ[C,ALIAS:"__inchar"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGOPENQQ(name)
          INTEGER*4 WGOPENQQ[C,ALIAS:"__wgopen"]
          CHARACTER*(*) name
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGCLOSEQQ(handle)
          INTEGER*4 WGCLOSEQQ[C,ALIAS:"__wgclose"], handle
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGSETACTIVEQQ(handle)
          INTEGER*4 WGSETACTIVEQQ[C,ALIAS:"__wgsetactive"], handle
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION WGGETACTIVEQQ()
          INTEGER*4 WGGETACTIVEQQ[C,ALIAS:"__wggetactive"]
        END FUNCTION
      END INTERFACE
$endif

      INTERFACE
        FUNCTION SETACTIVEQQ(lunit)
          INTEGER*4 SETACTIVEQQ[C,ALIAS:"__wgsetactiveunit"], lunit
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETACTIVEQQ()
          INTEGER*4 GETACTIVEQQ[C,ALIAS:"__wggetactiveunit"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION FOCUSQQ(IUNIT)
          INTEGER*4 FOCUSQQ, IUNIT
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION INQFOCUSQQ(IUNIT)
          INTEGER*4 INQFOCUSQQ,IUNIT
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETHANDLEQQ(IUNIT)
          INTEGER*4 GETHANDLEQQ, IUNIT
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETHWNDQQ(IUNIT)
          INTEGER*4 GETHWNDQQ, IUNIT
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETUNITQQ(IHANDLE)
          INTEGER*4 GETUNITQQ, IHANDLE
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION ABOUTBOXQQ(STR)
          INTEGER*4 ABOUTBOXQQ[C,ALIAS:"__QWINSetAboutString"]
          CHARACTER*(*) STR[REFERENCE]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION CLICKQQ(ITEM)
          INTEGER*4 CLICKQQ[C,ALIAS:"__QWINMenuClick"],ITEM
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION CLICKMENUQQ(ITEM)
          INTEGER*4 CLICKMENUQQ[C,ALIAS:"__QWINMenuClick"]
          INTEGER*4 ITEM
        END FUNCTION
      END INTERFACE


      INTERFACE
        FUNCTION SETWSIZEQQ(IUNIT,WINFO)
          STRUCTURE /QWINFO/
              INTEGER*2 TYPE          ! request type
              INTEGER*2 X             ! x coordinate for upper left
              INTEGER*2 Y             ! y coordinate for upper left
              INTEGER*2 H             ! window height
              INTEGER*2 W             ! window width
          END STRUCTURE
          INTEGER*4 SETWSIZEQQ, IUNIT
          RECORD /QWINFO/ WINFO
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETWSIZEQQ(IUNIT,IREQ,WINFO)
          STRUCTURE /QWINFO/
              INTEGER*2 TYPE          ! request type
              INTEGER*2 X             ! x coordinate for upper left
              INTEGER*2 Y             ! y coordinate for upper left
              INTEGER*2 H             ! window height
              INTEGER*2 W             ! window width
          END STRUCTURE
          INTEGER*4 GETWSIZEQQ, IUNIT
          INTEGER*4 IREQ
          RECORD /QWINFO/ WINFO
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION MESSAGEBOXQQ(MSG,CAPTION,MTYPE)
          CHARACTER*(*) MSG[REFERENCE]
          CHARACTER*(*) CAPTION[REFERENCE]
          INTEGER*4  MESSAGEBOXQQ[C,ALIAS:"__QWINMsgBox"],MTYPE
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETEXITQQ()
          INTEGER*4 GETEXITQQ[C, ALIAS:"__QWINGetExit"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION SETEXITQQ(exitmode)
          INTEGER*4 SETEXITQQ[C, ALIAS:"__QWINSetExit"]
          INTEGER*4 exitmode
        END FUNCTION
      END INTERFACE

! -----------------------------------------------------------------
! Access to Windows Handles for QuickWin components
! -----------------------------------------------------------------
      INTERFACE
        FUNCTION GETHANDLEFRAMEQQ()
          INTEGER*4 GETHANDLEFRAMEQQ[C,ALIAS:"__QWINTGethFrame"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETHANDLECLIENTQQ()
          INTEGER*4 GETHANDLECLIENTQQ[C,ALIAS:"__QWINTGethClient"]
        END FUNCTION
      END INTERFACE

      INTERFACE
        FUNCTION GETHANDLECHILDQQ(QUICKHND)
          INTEGER*4 GETHANDLECHILDQQ[C,ALIAS:"__QWINGethWnd"]
          INTEGER*4 QUICKHND
        END FUNCTION
      END INTERFACE

$if _MSFORTRAN_ .GE. 300
! -----------------------------------------------------------------
! Unused Routine -- simply returns, used to avoid "unused" warnings
! -----------------------------------------------------------------
      INTERFACE
        SUBROUTINE UNUSEDQQ[C,REFERENCE,VARYING,ALIAS:"__FFunusedqq"]()
        END SUBROUTINE
      END INTERFACE
$endif
      
! -----------------------------------------------------------------
! QuickWin Default Menu Support
! -----------------------------------------------------------------
! INITIALMENU -- user callbacks cannot be in the module MSFLIB
! INITIALSETTINGS -- user callbacks cannot be in the module MSFLIB

      INTERFACE
        SUBROUTINE WINPRINT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSAVE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINEXIT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINCOPY()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINPASTE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSIZETOFIT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINFULLSCREEN()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSTATE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINCASCADE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINTILE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINARRANGE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WININPUT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINCLEARPASTE()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSTATUS()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WININDEX()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINUSING()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINABOUT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSELECTTEXT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSELECTGRAPHICS()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE WINSELECTALL()
        END SUBROUTINE
      END INTERFACE

      INTERFACE
        SUBROUTINE NUL()
        END SUBROUTINE
      END INTERFACE


$endif  ! $if .not. defined (__MSFLIB_FGRAPH_INCLUDE)
!* fgraph.fd - declare constants and functions for graphics library
!*
!*   Copyright (c) 1987-1995 Microsoft Corporation.  All rights reserved.
!*
!* Purpose:
!*   This file declares the graphics library functions and
!*   the constants that are used with them.
!*

!* NOTE: When modifying this file, you must follow the following rules to
!* ensure compatibility with fixed form and free form source:
!*   1) All comments must start with '!'
!*   2) All code must be within columns 7 to 72
!*   3) For continuation, place a '&' in column 73,
!*      and a '&' in column 6 of the continuation line

$if .not. defined (_MSFORTRAN_)
$define _MSFORTRAN_ = 100
$endif
$if_MSFORTRAN_ .LT. 300
$NOTSTRICT         ! uses structures which are non-standard conforming
$endif

$if .not. defined($MSFLIB$ProgramUnitNumber)
$define $MSFLIB$ProgramUnitNumber = -1
$endif
$if .not. defined($MSFLIB$FDProgUnitNum) 
$define $MSFLIB$FDProgUnitNum = -2
$endif

$if .not. defined(__MSFLIB_FLIB_INCLUDE)


!     User-visible declarations for FORTRAN Graphics Library

!     Structure for getvideoconfig() as visible to user
      STRUCTURE/videoconfig/
        INTEGER*2 numxpixels       ! number of pixels on X axis
        INTEGER*2 numypixels       ! number of pixels on Y axis
        INTEGER*2 numtextcols      ! number of text columns available
        INTEGER*2 numtextrows      ! number of text rows available
        INTEGER*2 numcolors        ! number of actual colors
        INTEGER*2 bitsperpixel     ! number of bits per pixel
        INTEGER*2 numvideopages    ! number of available video pages
        INTEGER*2 mode             ! current video mode
        INTEGER*2 adapter          ! active display adapter
        INTEGER*2 monitor          ! active display monitor
        INTEGER*2 memory           ! adapter video memory in K bytes
      END STRUCTURE

!     Return value of getcurrentposition(), etc.
      STRUCTURE/xycoord/
        INTEGER*2 xcoord
        INTEGER*2 ycoord
      END STRUCTURE

!     Structure for text position
      STRUCTURE/rccoord/
        INTEGER*2 row
        INTEGER*2 col
      END STRUCTURE

!     ERROR HANDLING

!     Status info returned by grstatus()
      INTEGER*2 $GRPARAMETERALTERED,$GRCLIPPED,$GRNOOUTPUT,$GROK,       &
     &            $GRERROR,$GRMODENOTSUPPORTED,$GRNOTINPROPERMODE,      &
     &            $GRINVALIDPARAMETER,$GRFONTFILENOTFOUND,              &
     &            $GRINVALIDFONTFILE,$GRCORRUPTEDFONTFILE,              &
     &            $GRINSUFFICIENTMEMORY,$GRINVALIDIMAGEBUFFER,          &
     &            $GRNOBITMAPFILE, $GRFILEREADERROR,                    &
     &            $GRIMPROPERBITMAPFORMAT, $GRBITMAPTOOLARGE,           &
     &            $GRBITMAPDISPLAYERROR, $GRIMAGEREADERROR,             &
     &            $GRFILEOPENERROR, $GRFILEWRITERROR
!     Successful
      PARAMETER($GROK                     = 0)  ! success
!     Warnings
      PARAMETER($GRNOOUTPUT               = 1)  ! nothing drawn
      PARAMETER($GRCLIPPED                = 2)  ! output was partially clipped
      PARAMETER($GRPARAMETERALTERED       = 3)  ! input parameters adjusted
!     Errors
      PARAMETER($GRERROR                  = -1)  ! generic graphics error
      PARAMETER($GRMODENOTSUPPORTED       = -2)  ! video mode not supported
      PARAMETER($GRNOTINPROPERMODE        = -3)  ! not supported in current mode
      PARAMETER($GRINVALIDPARAMETER       = -4)  ! bad parameter
      PARAMETER($GRFONTFILENOTFOUND       = -5)  ! font file not found
      PARAMETER($GRINVALIDFONTFILE        = -6)  ! not a font file
      PARAMETER($GRCORRUPTEDFONTFILE      = -7)  ! inconsistent font file
      PARAMETER($GRINSUFFICIENTMEMORY     = -8)  ! out of memory
      PARAMETER($GRINVALIDIMAGEBUFFER     = -9)  ! bad image buffer detected
      PARAMETER($GRNOBITMAPFILE           = -10) ! bitmap file not found
      PARAMETER($GRFILEREADERROR          = -11) ! error reading bitmap file
      PARAMETER($GRIMPROPERBITMAPFORMAT   = -12) ! bitmap format not supported
      PARAMETER($GRBITMAPTOOLARGE         = -13) ! bitmap size > window config
      PARAMETER($GRBITMAPDISPLAYERROR     = -14) ! error displaying bitmap
      PARAMETER($GRIMAGEREADERROR         = -15) ! error reading image
      PARAMETER($GRFILEOPENERROR          = -16) ! error opening bitmap file
      PARAMETER($GRFILEWRITERROR          = -17) ! error writing bitmap file

!     SETUP AND CONFIGURATION

!     Arguments to setvideomode()
      INTEGER*2 $MAXRESMODE,$MAXCOLORMODE,$DEFAULTMODE,$TEXTBW40,       &
     &            $TEXTC40,$TEXTBW80,$TEXTC80,$TEXTMONO,$MRES16COLOR,   &
     &            $HRES16COLOR,$ERESNOCOLOR,$ERESCOLOR,$VRES2COLOR,     &
     &            $VRES16COLOR,$MRES256COLOR,$ORESCOLOR,$ORES256COLOR,  &
     &            $VRES256COLOR,$SRES16COLOR,$SRES256COLOR,$XRES16COLOR,&
     &            $XRES256COLOR,$ZRES16COLOR,$ZRES256COLOR
      PARAMETER($MAXRESMODE   =-3)    ! graphics mode with highest resolution
      PARAMETER($MAXCOLORMODE =-2)    ! graphics mode with most colors
      PARAMETER($DEFAULTMODE  =-1)    ! restore screen to original mode
      PARAMETER($TEXTBW40      =0)    ! 40 x 25 text, 16 grey
      PARAMETER($TEXTC40       =1)    ! 40 x 25 text, 16/8 color
      PARAMETER($TEXTBW80      =2)    ! 80 x 25 text, 16 grey
      PARAMETER($TEXTC80       =3)    ! 80 x 25 text, 16/8 color
      PARAMETER($TEXTMONO      =7)    ! 80 x 25 text, BW
      PARAMETER($MRES16COLOR   =13)   ! 320 x 200, 16 color
      PARAMETER($HRES16COLOR   =14)   ! 640 x 200, 16 color
      PARAMETER($ERESNOCOLOR   =15)   ! 640 x 350, BW
      PARAMETER($ERESCOLOR     =16)   ! 640 x 350, 4 or 16 color
      PARAMETER($VRES2COLOR    =17)   ! 640 x 480, BW
      PARAMETER($VRES16COLOR   =18)   ! 640 x 480, 16 color
      PARAMETER($MRES256COLOR  =19)   ! 320 x 200, 256 color
      PARAMETER($ORESCOLOR     =64)   ! 640 x 400, 1 of 16 colors (Olivetti)

!     The following 8 modes require VESA SuperVGA BIOS extensions
      PARAMETER($ORES256COLOR  =#0100)        ! 640 x 400, 256 color
      PARAMETER($VRES256COLOR  =#0101)        ! 640 x 480, 256 color

!     WARNING: DO NOT attempt to set the following modes without ensuring
!     that your monitor can safely handle that resolution. Otherwise, you
!     may risk damaging your display monitor! Consult your owner's manual
!     for details.
!     Note: _MAXRESMODE and _MAXCOLORMODE never select SRES, XRES, or ZRES
!     modes.

!     Requires NEC MultiSync 3D or equivalent, or better
      PARAMETER($SRES16COLOR   =#0102)        ! 800 x 600, 16 color
      PARAMETER($SRES256COLOR  =#0103)        ! 800 x 600, 256 color

!     Requires NEC MultiSync 4D or equivalent, or better
      PARAMETER($XRES16COLOR   =#0104)        ! 1024 x 768, 16 color
      PARAMETER($XRES256COLOR  =#0105)        ! 1024 x 768, 256 color

!     Requires NEC MultiSync 5D or equivalent, or better
      PARAMETER($ZRES16COLOR   =#0106)        ! 1280 x 1024, 16 color
      PARAMETER($ZRES256COLOR  =#0107)        ! 1280 x 1024, 256 color

!     VIDEOCONFIG adapter values

!     These constants can be used to determine the type of the active
!     adapter, using either simple comparisons or the bitwise-AND
!     operator
      INTEGER*2 $QUICKWIN, $MDPA,$EGA,$VGA,$OEGA,$OVGA,$SVGA
      PARAMETER($QUICKWIN =#0000)  ! Quick Win Graphics is devide independent
      PARAMETER($MDPA     =#0001)  ! Monochrome Display Adapter (MDPA)
      PARAMETER($EGA      =#0004)  ! Enhanced Graphics Adapter  (EGA)
      PARAMETER($VGA      =#0008)  ! Video Graphics Array       (VGA)
      PARAMETER($OEGA     =#0044)  ! Olivetti Enhanced Graphics Adapter (OEGA)
      PARAMETER($OVGA     =#0048)  ! Olivetti Video Graphics Array (OVGA)
      PARAMETER($SVGA     =#0088)  ! Super VGA with VESA BIOS support (SVGA)

!     VIDEOCONFIG monitor values

!     These constants can be used to determine the type of monitor in use,
!     using either simple comparisons or the bitwise-AND operator
      INTEGER*2 $MONO,$COLOR,$ENHCOLOR,$ANALOGMONO,                     &
     &            $ANALOGCOLOR,$ANALOG
      PARAMETER($MONO       =#0001)     ! Monochrome
      PARAMETER($COLOR      =#0002)     ! Color (or Enhanced emulating color)
      PARAMETER($ENHCOLOR   =#0004)     ! Enhanced Color
      PARAMETER($ANALOGMONO =#0008)     ! Analog Monochrome only
      PARAMETER($ANALOGCOLOR=#0010)     ! Analog Color only
      PARAMETER($ANALOG     =#0018)     ! Analog Color + Analog Monochrome

!     COORDINATE SYSTEMS

!     OUTPUT ROUTINES

!     Control parameters for rectangle(), polygon(), ellipse(), and pie()
      INTEGER*2 $GBORDER,$GFILLINTERIOR
      PARAMETER($GBORDER       =2)      ! draw outline only
      PARAMETER($GFILLINTERIOR =3)      ! fill using current fill mask

!     Parameters for clearscreen()
      INTEGER*2 $GCLEARSCREEN,$GVIEWPORT,$GWINDOW
      PARAMETER($GCLEARSCREEN  =0)
      PARAMETER($GVIEWPORT     =1)
      PARAMETER($GWINDOW       =2)

! TEXT

!     Parameters for displaycursor()
      INTEGER*2 $GCURSOROFF,$GCURSORON
      PARAMETER($GCURSOROFF =0)
      PARAMETER($GCURSORON  =1)

!     Parameters for wrapon()
      INTEGER*2 $GWRAPOFF,$GWRAPON
      PARAMETER($GWRAPOFF  =0)
      PARAMETER($GWRAPON   =1)

!     Parameters for scrolltextwindow()
      INTEGER*2 $GSCROLLUP,$GSCROLLDOWN
      PARAMETER($GSCROLLUP   =1)
      PARAMETER($GSCROLLDOWN =-1)

!     Request maximum number of rows in _settextrows() and _setvideomoderows()
      INTEGER*2 $MAXTEXTROWS
      PARAMETER($MAXTEXTROWS =-1)

!     "Action verbs" for putimage(), setwritemode()
      INTEGER*2 $GPSET,$GPRESET,$GAND,$GOR,$GXOR
      PARAMETER($GPSET         =3)
      PARAMETER($GPRESET       =2)
      PARAMETER($GAND          =1)
      PARAMETER($GOR           =0)
      PARAMETER($GXOR          =4)

!     Color values are used with setbkcolor in graphics modes and also by
!     remappalette and remapallpalette.  Also known as palette colors.
!     Not to be confused with color indices (aka. color attributes).

!     Universal color values (all color modes):
      INTEGER*4 $BLACK,$BLUE,$GREEN,$CYAN,$RED,$MAGENTA,$BROWN,         &
     &            $WHITE,$GRAY,$LIGHTBLUE,$LIGHTGREEN,$LIGHTCYAN,       &
     &            $LIGHTRED,$LIGHTMAGENTA,$YELLOW,$BRIGHTWHITE
      PARAMETER($BLACK         =#000000)
      PARAMETER($BLUE          =#200000)
      PARAMETER($GREEN         =#002000)
      PARAMETER($CYAN          =#202000)
      PARAMETER($RED           =#000020)
      PARAMETER($MAGENTA       =#200020)
      PARAMETER($BROWN         =#002020)
      PARAMETER($WHITE         =#303030)
      PARAMETER($GRAY          =#202020)
      PARAMETER($LIGHTBLUE     =#3F0000)
      PARAMETER($LIGHTGREEN    =#003f00)
      PARAMETER($LIGHTCYAN     =#3f3f00)
      PARAMETER($LIGHTRED      =#00003f)
      PARAMETER($LIGHTMAGENTA  =#3f003f)
      PARAMETER($YELLOW        =#003f3f)
      PARAMETER($BRIGHTWHITE   =#3f3f3f)

!     The following is obsolescent and defined only for backwards
!     compatibility
      INTEGER*4 $LIGHTYELLOW
      PARAMETER($LIGHTYELLOW   =#153f3f)

!     Mono mode F ($ERESNOCOLOR) color values:
      INTEGER*4 $MODEFOFF,$MODEFOFFTOON,$MODEFOFFTOHI,$MODEFONTOOFF,    &
     &            $MODEFON,$MODEFONTOHI,$MODEFHITOOFF,$MODEFHITOON,     &
     &            $MODEFHI
      PARAMETER($MODEFOFF      =0)
      PARAMETER($MODEFOFFTOON  =1)
      PARAMETER($MODEFOFFTOHI  =2)
      PARAMETER($MODEFONTOOFF  =3)
      PARAMETER($MODEFON       =4)
      PARAMETER($MODEFONTOHI   =5)
      PARAMETER($MODEFHITOOFF  =6)
      PARAMETER($MODEFHITOON   =7)
      PARAMETER($MODEFHI       =8)

!     Mono mode 7 ($TEXTMONO) color values:
       INTEGER*4 $MODE7OFF,$MODE7ON,$MODE7HI
      PARAMETER($MODE7OFF      =0)
      PARAMETER($MODE7ON       =1)
      PARAMETER($MODE7HI       =2)

!     QWIN Message IDS
     
      INTEGER*4 QWIN$MSG_TERM ,QWIN$MSG_EXITQ, QWIN$MSG_FINISHED,        &
     & QWIN$MSG_PAUSED, QWIN$MSG_RUNNING, QWIN$MSG_FILEOPENDLG,          &
     & QWIN$MSG_BMPSAVEDLG, QWIN$MSG_INPUTPEND,                          &
     & QWIN$MSG_PASTEINPUTPEND, QWIN$MSG_MOUSEINPUTPEND,                 &
     & QWIN$MSG_SELECTTEXT, QWIN$MSG_SELECTGRAPHICS,                     &
     & QWIN$MSG_PRINTABORT, QWIN$MSG_PRINTLOAD, QWIN$MSG_PRINTNODEFAULT, &
     & QWIN$MSG_PRINTDRIVER, QWIN$MSG_PRINTINGERROR, QWIN$MSG_PRINTING,  &
     & QWIN$MSG_PRINTCANCEL, QWIN$MSG_PRINTINPROGRESS,                   &
     & QWIN$MSG_HELPNOTAVAIL, QWIN$MSG_TITLETEXT

      PARAMETER (QWIN$MSG_TERM              =1)
      PARAMETER (QWIN$MSG_EXITQ =2)
      PARAMETER (QWIN$MSG_FINISHED=3)
      PARAMETER (QWIN$MSG_PAUSED =4)
      PARAMETER (QWIN$MSG_RUNNING =5)
      PARAMETER (QWIN$MSG_FILEOPENDLG =6)
      PARAMETER (QWIN$MSG_BMPSAVEDLG =7)
      PARAMETER (QWIN$MSG_INPUTPEND =8)
      PARAMETER (QWIN$MSG_PASTEINPUTPEND =9)
      PARAMETER (QWIN$MSG_MOUSEINPUTPEND =10)
      PARAMETER (QWIN$MSG_SELECTTEXT =11)
      PARAMETER (QWIN$MSG_SELECTGRAPHICS =12)
      PARAMETER (QWIN$MSG_PRINTABORT =13)
      PARAMETER (QWIN$MSG_PRINTLOAD =14)
      PARAMETER (QWIN$MSG_PRINTNODEFAULT =15)
      PARAMETER (QWIN$MSG_PRINTDRIVER =16)
      PARAMETER (QWIN$MSG_PRINTINGERROR =17)
      PARAMETER (QWIN$MSG_PRINTING =18)
      PARAMETER (QWIN$MSG_PRINTCANCEL =19)
      PARAMETER (QWIN$MSG_PRINTINPROGRESS =20)
      PARAMETER (QWIN$MSG_HELPNOTAVAIL =21)
      PARAMETER (QWIN$MSG_TITLETEXT =22)

!     WINDOW COORDINATE SYSTEM

!     Structure for window coordinate pair
      STRUCTURE/wxycoord/
        DOUBLE PRECISION wx   ! window x coordinate
        DOUBLE PRECISION wy   ! window y coordinate
      END STRUCTURE

!     Window coordinate entry points for graphics output routines

 
      STRUCTURE/fontinfo/
        INTEGER*4 type        ! b0 set = vector,clear = bit map
        INTEGER*4 ascent      ! pix dist from top to baseline
        INTEGER*4 pixwidth    ! character width in pixels, 0=prop
        INTEGER*4 pixheight   ! character height in pixels
        INTEGER*4 avgwidth    ! average character width in pixels
        CHARACTER*81 filename ! file name including path
        CHARACTER*32 facename ! font name
        LOGICAL*1 italic      ! .true. - font  is italic
        LOGICAL*1 emphasized  ! .true. - font is  emphasized (bold)
        LOGICAL*1 underline   ! .true. - font is underline
      END STRUCTURE

!     Font parameters

      INTEGER*2 $NO_SPACE,$FIXED_SPACE,$PROP_SPACE
      PARAMETER ($NO_SPACE    = 0)

      PARAMETER ($FIXED_SPACE = 1)
      PARAMETER ($PROP_SPACE  = 2)

      INTEGER*2 $NO_FONT_MAP,$VECTOR_MAP,$BIT_MAP
      PARAMETER ($NO_FONT_MAP = 0)
      PARAMETER ($VECTOR_MAP  = 1)
      PARAMETER ($BIT_MAP     = 2)



      INTEGER MOUSE$LBUTTONDOWN,MOUSE$MOVE,MOUSE$LBUTTONUP,             &
     & MOUSE$RBUTTONDOWN,MOUSE$RBUTTONUP,MOUSE$LBUTTONDBLCLK,           &
     & MOUSE$RBUTTONDBLCLK,MOUSE$KS_LBUTTON,MOUSE$KS_RBUTTON,           &
     & MOUSE$KS_SHIFT,MOUSE$KS_CONTROL,MOUSE$BADUNIT, MOUSE$BADEVENT

      PARAMETER(MOUSE$LBUTTONDOWN           = #1)
      PARAMETER(MOUSE$LBUTTONUP             = #2)
      PARAMETER(MOUSE$LBUTTONDBLCLK         = #4)
      PARAMETER(MOUSE$RBUTTONDOWN           = #8)
      PARAMETER(MOUSE$RBUTTONUP             = #10)
      PARAMETER(MOUSE$RBUTTONDBLCLK         = #20)
      PARAMETER(MOUSE$MOVE                  = #40)

      PARAMETER(MOUSE$KS_LBUTTON            = #1)
      PARAMETER(MOUSE$KS_RBUTTON            = #2)
      PARAMETER(MOUSE$KS_SHIFT              = #4)
      PARAMETER(MOUSE$KS_CONTROL            = #8)

      PARAMETER(MOUSE$BADUNIT               = -2)
      PARAMETER(MOUSE$BADEVENT              = -1)


$endif  ! $if .not. defined(__MSFLIB_FLIB_INCLUDE)
!****************************** Module Header ******************************
!*
!* Copyright (c) 1990-1995  Microsoft Corporation
!*
!* Module Name: FLIB.FD
!*
!* This module provides parameter and structure type declarations
!* needed to communicate with subprograms found in the various Fortran
!* libraries.  
!*
!* The functional areas covered are the following.
!*
!*    Data Type Codes
!*    Math error support
!*    Signal support
!*    Coprocessor Control
!*    Random Numbers
!*    Drives and Directories
!*    Files
!*    Keyboard
!*    Errors
!*    Sorting and Searching Arrays
!*    Configuration
!*    QuickWin Support
!*
!***************************************************************************

!* NOTE: When modifying this file, you must follow the following rules to
!* ensure compatibility with fixed form and free form source:
!*   1) All comments must start with '!'
!*   2) All code must be within columns 7 to 72
!*   3) For continuation, place a '&' in column 73,
!*      and a '&' in column 6 of the continuation line

$if .not. defined (_MSFORTRAN_)
$define _MSFORTRAN_ = 100
$endif

$if .not. defined($MSFLIB$ProgramUnitNumber)
$define $MSFLIB$ProgramUnitNumber = -1
$endif
$if .not. defined($MSFLIB$FDProgUnitNum) 
$define $MSFLIB$FDProgUnitNum = -2
$endif

$if .not. defined(__MSFLIB_FGRAPH_INCLUDE)


! -----------------------------------------------------------------
! Data Type Codes
! -----------------------------------------------------------------

      INTEGER*4 TY$REAL4
      INTEGER*4 TY$REAL8
      INTEGER*4 TY$CMPLX8
      INTEGER*4 TY$CMPLX16

      PARAMETER (TY$REAL4           = 1)
      PARAMETER (TY$REAL8           = 2)
      PARAMETER (TY$CMPLX8          = 3)
      PARAMETER (TY$CMPLX16         = 4)

! -----------------------------------------------------------------
! Math Error Support
! -----------------------------------------------------------------
! Codes for function failure.
      INTEGER*4 MTH$E_DOMAIN              ! Argument domain error
      INTEGER*4 MTH$E_SINGULARITY         ! Argument Singularity
      INTEGER*4 MTH$E_OVERFLOW            ! Overflow range error
      INTEGER*4 MTH$E_UNDERFLOW           ! Underflow range error
      INTEGER*4 MTH$E_TLOSS               ! Total loss of precision
      INTEGER*4 MTH$E_PLOSS               ! Partial loss of precision

      PARAMETER (MTH$E_DOMAIN      = 1)
      PARAMETER (MTH$E_SINGULARITY = 2)
      PARAMETER (MTH$E_OVERFLOW    = 3)
      PARAMETER (MTH$E_UNDERFLOW   = 4)
      PARAMETER (MTH$E_TLOSS       = 5)
      PARAMETER (MTH$E_PLOSS       = 6)

! Math error information structure.
      STRUCTURE /MTH$E_INFO/
        INTEGER*4 ERRCODE        ! INPUT : One of the MTH$ values above
        INTEGER*4 FTYPE          ! INPUT : One of the TY$ values above
        UNION
        MAP
            REAL*4 R4ARG1        ! INPUT : FIrst argument
            REAL*4 R4ARG2        ! INPUT : Second argument (if any)
            REAL*4 R4RES         ! OUTPUT : Desired result
        END MAP
        MAP
            REAL*8 R8ARG1        ! INPUT : FIrst argument
            REAL*8 R8ARG2        ! INPUT : Second argument (if any)
            REAL*8 R8RES         ! OUTPUT : Desired result
        END MAP
        MAP
            COMPLEX*8 C8ARG1     ! INPUT : FIrst argument
            COMPLEX*8 C8ARG2     ! INPUT : Second argument (if any)
            COMPLEX*8 C8RES      ! OUTPUT : Desired result
        END MAP
        MAP
            COMPLEX*16 C16ARG1   ! INPUT : FIrst argument
            COMPLEX*16 C16ARG2   ! INPUT : Second argument (if any)
            COMPLEX*16 C16RES    ! OUTPUT : Desired result
        END MAP
        END UNION
      END STRUCTURE


! -----------------------------------------------------------------
! Signal support
! -----------------------------------------------------------------
      INTEGER*4 SIG$ERR                   ! SIGNALQQ return value
      INTEGER*4 SIG$NSIG
      INTEGER*4 SIG$INT
      INTEGER*4 SIG$ILL
      INTEGER*4 SIG$FPE
      INTEGER*4 SIG$SEGV
      INTEGER*4 SIG$TERM
      INTEGER*4 SIG$USR1
      INTEGER*4 SIG$USR2
      INTEGER*4 SIG$USR3
      INTEGER*4 SIG$BREAK
      INTEGER*4 SIG$ABORT

      PARAMETER (SIG$ERR   = -1)
      PARAMETER (SIG$NSIG  = 23)
      PARAMETER (SIG$INT   =  2)
      PARAMETER (SIG$ILL   =  4)
      PARAMETER (SIG$FPE   =  8)
      PARAMETER (SIG$SEGV  = 11)
      PARAMETER (SIG$TERM  = 15)
      PARAMETER (SIG$USR1  = 16)
      PARAMETER (SIG$USR2  = 17)
      PARAMETER (SIG$USR3  = 20)
      PARAMETER (SIG$BREAK = 21)
      PARAMETER (SIG$ABORT = 22)

      INTEGER*4 FPE$INVALID
      INTEGER*4 FPE$DENORMAL
      INTEGER*4 FPE$ZERODIVIDE
      INTEGER*4 FPE$OVERFLOW
      INTEGER*4 FPE$UNDERFLOW
      INTEGER*4 FPE$INEXACT
      INTEGER*4 FPE$UNEMULATED
      INTEGER*4 FPE$SQRTNEG
      INTEGER*4 FPE$STACKOVERFLOW
      INTEGER*4 FPE$STACKUNDERFLOW
      INTEGER*4 FPE$EXPLICITGEN                 ! RAISEQQ( SIGFPE )

      PARAMETER (FPE$INVALID        = #81)
      PARAMETER (FPE$DENORMAL       = #82)
      PARAMETER (FPE$ZERODIVIDE     = #83)
      PARAMETER (FPE$OVERFLOW       = #84)
      PARAMETER (FPE$UNDERFLOW      = #85)
      PARAMETER (FPE$INEXACT        = #86)
      PARAMETER (FPE$UNEMULATED     = #87)
      PARAMETER (FPE$SQRTNEG        = #88)
      PARAMETER (FPE$STACKOVERFLOW  = #8a)
      PARAMETER (FPE$STACKUNDERFLOW = #8b)
      PARAMETER (FPE$EXPLICITGEN    = #8c)

! -----------------------------------------------------------------
! Coprocessor control
! -----------------------------------------------------------------

      INTEGER*2 FPCW$MCW_EM
      INTEGER*2 FPCW$INEXACT
      INTEGER*2 FPCW$UNDERFLOW
      INTEGER*2 FPCW$OVERFLOW
      INTEGER*2 FPCW$ZERODIVIDE
      INTEGER*2 FPCW$INVALID
      INTEGER*2 FPCW$DENORMAL

      INTEGER*2 FPCW$MCW_PC
      INTEGER*2 FPCW$64
      INTEGER*2 FPCW$53
      INTEGER*2 FPCW$24

      INTEGER*2 FPCW$MCW_IC
      INTEGER*2 FPCW$AFFINE
      INTEGER*2 FPCW$PROJECTIVE

      INTEGER*2 FPCW$MCW_RC
      INTEGER*2 FPCW$NEAR
      INTEGER*2 FPCW$DOWN
      INTEGER*2 FPCW$UP
      INTEGER*2 FPCW$CHOP

      INTEGER*2 FPSW$MSW_EM
      INTEGER*2 FPSW$INVALID
      INTEGER*2 FPSW$DENORMAL
      INTEGER*2 FPSW$ZERODIVIDE
      INTEGER*2 FPSW$OVERFLOW
      INTEGER*2 FPSW$UNDERFLOW
      INTEGER*2 FPSW$INEXACT

      PARAMETER (FPCW$MCW_EM        = #003F)  ! exception mask
      PARAMETER (FPCW$INVALID       = #0001)  ! invalid
      PARAMETER (FPCW$DENORMAL      = #0002)  ! denormal
      PARAMETER (FPCW$ZERODIVIDE    = #0004)  ! zero divide
      PARAMETER (FPCW$OVERFLOW      = #0008)  ! overflow
      PARAMETER (FPCW$UNDERFLOW     = #0010)  ! underflow
      PARAMETER (FPCW$INEXACT       = #0020)  ! inexact (precision)

      PARAMETER (FPCW$MCW_PC        = #0300)  ! precision control mask
      PARAMETER (FPCW$64            = #0300)  ! 64 bits
      PARAMETER (FPCW$53            = #0200)  ! 53 bits
      PARAMETER (FPCW$24            = #0000)  ! 24 bits

      PARAMETER (FPCW$MCW_IC        = #1000)  ! infinity control mask
      PARAMETER (FPCW$AFFINE        = #1000)  ! affine
      PARAMETER (FPCW$PROJECTIVE    = #0000)  ! projective

      PARAMETER (FPCW$MCW_RC        = #0C00)  ! rounding control mask
      PARAMETER (FPCW$CHOP          = #0C00)  ! chop
      PARAMETER (FPCW$UP            = #0800)  ! up
      PARAMETER (FPCW$DOWN          = #0400)  ! down
      PARAMETER (FPCW$NEAR          = #0000)  ! near

      PARAMETER (FPSW$MSW_EM        = #003F)  ! exception mask
      PARAMETER (FPSW$INVALID       = #0001)  ! invalid
      PARAMETER (FPSW$DENORMAL      = #0002)  ! denormal
      PARAMETER (FPSW$ZERODIVIDE    = #0004)  ! zero divide
      PARAMETER (FPSW$OVERFLOW      = #0008)  ! overflow
      PARAMETER (FPSW$UNDERFLOW     = #0010)  ! underflow
      PARAMETER (FPSW$INEXACT       = #0020)  ! inexact (precision)

! -----------------------------------------------------------------
! Random Numbers
! -----------------------------------------------------------------
      INTEGER*4 RND$TIMESEED

      PARAMETER (RND$TIMESEED = -1)

! -----------------------------------------------------------------
! Drives and Directories
! -----------------------------------------------------------------

      CHARACTER*1 FILE$CURDRIVE
      INTEGER*4   FILE$MAXNAME
      INTEGER*4   $MAXPATH

      PARAMETER (FILE$CURDRIVE = ' ')
      PARAMETER (FILE$MAXNAME = 255)
      PARAMETER ($MAXPATH = 260)

! -----------------------------------------------------------------
! Files
! -----------------------------------------------------------------

      STRUCTURE / FILE$INFO /
        INTEGER*4       CREATION          ! Creation time (-1 on FAT)
        INTEGER*4       LASTWRITE         ! Last write to file
        INTEGER*4       LASTACCESS        ! Last access (-1 on FAT)
        INTEGER*4       LENGTH            ! Length of file
        INTEGER*2       PERMIT            ! File access mode
        CHARACTER*255   NAME              ! File name
      END STRUCTURE

      INTEGER*4 FILE$NORMAL
      INTEGER*4 FILE$READONLY
      INTEGER*4 FILE$HIDDEN
      INTEGER*4 FILE$SYSTEM
      INTEGER*4 FILE$VOLUME
      INTEGER*4 FILE$DIR
      INTEGER*4 FILE$ARCHIVE
      INTEGER*4 FILE$FIRST
      INTEGER*4 FILE$LAST
      INTEGER*4 FILE$ERROR
      INTEGER*4 FILE$INVALID
      INTEGER*4 FILE$CURTIME

      PARAMETER (FILE$NORMAL      = #0000)
      PARAMETER (FILE$READONLY    = #0001)
      PARAMETER (FILE$HIDDEN      = #0002)
      PARAMETER (FILE$SYSTEM      = #0004)
      PARAMETER (FILE$VOLUME      = #0008)
      PARAMETER (FILE$DIR         = #0010)
      PARAMETER (FILE$ARCHIVE     = #0020)

      PARAMETER (FILE$FIRST       = -1)
      PARAMETER (FILE$LAST        = -2)
      PARAMETER (FILE$ERROR       = -3)

      PARAMETER (FILE$INVALID     = -1) ! For PACKTIMEQQ and UNPACKTIMEQQ
      PARAMETER (FILE$CURTIME     = -1) ! For SETFILETIMEQQ

! -----------------------------------------------------------------
! Keyboard
! -----------------------------------------------------------------


! -----------------------------------------------------------------
! Errors
! -----------------------------------------------------------------

      INTEGER*4 ERR$ZERO
      INTEGER*4 ERR$PERM
      INTEGER*4 ERR$NOENT
      INTEGER*4 ERR$SRCH
      INTEGER*4 ERR$INTR
      INTEGER*4 ERR$IO
      INTEGER*4 ERR$NXIO
      INTEGER*4 ERR$2BIG
      INTEGER*4 ERR$NOEXEC
      INTEGER*4 ERR$BADF
      INTEGER*4 ERR$CHILD
      INTEGER*4 ERR$AGAIN
      INTEGER*4 ERR$NOMEM
      INTEGER*4 ERR$ACCES
      INTEGER*4 ERR$FAULT
      INTEGER*4 ERR$NOTBLK
      INTEGER*4 ERR$BUSY
      INTEGER*4 ERR$EXIST
      INTEGER*4 ERR$XDEV
      INTEGER*4 ERR$NODEV
      INTEGER*4 ERR$NOTDIR
      INTEGER*4 ERR$ISDIR
      INTEGER*4 ERR$INVAL
      INTEGER*4 ERR$NFILE
      INTEGER*4 ERR$MFILE
      INTEGER*4 ERR$NOTTY
      INTEGER*4 ERR$TXTBSY
      INTEGER*4 ERR$FBIG
      INTEGER*4 ERR$NOSPC
      INTEGER*4 ERR$SPIPE
      INTEGER*4 ERR$ROFS
      INTEGER*4 ERR$MLINK
      INTEGER*4 ERR$PIPE
      INTEGER*4 ERR$DOM
      INTEGER*4 ERR$RANGE
      INTEGER*4 ERR$UCLEAN
      INTEGER*4 ERR$DEADLOCK

      PARAMETER (ERR$ZERO         =  0)
      PARAMETER (ERR$PERM         =  1)
      PARAMETER (ERR$NOENT        =  2)
      PARAMETER (ERR$SRCH         =  3)
      PARAMETER (ERR$INTR         =  4)
      PARAMETER (ERR$IO           =  5)
      PARAMETER (ERR$NXIO         =  6)
      PARAMETER (ERR$2BIG         =  7)
      PARAMETER (ERR$NOEXEC       =  8)
      PARAMETER (ERR$BADF         =  9)
      PARAMETER (ERR$CHILD        = 10)
      PARAMETER (ERR$AGAIN        = 11)
      PARAMETER (ERR$NOMEM        = 12)
      PARAMETER (ERR$ACCES        = 13)
      PARAMETER (ERR$FAULT        = 14)
      PARAMETER (ERR$NOTBLK       = 15)
      PARAMETER (ERR$BUSY         = 16)
      PARAMETER (ERR$EXIST        = 17)
      PARAMETER (ERR$XDEV         = 18)
      PARAMETER (ERR$NODEV        = 19)
      PARAMETER (ERR$NOTDIR       = 20)
      PARAMETER (ERR$ISDIR        = 21)
      PARAMETER (ERR$INVAL        = 22)
      PARAMETER (ERR$NFILE        = 23)
      PARAMETER (ERR$MFILE        = 24)
      PARAMETER (ERR$NOTTY        = 25)
      PARAMETER (ERR$TXTBSY       = 26)
      PARAMETER (ERR$FBIG         = 27)
      PARAMETER (ERR$NOSPC        = 28)
      PARAMETER (ERR$SPIPE        = 29)
      PARAMETER (ERR$ROFS         = 30)
      PARAMETER (ERR$MLINK        = 31)
      PARAMETER (ERR$PIPE         = 32)
      PARAMETER (ERR$DOM          = 33)
      PARAMETER (ERR$RANGE        = 34)
      PARAMETER (ERR$UCLEAN       = 35)
      PARAMETER (ERR$DEADLOCK     = 36)

      LOGICAL*4 ERR$HARDPROMPT      ! For SETERRORMODEQQ
      LOGICAL*4 ERR$HARDFAIL

      PARAMETER (ERR$HARDPROMPT   = .TRUE.)
      PARAMETER (ERR$HARDFAIL     = .FALSE.)

! ----------------------------------------------------------------
! Sorting and Searching Arrays
! ----------------------------------------------------------------

      INTEGER*4 SRT$REAL4
      INTEGER*4 SRT$REAL8
      INTEGER*4 SRT$INTEGER1
      INTEGER*4 SRT$INTEGER2
      INTEGER*4 SRT$INTEGER4

      PARAMETER (SRT$REAL4          = #00010000)
      PARAMETER (SRT$REAL8          = #00020000)
      PARAMETER (SRT$INTEGER1       = #00030000)
      PARAMETER (SRT$INTEGER2       = #00040000)
      PARAMETER (SRT$INTEGER4       = #00050000)

! ----------------------------------------------------------------
! Configuration
! ----------------------------------------------------------------

      INTEGER*4 CFG$WINNT
      PARAMETER (CFG$WINNT          = #00000001)


! ----------------------------------------------------------------
! QuickWin Support
! ----------------------------------------------------------------
$PACK:1
! structure for setwindowconfig/getwindowconfig
      STRUCTURE /windowconfig/
        INTEGER*2 numxpixels
        INTEGER*2 numypixels
        INTEGER*2 numtextcols
        INTEGER*2 numtextrows
        INTEGER*2 numcolors
        INTEGER*4 fontsize
        CHARACTER*(80) title
        INTEGER*2 bitsperpixel
        INTEGER*2 numvideopages
        INTEGER*2 mode
        INTEGER*2 adapter
        INTEGER*2 monitor
        INTEGER*2 memory
        INTEGER*2 environment
        CHARACTER*(32) extendfontname
        INTEGER*4 extendfontsize
        INTEGER*4 extendfontattributes
      END STRUCTURE
$PACK

!     Parameters for APPENDMENUQQ, INSERTMENUQQ, MODIFYMENUFLAGSQQ
      INTEGER*4 $MENUGRAYED, $MENUDISABLED, $MENUENABLED
      INTEGER*4 $MENUSEPARATOR, $MENUCHECKED, $MENUUNCHECKED
      PARAMETER($MENUGRAYED=1)
      PARAMETER($MENUDISABLED=2)
      PARAMETER($MENUENABLED=4)
      PARAMETER($MENUSEPARATOR=8)
      PARAMETER($MENUCHECKED=16)
      PARAMETER($MENUUNCHECKED=32)

! Clicking on menus.
      INTEGER*4 QWIN$STATUS
      INTEGER*4 QWIN$TILE
      INTEGER*4 QWIN$CASCADE
      INTEGER*4 QWIN$ARRANGE

      PARAMETER (QWIN$STATUS  = 1)
      PARAMETER (QWIN$TILE    = 2)
      PARAMETER (QWIN$CASCADE = 3)
      PARAMETER (QWIN$ARRANGE = 4)

! Extended font information

      INTEGER*4 QWIN$EXTENDFONT
      INTEGER*4 QWIN$EXTENDFONT_NORMAL
      INTEGER*4 QWIN$EXTENDFONT_UNDERLINE
      INTEGER*4 QWIN$EXTENDFONT_BOLD
      INTEGER*4 QWIN$EXTENDFONT_ITALIC

      PARAMETER (QWIN$EXTENDFONT            =#FFFFFFFE)
      PARAMETER (QWIN$EXTENDFONT_NORMAL     =#80000000)
      PARAMETER (QWIN$EXTENDFONT_BOLD       =#00000001)
      PARAMETER (QWIN$EXTENDFONT_UNDERLINE  =#00000002)
      PARAMETER (QWIN$EXTENDFONT_ITALIC     =#00000004)

! Size/Move a window
      STRUCTURE /QWINFO/
          INTEGER*2 TYPE          ! request type
          INTEGER*2 X             ! x coordinate for upper left
          INTEGER*2 Y             ! y coordinate for upper left
          INTEGER*2 H             ! window height
          INTEGER*2 W             ! window width
      END STRUCTURE

      INTEGER*2 QWIN$MIN
      INTEGER*2 QWIN$MAX
      INTEGER*2 QWIN$RESTORE
      INTEGER*2 QWIN$SET

      PARAMETER (QWIN$MIN     = 1)
      PARAMETER (QWIN$MAX     = 2)
      PARAMETER (QWIN$RESTORE = 3)
      PARAMETER (QWIN$SET     = 4)

      INTEGER*4 QWIN$FRAMEWINDOW
      INTEGER*4 QWIN$NOACTIVEWINDOW
      PARAMETER (QWIN$FRAMEWINDOW = #80000000) 
      PARAMETER (QWIN$NOACTIVEWINDOW = #C0000000)

      INTEGER*4 QWIN$FRAMEMAX
      INTEGER*4 QWIN$FRAMECURR
      INTEGER*4 QWIN$CHILDMAX
      INTEGER*4 QWIN$CHILDCURR
      INTEGER*4 QWIN$SIZEMAX
      INTEGER*4 QWIN$SIZECURR

      PARAMETER (QWIN$FRAMEMAX  = 1)
      PARAMETER (QWIN$FRAMECURR = 2)
      PARAMETER (QWIN$CHILDMAX  = 3)
      PARAMETER (QWIN$CHILDCURR = 4)
      PARAMETER (QWIN$SIZEMAX   = 3)
      PARAMETER (QWIN$SIZECURR  = 4)

!Get/Set Exit
      INTEGER*4 QWIN$EXITPROMPT, QWIN$EXITNOPERSIST, QWIN$EXITPERSIST

      PARAMETER(QWIN$EXITPROMPT= 1)     ! prompt before exiting
      PARAMETER(QWIN$EXITNOPERSIST= 2)  ! exit immediately
      PARAMETER(QWIN$EXITPERSIST= 3)    ! do not exit appilcation

! Message Boxes
      INTEGER*4 MB$ABORTRETRYIGNORE
      INTEGER*4 MB$DEFBUTTON1
      INTEGER*4 MB$DEFBUTTON2
      INTEGER*4 MB$DEFBUTTON3
      INTEGER*4 MB$ICONASTERISK
      INTEGER*4 MB$ICONEXCLAMATION
      INTEGER*4 MB$ICONHAND
      INTEGER*4 MB$ICONINFORMATION
      INTEGER*4 MB$ICONQUESTION
      INTEGER*4 MB$ICONSTOP
      INTEGER*4 MB$OK
      INTEGER*4 MB$OKCANCEL
      INTEGER*4 MB$RETRYCANCEL
      INTEGER*4 MB$SYSTEMMODAL
      INTEGER*4 MB$TASKMODAL
      INTEGER*4 MB$YESNO
      INTEGER*4 MB$YESNOCANCEL

      PARAMETER (MB$ABORTRETRYIGNORE = #00000002)
      PARAMETER (MB$DEFBUTTON1       = #00000000)
      PARAMETER (MB$DEFBUTTON2       = #00000100)
      PARAMETER (MB$DEFBUTTON3       = #00000200)
      PARAMETER (MB$ICONASTERISK     = #00000040)
      PARAMETER (MB$ICONEXCLAMATION  = #00000030)
      PARAMETER (MB$ICONHAND         = #00000010)
      PARAMETER (MB$ICONINFORMATION  = #00000040)
      PARAMETER (MB$ICONQUESTION     = #00000020)
      PARAMETER (MB$ICONSTOP         = #00000010)
      PARAMETER (MB$OK               = #00000000)
      PARAMETER (MB$OKCANCEL         = #00000001)
      PARAMETER (MB$RETRYCANCEL      = #00000005)
      PARAMETER (MB$SYSTEMMODAL      = #00001000)
      PARAMETER (MB$TASKMODAL        = #00002000)
      PARAMETER (MB$YESNO            = #00000004)
      PARAMETER (MB$YESNOCANCEL      = #00000003)

      INTEGER*4 MB$IDOK
      INTEGER*4 MB$IDCANCEL
      INTEGER*4 MB$IDABORT
      INTEGER*4 MB$IDRETRY
      INTEGER*4 MB$IDIGNORE
      INTEGER*4 MB$IDYES
      INTEGER*4 MB$IDNO

      PARAMETER (MB$IDOK     = 1)
      PARAMETER (MB$IDCANCEL = 2)
      PARAMETER (MB$IDABORT  = 3)
      PARAMETER (MB$IDRETRY  = 4)
      PARAMETER (MB$IDIGNORE = 5)
      PARAMETER (MB$IDYES    = 6)
      PARAMETER (MB$IDNO     = 7)

! InitialMenu should not be typed here, so that when InitialMenu is
! defined by the user it can include FLIB.FD.  Nor should it be external.
!     LOGICAL*4 INITIALMENU[EXTERN]


$endif  ! $if .not. defined(__MSFLIB_FGRAPH_INCLUDE)

END MODULE msflib
