C Eric E. Palmer - 26 May 2026
C     This is pulling out BRT0 from XFiles to create a stand alone version 
C     to support map_coverage_p_low
C
C$ Abstract
C
C   This function uses the image array extracted by the subroutine PICINPT 
C   to determine the brightness value at pixel/line location [Z(1),Z(2)] in 
C   the original image.  It determines the four surrounding pixels in the image 
C   array dn and finds the value BRT1 at the pont of interest through bilinear
C   interpolation.



c  ......................................................
      function xBrt(imgpl, npx,nln,dn, maxPix, maxLine)
c  ......................................................
      
      IMPLICIT NONE

      real*8        x, y, xBrt
      integer*4     i, j, b0, b1, b2, b3, npx, nln
      integer       dn(maxPix, maxLine)
      integer       maxPix, maxLine
      DOUBLE PRECISION      IMGPL(2)


      xBrt=0
      if((imgpl(1).ge.1).and.(imgpl(1).lt.npx).and.
     .   (imgpl(2).ge.1).and.(imgpl(2).lt.nln)) then
        i=int(imgpl(1))
        j=int(imgpl(2))
        x=imgpl(1)-i
        y=imgpl(2)-j
c        b0=dn(i+npx*(j-1))
c        b1=dn(i+1+npx*(j-1))
c        b2=dn(i+npx*j)
c        b3=dn(i+1+npx*j)
        b0=dn(i,     j-1)
        b1=dn(i+1,   j-1)
        b2=dn(i,     j)
        b3=dn(i+1,   j)


        if((b0.eq.0).and.(b1*b2*b3.ne.0)) b0=max(1,b1+b2-b3)
        if((b1.eq.0).and.(b2*b3*b0.ne.0)) b1=max(1,b0+b3-b2)
        if((b2.eq.0).and.(b3*b0*b1.ne.0)) b2=max(1,b3+b0-b1)
        if((b3.eq.0).and.(b0*b1*b2.ne.0)) b3=max(1,b2+b1-b0)
        if(b0*b1*b2*b3.eq.0) return
C      write (*,*) imgpl, b0, b1, b2, b3

        b3=b0-b1-b2+b3
        b1=b1-b0
        b2=b2-b0
        xBrt=b0+b1*x+b2*y+b3*x*y
        xBrt=max(0.d0,xBrt)
        return
      endif

      return
      end

