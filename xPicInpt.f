C  PROCEDURE MOSAICX
C     Copied from Xfiles and modified to be a stand alone code for 
C     map_coverage_p_low
C     Eric E. Palmer - 26 May 2026


c  ...............................................................
      subroutine xPicInpt(PICNM,dn, maxPix, maxLine)
c  ...............................................................
      
      IMPLICIT NONE

      DOUBLE PRECISION      Z1, Z2

      CHARACTER*3           ORDER
      CHARACTER*12          PICNM
      CHARACTER*72          PICTFILE
      CHARACTER*99999       INLINE
      
      INTEGER               NPX
      INTEGER               NLN, maxPix, maxLine
      INTEGER               I
      INTEGER               J
      INTEGER               I1
      INTEGER               J1
      INTEGER               I2
      INTEGER               J2
      INTEGER               SLEN
      INTEGER               BYTES
      INTEGER               d1(-1:1,-1:1)
      INTEGER               d2(-1:1,-1:1)

      INTEGER               dn(maxPix, maxLine)

      LOGICAL               EX

C---------------------------------------------------------------
C     OPen the image file
      I=SLEN(PICNM)      
      PICTFILE='SUMFILES/'//PICNM(1:I)//'.SUM'
      OPEN(UNIT=10,FILE=PICTFILE,STATUS='OLD')
        READ(10,*)
        READ(10,*)
        READ(10,*) NPX, NLN
      CLOSE(UNIT=10)
      CALL FIND_PICFILE(PICNM,PICTFILE,EX)
      IF(.NOT.EX) THEN
        DO J=1,NLN
        DO I=1,NPX
          dn(I,J)=0
c          DN(I+NPX*(J-1))=0
        ENDDO
        ENDDO
        RETURN
      ENDIF

C---------------------------------------------------------------
      bytes=2
      open(unit=10, file=pictfile, access='direct',
     .     recl=2*npx, status='old')
        read(10,rec=nln, err=20) inline(1:2*npx)
        go to 21
20      bytes=1
21      continue
      close(unit=10)

C---------------------------------------------------------------
C Figure out LSB/MSB for 2 byte integers

      if(bytes.eq.2) then
        open(unit=10, file=pictfile, access='direct',
     .       recl=2*npx, status='old')
          z1=0
          z2=0
          do i1=1,99,2
          do j1=1,99,2
            i=(5+i1*npx)/100
            j=(5+j1*nln)/100
            do i2=-1,1
            do j2=-1,1
              read(10,rec=j+j2) inline(1:2*npx)
              d1(i2,j2)=ichar(inline(2*(i+i2)-1:2*(i+i2)-1)) 
              d2(i2,j2)=ichar(inline(2*(i+i2):2*(i+i2))) 
            enddo
            enddo
            z1=z1+(4*d1(0,0)-d1(-1,0)-d1(1,0)-d1(0,-1)-d1(0,1))**2
            z2=z2+(4*d2(0,0)-d2(-1,0)-d2(1,0)-d2(0,-1)-d2(0,1))**2
          enddo
          enddo 
        close(unit=10)
        if(z1.gt.z2) order='LSB'
        if(z2.gt.z1) order='MSB'
      endif

C---------------------------------------------------------------
C Read in the file
      if(bytes.eq.1) then
        open(unit=10, file=PICTFILE, access='direct', 
     .       recl=NPX, status='old')
          do j=1,NLN
            read(10,rec=j) inline(1:npx)
            do i=1,NPX
              DN(I,J)=ichar(inline(I:I))
C              DN(I+NPX*(J-1))=ichar(inline(I:I))
            enddo
          enddo
        close (unit=10)
      endif
      if(bytes.eq.2) then
        open(unit=10, file=PICTFILE, access='direct', 
     .       recl=2*NPX, status='old')
          do j=1,NLN
            read(10,rec=j) inline(1:2*npx)
            do i=1,NPX
C              if(order.eq.'MSB') DN(I+NPX*(J-1))=
C     .          ichar(inline(2*I:2*I))+256*ichar(inline(2*I-1:2*I-1))
C              if(order.eq.'LSB') DN(I+NPX*(J-1))=
C     .          256*ichar(inline(2*I:2*I))+ichar(inline(2*I-1:2*I-1))

              if(order.eq.'MSB') DN(I,J)=
     .          ichar(inline(2*I:2*I))+256*ichar(inline(2*I-1:2*I-1))

              if(order.eq.'LSB') DN(I,J)=
     .          256*ichar(inline(2*I:2*I))+ichar(inline(2*I-1:2*I-1))

C      write(*,'(I6)', advance='no') DN(I,J)
            enddo
C      write (*,*) 
          enddo
        close(unit=10)
      endif

      return
      end

