c  Version 1.0 - 26 Sep 2019- Eric Palmer 
c	This outputs a set of binary files of i, e, and phase
c  gfortran backplanesISIS.f /usr/local/lib/spicelib.a /usr/local/src/v3.0.4/COMMON.a -O2 -o ~/bin/t.backplanesISIS
c  Version 1.1 - 25 Nov 2020 - Eric E. Palmer
c		Calculates lat/lon and outputs them also
c	Version 1.2 - 8 June 2021 - Eric E. Palmer
c		Updated to remove rotation that exists sometimes (not sure why)
c		Lon output is in East Longitude
c	Version 1.3 - 11 June 2021 - Eric E. Palmer
c		Output is in list format
c		Output file with Lon, Lat Plus channels for H, albedo, slope_pix, slope_norm
C	Version 1.4 - 14 June 2021
C		Outputs in matrix format
C	Version 1.5 - 14 June 2022 
C		Output user defined elevation relative to datum instead of radius
C		elevation is double precision
C		version is now a string
C		NTMP increased from 2001 to 3001
C  Version 1.6 - 7 Nov 2024
C     While the radius is rotated correctly, the slope and albedo
C        had a trasposition.  HT0 indicates a J, then I, which might
C        not be what we really want.  However, for expedience, I just
C        transposed all of the array access to be J then I to fix the output

      IMPLICIT NONE

      INTEGER               NTMP
      PARAMETER            (NTMP=3001)

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      U(3)
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      REAL*8                GAMMA, ETA, Z1, Z2, ILLUM, ALPHA, RPD
      REAL                  slope(3)

      REAL*4                TMPL(-NTMP:NTMP,-NTMP:NTMP,3)
      LOGICAL               HUSE(-NTMP:NTMP,-NTMP:NTMP)
      LOGICAL               TUSE(-NTMP:NTMP,-NTMP:NTMP)





      REAL*4                HT0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                mVal, mX, mY
      REAL*4                minVal, minX, minY

      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               zeros
      INTEGER               NPX, NLN, T1, T2
C      real               version
      CHARACTER*80          version
    
      DOUBLE PRECISION      V0(3)
      DOUBLE PRECISION      SZ(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      CX(3)
      DOUBLE PRECISION      CY(3)
      DOUBLE PRECISION      CZ(3)
      DOUBLE PRECISION      WX(3)
      DOUBLE PRECISION      WY(3)
      DOUBLE PRECISION      WZ(3)
      DOUBLE PRECISION      MMFL
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      CTR(2)

      DOUBLE PRECISION      localV(3)
      REAL                  Z0
      REAL                  ang
      REAL                  lat, lon
      DOUBLE PRECISION      dist, datum



      CHARACTER*6           MAP0
      CHARACTER*72          LMRKFILE
      CHARACTER*72          PICT
      CHARACTER*72          PICTFILE
    
      version = "1.6"


      WRITE(*,*) 'Version:', version

      WRITE(6,*) 'Input map name (only 6 char no MAPFILES and .MAP)'
      READ(5,FMT='(A6)') MAP0
      WRITE(6,*) 'Input datum (km) to use (read as double precision)'
      WRITE(6,*) 'Standard for Luna is 1737.4'
      READ(5,*) datum
      WRITE(6,*) 'Datum:', datum


      LMRKFILE='MAPFILES/'//MAP0//'.MAP'
      CALL READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT0,AL0)
      write (*,*) "BIGMAP position"
      write (*,*) "    V", V
      write (*,*) "    Ux", UX
      write (*,*) "    Uy", UY
      write (*,*) "    Uz", UZ
      write (*,*) "    QSZ", QSZ

      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        HUSE(I,J)=.TRUE.
      ENDDO
      ENDDO

      CALL HGT2SLP(NTMP,QSZ,HUSE,HT0, TUSE,TMPL)

C     Do the basics for the center pixel
      slope (1) = 0
      slope (2) = 0
      slope (3) = 0

C     Open the files that we will create
      LMRKFILE=MAP0//'-r.txt'
      OPEN(UNIT=10,FILE=LMRKFILE)
      write (*,*) LMRKFILE, " change to datum and scale x1000, km to m"
      write (*,*) "Be sure your input BIGMAP is in km"

      LMRKFILE=MAP0//'-alb.txt'
      OPEN(UNIT=11,FILE=LMRKFILE)

      LMRKFILE=MAP0//'-slopeN.txt'
      OPEN(UNIT=12,FILE=LMRKFILE)

      LMRKFILE=MAP0//'-h.txt'
      OPEN(UNIT=13,FILE=LMRKFILE)

      LMRKFILE=MAP0//'-v.txt'
      OPEN(UNIT=14,FILE=LMRKFILE)

      LMRKFILE=MAP0//'-lon.txt'
      OPEN(UNIT=15,FILE=LMRKFILE)

      LMRKFILE=MAP0//'-lat.txt'
      OPEN(UNIT=16,FILE=LMRKFILE)


C     Loop over the entire array
      DO i=-QSZ,QSZ                                                     col, X
      DO j=-QSZ,QSZ                                                     row, Y

C         Recaculate the vector to each pixel
          Z0=HT0(J,I)
          localV(1)=V(1)+SCALE*(J*UY(1)+I*UX(1)+Z0*UZ(1))
          localV(2)=V(2)+SCALE*(J*UY(2)+I*UX(2)+Z0*UZ(2))
          localV(3)=V(3)+SCALE*(J*UY(3)+I*UX(3)+Z0*UZ(3))

C          Not used
C          GAMMA=SQRT(1+TMPL(I,J,1)**2+TMPL(I,J,2)**2)

C         Calculate the angles
C             Run the fastes array element for the 1st index
C         Albedo
C             Theep - I'm not sure John is correct itwh this, but let's see
          write(11,240, advance="no") (AL0(J,I))

C         SlopeNormal
          write(12,240, advance="no") (TMPL(J,I,1))

C         Slope
          write(13,240, advance="no") (TMPL(J,I,1))
          write(14,240, advance="no") (TMPL(J,I,2))

C			Lat and lon
          dist = sqrt (localV(1)**2 + localV(2)**2 + localV(3)**2)
          lon =atan2 ( localV(2) , localV(1)) * 180 / 3.1415926
          if (lon .lt. 0) then
              lon = lon + 360
          endif
          lat = asin ( localV(3)/dist) * 180/3.1415926
          write(10,240, advance="no") (dist-datum)*1000
          write(15,240, advance="no") lon
          write(16,240, advance="no") lat

        ENDDO

        write (10,*) 
        write (11,*) 
        write (12,*) 
        write (13,*) 
        write (14,*) 
        write (15,*) 
        write (16,*) 
      ENDDO

 240  format (f14.5)


      STOP
      END

c   ................................................
      SUBROUTINE READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT,ALB)
c   ................................................

      IMPLICIT NONE
      
      INTEGER               NTMP
      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               K0
      INTEGER               IX(24)
      INTEGER               JX(24)
      INTEGER               NREC

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      HSCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)

      CHARACTER*72          BLINE
      CHARACTER*72          LMRKFILE

      CHARACTER*1           CH1
      CHARACTER*2           CH2, CH2F
      CHARACTER*4           CH4, CH4F
      INTEGER*2             IX2
      REAL*4                RL4
      EQUIVALENCE          (IX2,CH2)
      EQUIVALENCE          (RL4,CH4)

      CHARACTER*2           C2
      INTEGER*2             I2
      EQUIVALENCE          (I2,C2)
      LOGICAL               LFLAG

      c2='69'
      LFLAG=.TRUE.
      if(i2.eq.13881) LFLAG=.FALSE.

      OPEN(UNIT=20, FILE=LMRKFILE, ACCESS='DIRECT',
     .     RECL=72, status='OLD')

c       Read is the scale fo the bigmap
        READ(20,REC=1) BLINE
        CH4f=BLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        SCALE=RL4

c       Read is the size of the bigmap
        QSZ=ICHAR(BLINE(11:11))
     .     +ICHAR(BLINE(12:12))*256

c       Loop over K four times (for values for V, and U[XYZ])
c       Load up V, UX, UY and UZ
        DO K=1,3
          CH4f=BLINE(12+4*K:15+4*K)
          call flip(4,lflag,ch4f,ch4)
          V(K)=RL4
          CH4f=BLINE(24+4*K:27+4*K)
          call flip(4,lflag,ch4f,ch4)
          UX(K)=RL4
          CH4f=BLINE(36+4*K:39+4*K)
          call flip(4,lflag,ch4f,ch4)
          UY(K)=RL4
          CH4f=BLINE(48+4*K:51+4*K)
          call flip(4,lflag,ch4f,ch4)
          UZ(K)=RL4
        ENDDO


c       Load the H Scale
        CH4f=BLINE(64:67)
        call flip(4,lflag,ch4f,ch4)
        HSCALE=RL4

c       Clear the memory
        DO I=-QSZ,QSZ
          DO J=-QSZ,QSZ
            ALB(I,J)=0
            HT(I,J)=0
          ENDDO
        ENDDO
        
c       Read stuff in, 24 bytes at a time
        NREC=1        
        K=0
        DO J=-QSZ,QSZ
          DO I=-QSZ,QSZ
            K=K+1
            IX(K)=I
            JX(K)=J
            IF(K.EQ.24) THEN
              NREC=NREC+1
              READ(20,REC=NREC) BLINE
              DO K=1,24
                CH1=BLINE(3*K:3*K)
                IF(ICHAR(CH1).NE.0) THEN
                  CH2f=BLINE(3*K-2:3*K-1)
                  call flip(2,lflag,ch2f,ch2)
                  HT(IX(K),JX(K))=HSCALE*IX2
                  ALB(IX(K),JX(K))=.01*ICHAR(CH1)
                ENDIF
              ENDDO
              K=0
            ENDIF
          enddo
        enddo

c       Read the remainder of a 24 byte line
        IF(K.NE.0) THEN
          K0=K
          NREC=NREC+1
          READ(20,REC=NREC) BLINE
          DO K=1,K0
            CH1=BLINE(3*K:3*K)
            CH2f=BLINE(3*K-2:3*K-1)
            call flip(2,lflag,ch2f,ch2)
            HT(IX(K),JX(K))=HSCALE*IX2
            ALB(IX(K),JX(K))=.01*ICHAR(CH1)
          ENDDO
        ENDIF

      CLOSE(UNIT=20)
      
      RETURN
      END     

c   ................................................
      SUBROUTINE WRITE_MAP(LMRKFILE,NTMP,QSZ,SCALE,
     .                     V,UX,UY,UZ,HT,ALB)
c   ................................................

      IMPLICIT NONE
      
      INTEGER               NTMP
      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NREC

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      HSCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      VSIG(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      Z1

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)

      CHARACTER*72          BLINE
      CHARACTER*72          LMRKFILE

      CHARACTER*1           CH1
      CHARACTER*2           CH2, CH2F
      CHARACTER*4           CH4, CH4F
      INTEGER*2             IX2
      REAL*4                RL4
      EQUIVALENCE          (IX2,CH2)
      EQUIVALENCE          (RL4,CH4)

      CHARACTER*2           C2
      INTEGER*2             I2
      EQUIVALENCE          (I2,C2)
      LOGICAL               LFLAG

      c2='69'
      LFLAG=.TRUE.
      if(i2.eq.13881) LFLAG=.FALSE.

      OPEN(UNIT=10,FILE=LMRKFILE,ACCESS='DIRECT',
     .     RECL=72,STATUS='UNKNOWN')
        DO K=1,72
          BLINE(K:K)=CHAR(0)
        ENDDO
        BLINE(1:6)='UNUSED'
        RL4=SCALE
        call flip(4,lflag,ch4,ch4f)
        BLINE(7:10)=CH4f
        BLINE(11:11)=CHAR(QSZ-256*(qsz/256))  !!!*
        BLINE(12:12)=CHAR(qsz/256)
        RL4=50
        call flip(4,lflag,ch4,ch4f)
        BLINE(68:71)=CH4f
        BLINE(13:13)=CHAR(50)
        BLINE(14:14)=CHAR(50)
        BLINE(15:15)=CHAR(50)
        DO K=1,3
          RL4=V(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(12+4*K:15+4*K)=CH4f
          RL4=UX(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(24+4*K:27+4*K)=CH4f
          RL4=UY(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(36+4*K:39+4*K)=CH4f
          RL4=UZ(K)
          call flip(4,lflag,ch4,ch4f)
          BLINE(48+4*K:51+4*K)=CH4f
        ENDDO
        z1=1.0
        do j=-QSZ,QSZ
        do i=-QSZ,QSZ
        if(alb(i,j).gt.0.005) then
          z1=max(z1,ABS(ht(i,j)))
        endif
        enddo
        enddo
        HSCALE=z1/30000
        RL4=HSCALE
        call flip(4,lflag,ch4,ch4f)
        BLINE(64:67)=CH4f
        NREC=1
        WRITE(10,REC=NREC) BLINE
        K=0
        do j=-QSZ,QSZ
        do i=-QSZ,QSZ
          K=K+1
          IX2=NINT(HT(I,J)/HSCALE)
          CH1=CHAR(NINT(100*ALB(I,J)))
          call flip(2,lflag,ch2,ch2f)
          BLINE(3*K-2:3*K-1)=CH2f
          BLINE(3*K:3*K)=CH1
          IF(K.EQ.24) THEN
            NREC=NREC+1
            write(10, REC=NREC) BLINE
            DO K=1,72
              BLINE(K:K)=CHAR(0)
            ENDDO
            K=0
          ENDIF
        enddo
        enddo
        IF(K.NE.0) THEN
          NREC=NREC+1
          write(10, REC=NREC) BLINE
        ENDIF
      CLOSE(UNIT=10)

      RETURN
      END     

c   ..................................................
      subroutine flip(n,lflag,ch1,ch2)
c   ..................................................

      integer*4        n, i
      character*(*)    ch1, ch2
      logical          lflag

      if(lflag) then
        do i=1,n
          ch2(i:i)=ch1(n-i+1:n-i+1)
        enddo
      else
        ch2=ch1
      endif
      
      return
      end
      
