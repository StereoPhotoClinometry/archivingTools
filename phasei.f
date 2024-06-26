c  Version 1.0 - 26 Sep 2019- Eric Palmer 
c	This outputs a set of binary files of i, e, and phase
c  gfortran phasei.f /usr/local/lib/spicelib.a /Users/JW/Dropbox/SPC-ORex/v3.0.4/COMMON.a -O2 -o ~/bin/t.phasei
c  Version 1.1 - 25 Nov 2020 - Eric E. Palmer
c		Calculates lat/lon and outputs them also
c  Version 1.2 -- 14 May 2021
C     Checks for bad data.  If Z2 is greater than 1, set to 1 and show error
C  Version 1.3 - 23 Sep 2021 - Eric E. Palmer
C           Added a check for NaN for COS for incidence, emission and phase chanel
C  Version 1.4 - 18 Oct 2021 - Eric E. Palmer
C		Add X/Y/Z Cartisian coordinats to the output
C  Version 1.5 - 18 Nov 2021 - John R. Weirich
C		Corrected order of I,J in calculations for lat/emission/etc. also changed order of I,J in the loop.
C  Version 1.6 - 23 Feb 2022 - John R. Weirich
C		Changed East Latitude to go from 0 to 360 instead of -180 180.
C  Version 1.7 - 20 Apr 2022 - John R. Weririch
C		Modified to allow for larger bigmaps. Q of 2349.
C		Tried NTMP of 4750 and got a compiling error.
C  Version 1.8 - 09 June 2022
C		Now writes out radius, also updated radius to have double precision
C  Version 1.9 - 15 June 2022 - Palmer
C     Removed radius for clarity.  Changed version to a string
C  Version 1.10 - 29 Aug 2023 - Palmer
C		Set up dynamic memory to deal with dynamicLib issues for newer mac hardware

      IMPLICIT NONE

      INTEGER               NTMP
      PARAMETER            (NTMP=4001)

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      U(3)
      DOUBLE PRECISION      W(3)
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      REAL*8                GAMMA, ETA, Z1, Z2, ILLUM, ALPHA, RPD
      REAL*8                slope(3)

C      REAL*4                TMPL(-NTMP:NTMP,-NTMP:NTMP,3)
      LOGICAL               HUSE(-NTMP:NTMP,-NTMP:NTMP)
      LOGICAL               TUSE(-NTMP:NTMP,-NTMP:NTMP)





C      REAL*4                HT0(-NTMP:NTMP,-NTMP:NTMP)
c      REAL*4                AL0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                mVal, mX, mY
      REAL*4                minVal, minX, minY

      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               zeros
      INTEGER               NPX, NLN, T1, T2
c      real               version
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

      DOUBLE PRECISION      CP(3)
      DOUBLE PRECISION      SP(3)
      DOUBLE PRECISION      localV(3)
      REAL                  Z0
      REAL                  ang
      REAL                  lat, lon
      DOUBLE PRECISION      dist
      DOUBLE PRECISION      hold

      CHARACTER*6           MAP0
      CHARACTER*72          LMRKFILE
      CHARACTER*72          PICT
      CHARACTER*72          PICTFILE
    
C Set up Dynamic memory
      integer :: inmax, error, stat
      real*4, allocatable :: AL0(:,:)
      real*4, allocatable :: HT0(:,:)
      real*4, allocatable :: TMPL(:,:, :)

      allocate (AL0 (-NTMP:NTMP,-NTMP:NTMP))
      allocate (HT0 (-NTMP:NTMP,-NTMP:NTMP))
      allocate (TMPL(-NTMP:NTMP,-NTMP:NTMP, 3))


C Version of the code
      version = "1.10"
      WRITE(*,*) 'Version:', version

C What map name do you want
      WRITE(6,*) 'Input map name (only 6 char no MAPFILES and .MAP)'
      READ(5,FMT='(A6)') MAP0
      write (*,*) "Map:  ", MAP0

      LMRKFILE='MAPFILES/'//MAP0//'.MAP'
      CALL READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT0,AL0)
      write (*,*) "BIGMAP position"
      write (*,*) "    V", V
      write (*,*) "    Ux", UX
      write (*,*) "    Uy", UY
      write (*,*) "    Uz", UZ
      write (*,*) "    QSZ", QSZ

C What image are we working with
      WRITE(6,*) 'Input image name'
      READ(5,FMT='(A12)') PICT


C Read metadata from sumfile for all of the images
      PICTFILE='SUMFILES/'//PICT(1:12)//'.SUM'
      write (*,*) PICTFILE
      OPEN(UNIT=20,FILE=PICTFILE,STATUS='OLD')

        READ(20,*)
        READ(20,*)
        READ(20,*)  NPX, NLN, T1, T2
        READ(20,*)  
        READ(20,*) (V0(I), I=1,3)
        write (*,*) "   V0", V0, "SCOBJ"
        READ(20,*) 
        READ(20,*)
        READ(20,*)
        READ(20,*) (SZ(I), I=1,3)
        write (*,*) "   SZ", SZ, "Sun Unit Vector"
      CLOSE(UNIT=20)

C     W is spacecraft vector to maplet center
      CALL VADD(V,V0,W)
      write (*,*) "Spacecraft position relative to bigmap"
      write (*,*) "    W", W
C     Converts to unit vector
      CALL VHAT(W,W)
      write (*,*) "    W`", W

C     Unit vector from maplet cetner to sc in maplet coord
      CP(1)=-VDOT(W,UX)
      CP(2)=-VDOT(W,UY)
      CP(3)=-VDOT(W,UZ)
C     Unit vector from maplet cetner to sun in maplet coord
      SP(1)= VDOT(SZ,UX)
      SP(2)= VDOT(SZ,UY)
      SP(3)= VDOT(SZ,UZ)

      write (*,*) "    CP", CP
      write (*,*) "    SP", SP


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
      write (*,*) "     TMPL", slope


      GAMMA=SQRT(1+slope(1)**2+slope(2)**2)
      write (*,*) "    gamma", gamma

C     Compute the incidence angle (center pixel)
      Z1=(SP(3)+slope(1)*SP(1)+slope(2)*SP(2))/GAMMA
      write (*,*) "Z1 - cos i", Z1
      write (*,*) "Deg", ACOS (Z1)/RPD()

C     Compute the emission angle (center pixel)
      Z2=(CP(3)+slope(1)*CP(1)+slope(2)*CP(2))/GAMMA
      write (*,*) "Z2 - cos e", Z2
      write (*,*) "Deg", ACOS (Z2)/RPD()
      ALPHA=ACOS(VDOT(CP,SP))/RPD()
      write (*,*) "Alpha - phase", ALPHA


C     Open the files that we will create
      LMRKFILE=MAP0//'-i.TXT'
      OPEN(UNIT=10,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-e.TXT'
      OPEN(UNIT=11,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-a.TXT'
      OPEN(UNIT=12,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-h.TXT'
      OPEN(UNIT=13,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-v.TXT'
      OPEN(UNIT=14,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-lat.TXT'
      OPEN(UNIT=15,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-lon.TXT'
      OPEN(UNIT=16,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-x.TXT'
      OPEN(UNIT=17,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-y.TXT'
      OPEN(UNIT=18,FILE=LMRKFILE)
      LMRKFILE=MAP0//'-z.TXT'
      OPEN(UNIT=19,FILE=LMRKFILE)

C     Loop over the entire array
C       To match readmap, the fastest change in the 2nd index of the array
      DO I=-QSZ,QSZ                                                     col, X
      DO J=-QSZ,QSZ                                                     row, Y

c          tmpl (J,I,1) = 0
c          tmpl (J,I,2) = 0
C         Recaculate the vector to each pixel
          Z0=HT0(J,I)
          localV(1)=V(1)+SCALE*(J*UY(1)+I*UX(1)+Z0*UZ(1))
          localV(2)=V(2)+SCALE*(J*UY(2)+I*UX(2)+Z0*UZ(2))
          localV(3)=V(3)+SCALE*(J*UY(3)+I*UX(3)+Z0*UZ(3))

C         Converts into spacecraft frame
          CALL VADD(localV,V0,W)
          CALL VHAT(W,W)


          CP(1)=-VDOT(W,UX)
          CP(2)=-VDOT(W,UY)
          CP(3)=-VDOT(W,UZ)
          SP(1)= VDOT(SZ,UX)
          SP(2)= VDOT(SZ,UY)
          SP(3)= VDOT(SZ,UZ)
          GAMMA=SQRT(1+TMPL(J,I,1)**2+TMPL(J,I,2)**2)

C         Look for bad data
          if (GAMMA .EQ. 0) then
             write (*,*) "Gamma null", j, i, TMPL(J,I,1), TMPL(J,I,2)
             exit
          ENDIF
  

C         Calculate the angles
C             Run the fastes array element for the 2nd index
C         Incidence
          Z1=(SP(3) + TMPL(J,I,1)*SP(1) + TMPL(J,I,2)*SP(2) )/GAMMA
          if (Z1 .GT. 1) then
             write (*,*) "Z1 is greater than 1",J,I, Z1, CP, SP, gamma
             Z1 = 1
          endif
          ang = ACOS (Z1) / RPD()
          write(10,240, advance="no") ang

C         Emission
          Z2=(CP(3) + TMPL(J,I,1)*CP(1) + TMPL(J,I,2)*CP(2) )/GAMMA
          if (Z2 .gt. 1) then
             write (*,*) "Z2 is greater than 1", Z2, ANG, "J, I", J, I
             Z2 = 1 
          ENDIF
          ang = ACOS (Z2) / RPD()
          write(11,240, advance="no") ang

C         Phase
          hold = VDOT(CP,SP)
          if (hold .GT. 1) then
             write (*,*) J,I, hold, CP, SP
             hold = 1
          endif
          ALPHA=ACOS(hold)/RPD()
          write(12,240, advance="no") ALPHA

C         Slope
          write(13,240, advance="no") (TMPL(J,I,1))
          write(14,240, advance="no") (TMPL(J,I,2))

C			Lat and lon
          dist = sqrt (localV(1)**2 + localV(2)**2 + localV(3)**2)
          lon =atan2 ( localV(2) , localV(1)) * 180 / 3.1415926
          lat = asin ( localV(3)/dist) * 180/3.1415926
          if (lon .LT. 0)  then
             lon = lon + 360
          endif
          write(15,240, advance="no") lat 
          write(16,240, advance="no") lon 

C         Write out X/Y/Z Cartesian coordintes
          write(17,240, advance="no") localV(1) 
          write(18,240, advance="no") localV(2) 
          write(19,240, advance="no") localV(3) 

        ENDDO

        write (10, *)
        write (11, *)
        write (12, *)
        write (13, *)
        write (14, *)
        write (15, *)
        write (16, *)
        write (17, *)
        write (18, *)
        write (19, *)
      ENDDO

 240  format (f14.5)

      close (10)
      close (11)
      close (12)
      close (13)
      close (14)
      close (15)
      close (16)
      close (17)
      close (18)
      close (19)

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
      
