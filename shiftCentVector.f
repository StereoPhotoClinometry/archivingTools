c  Version 1.0 - 18 March 2024- Bob Gaskell
c  This will shift the center vector in cartisian space

      IMPLICIT NONE

      INTEGER               NTMP
      PARAMETER            (NTMP=5000)

      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)

      REAL*4                HT0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL0(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                HT1(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                AL1(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                dX, dY, dZ
      REAL                  version 

      INTEGER               QSZ
      INTEGER               I
      INTEGER               J
      INTEGER               K

      CHARACTER*6           MAP0
      CHARACTER*6           MAP1
      CHARACTER*72          LMRKFILE
  
      version = 1.0

      write (*,*) "Version: ", version
      WRITE(6,*) 'Input input map name'
      READ(5,FMT='(A6)') MAP0
      WRITE(6,*) 'Input output map name [6 char]'
      READ(5,FMT='(A6)') MAP1

      if (MAP0 .EQ. MAP1) then
        write (*,*) "Error:  Cannot be the same name"
        stop
      endif

      if (MAP1(6:6) .EQ. " ") then
        write (*,*) "Error:  MAPLET name must be 6 characters"
        stop
      endif


      LMRKFILE='MAPFILES/'//MAP0//'.MAP'
      CALL READ_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT0,AL0)

      WRITE(6,*) 'Input delta X, Y and Z for central vector [km]'
      READ(5,*) dX, dY, dZ

      write (*,*) "Shifting: ", dX, dY, dZ
      write (*,*) "Old: ", V

      V(1) = V(1) + dX
      V(2) = V(2) + dY
      V(3) = V(3) + dZ

      write (*,*) "New: ", V


      LMRKFILE='MAPFILES/'//MAP1//'.MAP'
      CALL WRITE_MAP(LMRKFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT0,AL0)

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

        READ(20,REC=1) BLINE
        CH4f=BLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        SCALE=RL4
        QSZ=ICHAR(BLINE(11:11))
     .     +ICHAR(BLINE(12:12))*256
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
        CH4f=BLINE(64:67)
        call flip(4,lflag,ch4f,ch4)
        HSCALE=RL4

        DO I=-QSZ,QSZ
        DO J=-QSZ,QSZ
          ALB(I,J)=0
          HT(I,J)=0
        ENDDO
        ENDDO
        
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
      
