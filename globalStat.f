c     gfortran -O2  globalStat.f /usr/local/lib/spicelib.a /usr/local/lib/COMMON.a -o ~/bin/globalStat
c	May/Jun 2021 - Eric E. Palmer
C		Adjusted to only add 1 (vs 15) for each image
C	Version 2.0  Jun 18 2021
C		Changed output name
C  Version 1.0 June 21 1021
C     Change program name to globalStat
C     Added image resolution
C     Output files are global-XXXX.ll
C  Version 2.1 Nov 30 2021
C     Fixed output and array index
C	Version 2.2 - Aug 16th 2022
C		Changed the output to go from 1-360, rather than 361
C		The pgm is still 361
C  Version 2.3 - Sep 12, 2022
C     Made output in both grid and vector
C     Complies with grid.txt or c1.txt, c2.txt etc
C     Removed the generation of the pgm


      IMPLICIT NONE
      
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      V0(3)
      DOUBLE PRECISION      V(3,361,181)
      DOUBLE PRECISION      W(3)
      DOUBLE PRECISION      CX(3)
      DOUBLE PRECISION      CY(3)
      DOUBLE PRECISION      CZ(3)
      DOUBLE PRECISION      SZ(3)
      DOUBLE PRECISION      UZ(3,361,181)
      DOUBLE PRECISION      CTR(2)
      DOUBLE PRECISION      MMFL
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      IMGPL(2)
      DOUBLE PRECISION      RESLIM
      DOUBLE PRECISION      Z1, Z2, Z3, Z4, Z5, Z6, Z7
      REAL    bestRes(361,181)

      INTEGER               coverage(361,181)
      INTEGER               NPX, NLN
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               SLEN

      CHARACTER*1           ANS        
      CHARACTER*12          PICNM         
      CHARACTER*13          XNAME         
      CHARACTER*72          PICTFILE
      CHARACTER*72          INFILE
      CHARACTER*72          OUTFILE
      CHARACTER*80          LINE
      character*361         cline

      LOGICAL               USE
      real version

C     Set limiting resolution
      version = 2.3
      write (*,*) "Version: ", version
      WRITE(6,*) 'Input RESLIM (km/px) Accept everything lower"'
      READ(5,*) RESLIM

C		Use all unless coverage_p.in
      WRITE(6,*) 'Use all images? (y/n)'
      READ(5,FMT='(A1)') ANS
      INFILE='coverage_p.in'
      INQUIRE(FILE=INFILE, EXIST=USE)
      IF((.NOT.USE).OR.(ANS.EQ.'y')) INFILE='PICTLIST.TXT'
      open(unit=20, file=INFILE, status='old')


C		Initalize variables
      do i=1,361
      do j=1,181
        coverage(i,j)=0
        bestRes(i,j)=99999
      enddo
      enddo
      write (*,*) "Reading from SHAPE.TXT";
      do i=2,360
      do j=2,180
        z1=91-j
        z2=i-1
        CALL LATREC(1.d0,Z2*RPD(),Z1*RPD(), W)
        write (*,*) I,J
        CALL U2VN(W,V(1,I,J),UZ(1,I,J))
      enddo
      write (*,"(A1)", advance="no") "."
      enddo


C		Cycle over all images (SUMFILES)
10      continue
        read(20,fmt='(a13)') xname
        if(xname(1:1).eq.'!') go to 10
        if(xname(1:1).eq.'#') go to 10
        IF(xname(1:3).ne.'END') then
          write (*,*) xname
          PICNM=XNAME(2:13)
          I=SLEN(PICNM)
          PICTFILE='./SUMFILES/'//PICNM(1:I)//'.SUM'
          OPEN(UNIT=10,FILE=PICTFILE,STATUS='OLD')
            READ(10,*)
            READ(10,*)
            READ(10,*)  NPX, NLN
            READ(10,*)  MMFL, CTR(1), CTR(2)
            READ(10,*) (V0(I), I=1,3) 
            READ(10,*) (CX(I), I=1,3)
            READ(10,*) (CY(I), I=1,3)
            READ(10,*) (CZ(I), I=1,3)
            READ(10,*) (SZ(I), I=1,3)
            READ(10,*)  KMAT(1,1), KMAT(1,2), KMAT(1,3),
     .                  KMAT(2,1), KMAT(2,2), KMAT(2,3)
            READ(10,FMT='(A80)') LINE
            IF(LINE(64:73).EQ.'DISTORTION') THEN
              READ(LINE,*) (D(I), I=1,4)
            ELSE
              D(1)=0.D0
              D(2)=0.D0
              D(3)=0.D0
              D(4)=0.D0
            ENDIF
          CLOSE(UNIT=10)
          Z1=NPX/(2*KMAT(1,1)*MMFL)
          Z2=NLN/(2*KMAT(2,2)*MMFL)
          Z3=COS(SQRT(Z1**2+Z2**2))
          Z4=1.D0/(MMFL*SQRT(ABS(KMAT(1,1)*KMAT(2,2))))
          Z6=0
          Z7=0
          K=0

C         Cycle over every pixel
          do i=1,360
          do j=2,180
            CALL VADD(V0,V(1,i,j),W)
            Z5=VNORM(W)
            CALL VHAT(W,W)
            IF(VDOT(W,CZ).GT.Z3) THEN
            IF(-VDOT(W,UZ(1,i,j)).GT.0) THEN
            IF(VDOT(SZ,UZ(1,i,j)).GT.0) THEN
              CALL V2IMGPL(V(1,i,j),V0,PICNM,NPX,NLN,MMFL,CTR,KMAT,D,
     .                     CX,CY,CZ, USE,IMGPL)
              Z5=Z4*Z5/(-VDOT(W,UZ(1,i,j)))
              USE=USE.AND.(Z5.LE.RESLIM)


C             Add 1 rather than 15)
              IF(USE) THEN
                coverage(i,j)=min(255,coverage(i,j)+1)
                Z6=Z6-VDOT(W,UZ(1,i,j))
                Z7=Z7+Z5
                K=K+1
                if (Z5 .LT. bestRes (i, j) ) then
                   bestRes(i,j) = Z5
                endif
              ENDIF
            ENDIF
            ENDIF
            ENDIF
          enddo
          enddo

C         Maybe test over valid data
          IF(K.NE.0) THEN
            Z6=Z6/K
            Z7=Z7/K
            WRITE(6,*) PICNM, ACOS(Z6)/RPD(), Z7
          ENDIF
          go to 10
        ENDIF
      close(unit=20)
      write (*,*) "done thinking"

C     Output the file in temp grayscale and ascii
c      open (unit=10, file='coverage.gray', access='direct', 
c     .      recl=361, status='unknown')
C     Vector formats
      outfile='global-cov.c3.txt'
      open (unit=11, file=outfile)
      outfile='global-res.c3.txt'
      open (unit=12, file=outfile)
    
      write (*,*) "Pre grid"

C     Gridded formats
C      outfile='global-cov.grid.txt'
C      open (unit=13, file=outfile)
C      outfile='global-res.grid.txt'
C      open (unit=14, file=outfile)
        do j=1,181
          do i=1,360
            cline(i:i)=char(coverage(i,j))
            write (11, 99) i, 91-j, coverage(i,j)
            write (12, 98) i, 91-j, bestRes(i,j)
C            write (13, 96, advance="no") coverage(i,j)
C            write (14, 97, advance="no") bestRes(i,j)
          enddo
c          write(10,rec=j) cline 
C          write(13) 
C          write(14) 
        enddo

      write (*,*) "done grid"
c      close(unit=10)
      close(unit=11)
      close(unit=12)
C      close(unit=13)
C      close(unit=14)
 96   format (i7)
 97   format (f22.8)
 98   format (2i5, f18.8)
 99   format (3i5)

c      write(6,*) 
c      write(6,*) 'coverage done'
c
c
cC     Generate the PGM
c      npx=361
c      nln=181
c      infile='coverage.gray'
c      outfile='coverage_p.pgm'
c      call raw2pgm(infile,outfile,npx,nln)
c      write(6,*) 'gc coverage_p.pgm'
c      open (unit=10, file='coverage.gray', status='unknown')
c      close(unit=10, status='delete')

      STOP
      END

