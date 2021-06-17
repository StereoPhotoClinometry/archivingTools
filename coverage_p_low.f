c     ftn  coverage_p.f /Users/rgaskell/toolkit/lib/spicelib.a -o coverage_p.e 

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

      WRITE(6,*) 'Input RESLIM (km/px)'
      READ(5,*) RESLIM

      do i=1,361
      do j=1,181
        coverage(i,j)=0
      enddo
      enddo
      do i=1,361
      do j=2,180
        z1=91-j
        z2=i-1
        CALL LATREC(1.d0,Z2*RPD(),Z1*RPD(), W)
        CALL U2VN(W,V(1,I,J),UZ(1,I,J))
      enddo
      enddo

      WRITE(6,*) 'Use all images? (y/n)'
      READ(5,FMT='(A1)') ANS
      INFILE='coverage_p.in'
      INQUIRE(FILE=INFILE, EXIST=USE)
      IF((.NOT.USE).OR.(ANS.EQ.'y')) INFILE='PICTLIST.TXT'
      open(unit=20, file=INFILE, status='old')
10      continue
        read(20,fmt='(a13)') xname
        if(xname(1:1).eq.'!') go to 10
        if(xname(1:1).eq.'#') go to 10
        IF(xname(1:3).ne.'END') then
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
          do i=1,361
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
              IF(USE) THEN
                coverage(i,j)=min(255,coverage(i,j)+1)
                Z6=Z6-VDOT(W,UZ(1,i,j))
                Z7=Z7+Z5
                K=K+1
              ENDIF
            ENDIF
            ENDIF
            ENDIF
          enddo
          enddo
          IF(K.NE.0) THEN
            Z6=Z6/K
            Z7=Z7/K
            WRITE(6,*) PICNM, ACOS(Z6)/RPD(), Z7
          ENDIF
          go to 10
        ENDIF
      close(unit=20)

      open (unit=10, file='coverage.gray', access='direct', 
     .      recl=361, status='unknown')
        do j=1,181
          do i=1,361
            cline(i:i)=char(coverage(i,j))
          enddo
          write(10,rec=j) cline 
        enddo
      close(unit=10)
      write(6,*) 
      write(6,*) 'coverage done'

      npx=361
      nln=181
      infile='coverage.gray'
      outfile='coverage_p.pgm'
      call raw2pgm(infile,outfile,npx,nln)
      write(6,*) 'gc coverage_p.pgm'
      open (unit=10, file='coverage.gray', status='unknown')
      close(unit=10, status='delete')

      STOP
      END
