c     gfortran -O2  map_coverage_p_low.f /usr/local/lib/spicelib.a /usr/local/lib/COMMON.a -o ~/bin/map_coverage_p_low
c		17 June 2021 - Eric E. Palmer
c     Adjusted to add 1 (vs 15) for each image detected
C     Also includes outputting the best resolution per pixel
C     Input 
C         The worst resolution image that will be used
C         A maplet name
C         If you don't want to use PICTLIST.TXT you can select a different one
C					as coverage_p.in
C     Output
C         pgm of the coverage with each DN as an image
C         txt of the coverage, actual vlaues
C         txt of the best resolution detected
C     Version 1.2 - 19 Oct 2021
C         Fixed resolution check (z5 rather than RESLIM)

      IMPLICIT NONE
      
      INTEGER               BTMP
      PARAMETER            (BTMP=1025)
      INTEGER               QSZ

      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      V0(3)
      DOUBLE PRECISION      S0
      DOUBLE PRECISION      VK(3,-BTMP:BTMP,-BTMP:BTMP)
      DOUBLE PRECISION      N(3,-BTMP:BTMP,-BTMP:BTMP)
      DOUBLE PRECISION      W(3)
      DOUBLE PRECISION      CX(3)
      DOUBLE PRECISION      CY(3)
      DOUBLE PRECISION      CZ(3)
      DOUBLE PRECISION      SZ(3)
      DOUBLE PRECISION      UX(3)
      DOUBLE PRECISION      UY(3)
      DOUBLE PRECISION      UZ(3)
      DOUBLE PRECISION      CTR(2)
      DOUBLE PRECISION      MMFL
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      IMGPL(2)
      DOUBLE PRECISION      RESLIM
      DOUBLE PRECISION      RES
      DOUBLE PRECISION      Z1, Z2, Z3, Z4, Z5, Z6, Z7

      REAL*4                HT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                AL(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZHT(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                ZAL(-BTMP:BTMP,-BTMP:BTMP)
      REAL*4                NH(-BTMP:BTMP,-BTMP:BTMP)
      real*4                tmpl(-btmp:btmp,-btmp:btmp,3)
      real*4                bestRes (-BTMP:BTMP, -BTMP:BTMP)

      INTEGER               NPX, NLN
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               usedK
      INTEGER               SLEN
      INTEGER               LMCOUNT      
      INTEGER               T1, T2

      CHARACTER*1           ANS        
      CHARACTER*6           BIGMAP        
      CHARACTER*12          PICNM         
      CHARACTER*13          XNAME         
      CHARACTER*72          PICTFILE
      CHARACTER*72          LMRKFILE
      CHARACTER*72          INFILE
      CHARACTER*72          OUTFILE
      CHARACTER*80          LINE
      character*(2051)      tline
      real version

      LOGICAL               USE
      LOGICAL               HUSE(-BTMP:BTMP,-BTMP:BTMP)
      LOGICAL               ZUSE(-BTMP:BTMP,-BTMP:BTMP)
      logical               tuse(-btmp:btmp,-btmp:btmp)

      version = 1.2

C Start with the bigmap, get its positional data
      write (*,*) "Version: ", version
      WRITE(6,*) 'Input RESLIM (km/px)'
      READ(5,*) RESLIM
      WRITE(6,*) 'Input MAPNAME'
      read(5,fmt='(a6)') BIGMAP
      LMRKFILE='./MAPFILES/'//BIGMAP//'.MAP'
      CALL READ_MAP(LMRKFILE,BTMP,QSZ,S0,V,UX,UY,UZ,HT,AL)
      CALL get_heights(btmp,qsz,ux,uy,uz,v,s0, zuse,zht,zal) 
      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        HUSE(I,J)=.TRUE.

C       If Albedo value is very low, skip that pixel on the maplet
        IF(AL(I,J).LT.(0.005)) THEN
          HUSE(I,J)=ZUSE(I,J)
          HT(I,J)=ZHT(I,J)
        ENDIF

C       Set pixel counter to 0
        NH(I,J)=0


C       Set best resolution value to high value to avoid 
        bestRes (i,J) = 9999
      ENDDO
      ENDDO
      call hgt2slp(btmp,qsz,HUSE,HT, tuse,tmpl)
      DO J=-QSZ,QSZ
      DO I=-QSZ,QSZ

C       VK is the vector to the 3D position of each surface element
      IF(HUSE(I,J)) THEN
        VK(1,I,J)=V(1)+S0*(J*UX(1)+I*UY(1)+HT(I,J)*UZ(1))
        VK(2,I,J)=V(2)+S0*(J*UX(2)+I*UY(2)+HT(I,J)*UZ(2))
        VK(3,I,J)=V(3)+S0*(J*UX(3)+I*UY(3)+HT(I,J)*UZ(3))

C       N is a vector for the normal reflecting the local slope
        if(tuse(i,j)) then
          z1=sqrt(1+tmpl(i,j,1)**2+tmpl(i,j,2)**2)
          n(1,i,j)=(uz(1)+ux(1)*tmpl(i,j,1)+uy(1)*tmpl(i,j,2))/z1
          n(2,i,j)=(uz(2)+ux(2)*tmpl(i,j,1)+uy(2)*tmpl(i,j,2))/z1
          n(3,i,j)=(uz(3)+ux(3)*tmpl(i,j,1)+uy(3)*tmpl(i,j,2))/z1
        endif
      ENDIF
      ENDDO
      ENDDO

C Get list of images to check -- coverage_p.in allows a subset (if you also say 'y')
      WRITE(6,*) 'Use all images? (y/n)'
      READ(5,FMT='(A1)') ANS
      INFILE='coverage_p.in'
      INQUIRE(FILE=INFILE, EXIST=USE)
      IF((.NOT.USE).OR.(ANS.EQ.'y')) INFILE='PICTLIST.TXT'
      open(unit=20, file=INFILE, status='old')
      open(unit=30, file='coverage_p.out', status='unknown')


C Loop through all SUMFILES to get geometry
        WRITE(6,FMT='(1X,A12,2A10,4A10)')  "PICNM", "Angle", "Res?",
     .     "# LMK", "Min Thres", "Max Thres", "Pixel", "Used"
        WRITE(30,FMT='(1A,A12,2A10,4A10)')  "#", "PICNM","Angle","Res?",
     .     "# LMK", "Min Thres", "Max Thres", "Pixel", "Used"
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
            READ(10,*)  NPX, NLN, T1, T2
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
C         Z1 and Z2 - are likely the physical size of the detector
          Z1=NPX/(2*KMAT(1,1)*MMFL)
          Z2=NLN/(2*KMAT(2,2)*MMFL)

C         Z3 is the cos of the diagonal
          Z3=COS(SQRT(Z1**2+Z2**2))
          Z4=1.D0/(MMFL*SQRT(ABS(KMAT(1,1)*KMAT(2,2))))
          Z6=0
          Z7=0
          K=0
          usedK=0

C         Loop over maplet's boundary
          DO J=-QSZ,QSZ
          DO I=-QSZ,QSZ
          IF(HUSE(I,J).AND.TUSE(I,J)) THEN
C           V0 is SCOBJ, W *should* be the vector from the s/c to surface
            CALL VADD(V0,VK(1,I,J),W)

C           Z5 is the scaler, distance from the S/C
            Z5=VNORM(W)
            CALL VHAT(W,W)

            IF(VDOT(W,CZ).GT.Z3) THEN

C           Testing to see if illuminated 
            IF(-VDOT(W,N(1,I,J)).GT.0) THEN
            IF(VDOT(SZ,N(1,i,j)).GT.0) THEN

C           Use V2IMGPL to see if the pixel is valid for the image
              CALL V2IMGPL(VK(1,I,J),V0,PICNM,NPX,NLN,MMFL,CTR,
     .                     KMAT,D,CX,CY,CZ, USE,IMGPL)

C             Check for image resolution
              RES = Z4 * Z5
C             Bob's calculation of resolution (Z5)
              Z5=Z4*Z5/(-VDOT(W,N(1,i,j)))
              USE=USE.AND.(Z5.LE.RESLIM)


C             Incremement counter
C             Sum the emission and image resolution for average
C             Store better resolution
              IF(USE) THEN 
                NH(I,J)=NH(I,J)+1 
                Z6=Z6-VDOT(W,N(1,i,j))
                Z7=Z7+Z5
                K=K+1

C 	              Res is the current pixel resolution.  Save it if it's better
                 if (RES .LT. bestRes(i,j) ) then
                    bestRes(i,j) = RES
                    usedK=usedK+1
                 endif

              ENDIF
            ENDIF
            ENDIF
            ENDIF
          ENDIF
          ENDDO
          ENDDO

C         If image was used, report some statistics to stdout
          IF(K.NE.0) THEN
C           Average the emission angle and image resolution that was used
            Z6=Z6/K
            Z7=Z7/K
            I=LMCOUNT(PICNM,0)
            WRITE(6,FMT='(1X,A12,2F10.3,5I10)')  PICNM, 
     .        ACOS(Z6)/RPD(), Z7, I, T1, T2, K, usedK
            WRITE(30,FMT='(1X,A12,2F10.3,5I10)') PICNM, 
     .        ACOS(Z6)/RPD(), Z7, I, T1, T2, K, usedK
           ENDIF
          go to 10
        ENDIF
      close(unit=30)
      close(unit=20)

C Make a binary 2D array before it is converted to a pgm
      open(unit=55,file='coverage.gray', access='direct', 
     .     recl=2*qsz+1, status='unknown')
      outfile=BIGMAP//'-cov.txt'
      open(unit=56,file=outfile)
      outfile=BIGMAP//'-res.txt'
      open(unit=57,file=outfile)
        do j=-qsz,qsz
          do i=-qsz,qsz
C           Take the number of images and multipy it by 1 (or 15 originally)
            z1=1*nh(i,j)
c            z1=15*nh(i,j)
            z1=min(255.,z1)
            tline(i+qsz+1:i+qsz+1)=char(nint(z1))
            write (56, 98, advance="no") nh(i,j)
            write (57, 99, advance="no") bestRes(i,j)
          enddo
          write(55,rec=j+qsz+1) tline(1:2*qsz+1)
          write (56, 99) 
          write (57, 99) 
        enddo
      close(unit=55)
      close(unit=56)
      close(unit=57)
 98   format (f9.0)
 99   format (f16.8)

C Convert to PGM
C Gray is unsigned 8-bit char (0-255)
      j=2*qsz+1
      infile='coverage.gray'
      outfile=BIGMAP//'-cov.pgm'
      call raw2pgm(infile,outfile,j,j)
      open (unit=10, file='coverage.gray', status='unknown')
      close(unit=10, status='delete')

      STOP
      END

