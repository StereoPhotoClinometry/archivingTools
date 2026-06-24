C  PROCEDURE MOSAICX

c   ................................................
      SUBROUTINE SCOBJ(R0,U0,TRJ_AXIS,TRJ,POL_AXIS,POL,LN0,LN,V)
c   ................................................

      IMPLICIT NONE

      DOUBLE PRECISION  R0(3)
      DOUBLE PRECISION  U0(3)
      DOUBLE PRECISION  TRJ_AXIS(3)
      DOUBLE PRECISION  TRJ(3)
      DOUBLE PRECISION  POL_AXIS(3)
      DOUBLE PRECISION  POL
      DOUBLE PRECISION  LN0
      DOUBLE PRECISION  LN
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  Z

      Z=TRJ(1)*(LN-LN0)+TRJ(2)*(LN-LN0)**2+TRJ(3)*(LN-LN0)**3
      CALL VROTV(U0,TRJ_AXIS,Z,V)
      Z=R0(1)+R0(2)*(LN-LN0)+R0(3)*(LN-LN0)**2
      CALL VSCL(Z,V,V)
      Z=-POL*(LN-LN0)
      CALL VROTV(V,POL_AXIS,Z,V)
          
      RETURN
      END

c   ................................................
      SUBROUTINE POINTING(CX,CY,CZ,CAX,IMAX,TRJ_AXIS,TRJ,
     .                    POL_AXIS,POL,LN0,LN,WX,WY,WZ)
c   ................................................

      IMPLICIT NONE

      DOUBLE PRECISION  CX(3)
      DOUBLE PRECISION  CY(3)
      DOUBLE PRECISION  CZ(3)
      DOUBLE PRECISION  CAX(20,3)
      DOUBLE PRECISION  TRJ_AXIS(3)
      DOUBLE PRECISION  TRJ(3)
      DOUBLE PRECISION  POL_AXIS(3)
      DOUBLE PRECISION  POL
      DOUBLE PRECISION  LN0
      DOUBLE PRECISION  LN
      DOUBLE PRECISION  WX(3)
      DOUBLE PRECISION  WY(3)
      DOUBLE PRECISION  WZ(3)
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  CMAT(3,3)
      DOUBLE PRECISION  XMAT(3,3)
      DOUBLE PRECISION  Z
      
      INTEGER           I
      INTEGER           J
      INTEGER           IMAX

      DO J=1,3
        CMAT(1,J)=CX(J)
        CMAT(2,J)=CY(J)
        CMAT(3,J)=CZ(J)
      ENDDO
      IF(LN.NE.LN0) THEN
        DO J=1,3
          V(J)=0
          DO I=1,IMAX
            V(J)=V(J)+CAX(I,J)*(LN-LN0)**I
          ENDDO
        ENDDO
        CALL ROT2MAT(V,XMAT)
        CALL MXM(XMAT,CMAT,CMAT)
      ENDIF
      DO J=1,3
        WX(J)=CMAT(1,J)
        WY(J)=CMAT(2,J)
        WZ(J)=CMAT(3,J)
      ENDDO
      Z=TRJ(1)*(LN-LN0)+TRJ(2)*(LN-LN0)**2+TRJ(3)*(LN-LN0)**3
      CALL VROTV(WX,TRJ_AXIS,Z,WX)
      CALL VROTV(WY,TRJ_AXIS,Z,WY)
      CALL VROTV(WZ,TRJ_AXIS,Z,WZ)
      Z=-POL*(LN-LN0)
      CALL VROTV(WX,POL_AXIS,Z,WX)
      CALL VROTV(WY,POL_AXIS,Z,WY)
      CALL VROTV(WZ,POL_AXIS,Z,WZ)
          
      RETURN
      END


c   ................................................
      SUBROUTINE ROT2MAT(V,M)
c   ................................................

      implicit none

      real*8     V(3), M(3,3), U(3), Z, VNORM
      integer*4  i, j
          
      Z=VNORM(V)
      IF(Z.EQ.0) THEN
        DO I=1,3
        DO J=1,3
          M(I,J)=0.D0
        ENDDO
        ENDDO
        DO I=1,3
          M(I,I)=1.D0
        ENDDO
        RETURN
      ENDIF
      CALL VHAT(V,U)
            
      do i=1,3
      do j=1,3
        M(i,j)=u(i)*u(j)*(1.d0-cos(z))
      enddo
      enddo
      M(1,1)=M(1,1)+cos(z)
      M(2,2)=M(2,2)+cos(z)
      M(3,3)=M(3,3)+cos(z)
      M(1,2)=M(1,2)-u(3)*sin(z)
      M(2,1)=M(2,1)+u(3)*sin(z)
      M(2,3)=M(2,3)-u(1)*sin(z)
      M(3,2)=M(3,2)+u(1)*sin(z)
      M(3,1)=M(3,1)-u(2)*sin(z)
      M(1,3)=M(1,3)+u(2)*sin(z)
        
      return
      end

C   ..................................................
      SUBROUTINE MMPX(KMAT,CTR,D,MM,PX)
C   ..................................................

      IMPLICIT NONE
          
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      CTR(2)
      DOUBLE PRECISION      MM(2)  
      DOUBLE PRECISION      PX(2)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      RSQ
      INTEGER               K
      LOGICAL               USE
      
      X=MM(1)*(1.d0+D(1))
      Y=MM(2)*(1.d0+D(1))

      USE=.FALSE.
      DO K=2,4
        USE=USE.OR.(D(K).NE.0)
      ENDDO
      
      IF(USE) THEN
        RSQ=X**2+Y**2
        Z=1.d0+D(2)*RSQ+D(3)*Y+D(4)*X
        X=X*Z
        Y=Y*Z
      ENDIF
          
      PX(1)=CTR(1)+KMAT(1,1)*X+KMAT(1,2)*Y+KMAT(1,3)*X*Y
      PX(2)=CTR(2)+KMAT(2,1)*X+KMAT(2,2)*Y+KMAT(2,3)*X*Y
          
      RETURN
      END

C   ..................................................
      subroutine getmm(picnm,mmfl,z1,z2,z3,z)
C   ..................................................

      IMPLICIT NONE

      double precision      mmfl
      double precision      z1, z2, z3
      double precision      z(2)
      double precision      r
      double precision      pd(0:100,100)
      double precision      X(100)

      INTEGER               JD
      INTEGER               KD
      INTEGER               I
      INTEGER               J

      CHARACTER*12          PICNM
      CHARACTER*12          TMPL(100)
      CHARACTER*72          TYPE(100)
      CHARACTER*80          LINE

      LOGICAL               EX

      INTEGER*4             IFF
      SAVE                  IFF, KD, TMPL, TYPE, PD
      data                  iff/0/

      IF(IFF.EQ.0) THEN
        IFF=1
        KD=0
        INQUIRE(FILE='INIT_LITHOS.TXT',EXIST=EX)
        IF(EX) THEN
          OPEN(UNIT=25,FILE='INIT_LITHOS.TXT',STATUS='OLD')
13          CONTINUE
            READ(25,FMT='(A80)') LINE
            IF(LINE(1:3).NE.'END') THEN
              IF(LINE(1:8).EQ.'DISTORT=') THEN
                KD=KD+1
                READ(LINE(9:80),*) TMPL(KD), TYPE(KD), J
                READ(25,*) (PD(I,KD), I=0,J)
              ENDIF
              GO TO 13
            ENDIF 
          CLOSE(UNIT=25)
        ENDIF
      ENDIF

      IF(KD.NE.0) THEN
        JD=0
        DO J=1,KD
          DO I=1,12
            IF((TMPL(J)(I:I).NE.'#').AND.(TMPL(J)(I:I).NE.' ')
     .         .AND.(PICNM(I:I).NE.TMPL(J)(I:I))) GO TO 10
          ENDDO
          JD=J
10        CONTINUE
        ENDDO
        IF(JD.NE.0) THEN
          IF(TYPE(JD).EQ.'CUBIC') THEN
            z(1)=(1+PD(0,JD))*mmfl*z1/z3
            z(2)=(1+PD(0,JD))*mmfl*z2/z3
            X( 1)=1.D0
            X( 2)=z(1)
            X( 3)=z(2)            
            X( 4)=z(1)*z(1)
            X (5)=z(1)*z(2)            
            X( 6)=z(2)*z(2)            
            X( 7)=z(1)*z(1)*z(1)
            X( 8)=z(1)*z(1)*z(2)            
            X( 9)=z(1)*z(2)*z(2)            
            X(10)=z(2)*z(2)*z(2)   
            z(1)=0
            z(2)=0  
            DO I=1,10
              z(1)=z(1)+PD(I,JD)*X(I)
              z(2)=z(2)+PD(I+10,JD)*X(I)
            ENDDO
            RETURN
          ENDIF         
          IF(TYPE(JD).EQ.'RADIAL') THEN
            z(1)=(1+PD(0,JD))*mmfl*z1/z3
            z(2)=(1+PD(0,JD))*mmfl*z2/z3
            X(1)=z(1)**2+z(2)**2
            z(1)=z(1)*(1+PD(1,JD)*X(1)+PD(2,JD)*X(1)**2)           
            z(2)=z(2)*(1+PD(1,JD)*X(1)+PD(2,JD)*X(1)**2)           
            RETURN
          ENDIF         
          IF(TYPE(JD).EQ.'OWEN') THEN
            z(1)=(1+PD(0,JD))*mmfl*z1/z3
            z(2)=(1+PD(0,JD))*mmfl*z2/z3
            R=SQRT(z(1)**2+z(2)**2)
            X(1)=z(1)
            X(2)=z(2) 
            z(1)=z(1)-PD(1,JD)*X(2)*R+PD(2,JD)*X(1)*R**2
     .               -PD(3,JD)*X(2)*R**3+PD(4,JD)*X(1)*R**4
     .               +PD(5,JD)*X(1)*X(2)+PD(6,JD)*X(1)*X(1)
            z(2)=z(2)+PD(1,JD)*X(1)*R+PD(2,JD)*X(2)*R**2
     .               +PD(3,JD)*X(1)*R**3+PD(4,JD)*X(2)*R**4
     .               +PD(5,JD)*X(2)*X(2)+PD(6,JD)*X(1)*X(2)
            RETURN
          ENDIF         
          IF(TYPE(JD).EQ.'LINESCAN') THEN
            z(2)=mmfl*z2/z3
            Z(1)=(1+PD(0,JD))*mmfl*z1/z3
            R=SQRT(Z(1)**2+PD(1,JD)**2)
            Z(1)=Z(1)*(1+PD(2,JD)*R**2+PD(3,JD)*R**3)
            RETURN
          ENDIF         
          IF(TYPE(JD).EQ.'APOLLO') THEN
            z(1)=(1+PD(0,JD))*mmfl*z1/z3
            z(2)=(1+PD(0,JD))*mmfl*z2/z3
            X(1)=z(1)**2+z(2)**2
            X(2)=X(1)*X(1)
            X(3)=X(1)*X(2)
            z(1)=z(1)*(1+PD(1,JD)*X(1)+PD(2,JD)*X(2)+PD(3,JD)*X(3))           
            z(2)=z(2)*(1+PD(1,JD)*X(1)+PD(2,JD)*X(2)+PD(3,JD)*X(3))
            X(4)=PD(4,JD)*X(1)+PD(5,JD)*X(2)
            Z(1)=Z(1)+X(4)*SIN(PD(6,JD))           
            Z(2)=Z(2)-X(4)*COS(PD(6,JD))           
            RETURN
          ENDIF         
          IF(TYPE(JD).EQ.'HIRISE') THEN
            z(1)=(1+PD(0,JD))*mmfl*z1/z3
            z(2)=(1+PD(0,JD))*mmfl*z2/z3
            z(1)=z(1)-PD(1,JD)
            z(2)=z(2)-PD(2,JD)
            RETURN
          ENDIF         
        ENDIF
      ENDIF

      z(1)=mmfl*z1/z3
      z(2)=mmfl*z2/z3

      RETURN
      END
      
C   ..................................................
      FUNCTION SLEN(STRING)
C   ..................................................

      IMPLICIT NONE
      
      INTEGER*4        SLEN, I
      CHARACTER*(*)     STRING
      
      SLEN=LEN(STRING)
      DO I=1,SLEN
      IF(STRING(I:I).EQ.' ') THEN
        SLEN=I-1
        RETURN
      ENDIF
      ENDDO

      RETURN
      END            
      
      
c  ..............................................
      subroutine raw2pgm(infile,outfile,npx,nln)
c  ..............................................

      implicit none
      
      integer*4             npx, nln, i, j, k
      character*10          line
      character*50          header
      character*72          infile, outfile
      character*30000000    dn

      
      header(1:2)='P5'
      header(3:3)=char(10)
      header(4:5)='#.'
      header(6:6)=char(10)
      k=7
      write(line,fmt='(i10)') npx
      do i=1,10
      if(line(i:i).ne.' ') then
        header(k:k+10-i) = line(i:10)
        k=k+11-i
        go to 10
      endif
      enddo
10    continue
      header(k:k)=' '
      k=k+1
      write(line,fmt='(i10)') nln
      do i=1,10
      if(line(i:i).ne.' ') then
        header(k:k+10-i) = line(i:10)
        k=k+11-i
        go to 20
      endif
      enddo
20    continue
      header(k:k)=char(10)
      k=k+1
      header(k:k+2)='255'
      k=k+3
      header(k:k)=char(10)
      
      open(unit=10, file=infile, recl=npx, access='direct', 
     .     status='old')
        dn(1:k)=header(1:k)
        do j=1,nln
          read(10,rec=j) dn(k+(j-1)*npx+1:k+j*npx)
        enddo
      close(unit=10)
      open(unit=20, file=outfile, status='unknown')
      close(unit=20, status='delete')
      open(unit=20, file=outfile, recl=npx, access='direct', 
     .     status='unknown')
        do j=1,nln
          write(20,rec=j) dn(1+(j-1)*npx:j*npx)
        enddo
      close(unit=20)
      open(unit=20, file=outfile, recl=1, access='direct', 
     .     status='unknown')
        do j=npx*nln+1,npx*nln+k
          write(20,rec=j) dn(j:j)
        enddo
      close(unit=20)
      
      return
      end

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
      

