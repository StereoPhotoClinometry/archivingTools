c   ................................................
      SUBROUTINE XV2IMGPL(V,V0,PICNM,NPX,NLN,MMFL,CTR,KMAT,D,
     .                   CX,CY,CZ, USE,IMGPL)
c   ................................................

      IMPLICIT NONE

      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      DOUBLE PRECISION      IMGPL(2)
      DOUBLE PRECISION      MMFL
      DOUBLE PRECISION      KMAT(2,3)
      DOUBLE PRECISION      D(4)
      DOUBLE PRECISION      CTR(2)
      DOUBLE PRECISION      MM(2)
      DOUBLE PRECISION      Z(2)
      DOUBLE PRECISION      V0(3)
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      V1(3)
      DOUBLE PRECISION      U(3)
      DOUBLE PRECISION      CX(3)      
      DOUBLE PRECISION      CY(3)
      DOUBLE PRECISION      CZ(3)
      DOUBLE PRECISION      WX(3)
      DOUBLE PRECISION      WY(3)
      DOUBLE PRECISION      WZ(3)
      DOUBLE PRECISION      Z1
      DOUBLE PRECISION      Z2
      DOUBLE PRECISION      Z3
      DOUBLE PRECISION      R0(3)
      DOUBLE PRECISION      U0(3)
      DOUBLE PRECISION      CAX(20,3)
      DOUBLE PRECISION      TRJ_AXIS(3)
      DOUBLE PRECISION      TRJ(3)
      DOUBLE PRECISION      POL_AXIS(3)
      DOUBLE PRECISION      POL
      DOUBLE PRECISION      X0, X1, Y0, Y1
          
      INTEGER               I
      INTEGER               K
      INTEGER               KMAX
      INTEGER               NPX
      INTEGER               NLN
      INTEGER               SLEN
      INTEGER               IFF
          
      CHARACTER*12          PICNM, saveName
      CHARACTER*72          PICTFILE
      CHARACTER*80          LINE
          
      LOGICAL               USE
      LOGICAL               LS

      SAVE                  IFF, saveName
      SAVE   U0, R0, POL_AXIS, POL, TRJ_AXIS, TRJ, KMAX, CAX




      if (PICNM .NE. saveName) then
        saveName = PICNM
        LS=.FALSE.
        I=SLEN(PICNM)
        PICTFILE='SUMFILES/'//PICNM(1:I)//'.SUM'
        OPEN(UNIT=32,FILE=PICTFILE,STATUS='OLD')
          READ(32,FMT='(A80)') LINE
          IF(LINE(14:14).EQ.'L') THEN
            LS=.TRUE.
10          CONTINUE
            READ(32,FMT='(A80)') LINE
            IF(LINE(1:14).NE.'LINE SCAN DATA') GO TO 10
            READ(32,*) (U0(I), I=1,3) 
            READ(32,*) (R0(I), I=1,3)
            READ(32,*) (POL_AXIS(I), I=1,3)
            READ(32,*) POL
            READ(32,*) (TRJ_AXIS(I), I=1,3)
            READ(32,*) (TRJ(I), I=1,3)
            READ(32,*) KMAX
            DO K=1,KMAX
              READ(32,*) (CAX(K,I), I=1,3)
            ENDDO
          ENDIF
        CLOSE(UNIT=32)

      endif

      USE=.FALSE.

      IF(LS) THEN
        R0(1)=VNORM(V0)
        CALL VHAT(V0,U0)
        Z(2)=CTR(2)
        DO K=1,3
          X0=Z(2)-50
          CALL SCOBJ(R0,U0,TRJ_AXIS,TRJ,POL_AXIS,POL,CTR(2),X0,V1)
          CALL POINTING(CX,CY,CZ,CAX,KMAX,TRJ_AXIS,TRJ,
     .                      POL_AXIS,POL,CTR(2),X0,WX,WY,WZ)
          CALL VADD(V1,V,U)
          Z1=VDOT(U,WX)
          Z2=VDOT(U,WY)
          Z3=VDOT(U,WZ)
          call getmm(picnm,mmfl,z1,z2,z3,mm)
          Y0=MM(2)
          X1=Z(2)+50
          CALL SCOBJ(R0,U0,TRJ_AXIS,TRJ,POL_AXIS,POL,CTR(2),X1,V1)
          CALL POINTING(CX,CY,CZ,CAX,KMAX,TRJ_AXIS,TRJ,
     .                      POL_AXIS,POL,CTR(2),X1,WX,WY,WZ)
          CALL VADD(V1,V,U)
          Z1=VDOT(U,WX)
          Z2=VDOT(U,WY)
          Z3=VDOT(U,WZ)
          call getmm(picnm,mmfl,z1,z2,z3,mm)
          Y1=MM(2)
          Z(2)=(X0*Y1-X1*Y0)/(Y1-Y0)
        ENDDO
        X0=Z(2)
        CALL SCOBJ(R0,U0,TRJ_AXIS,TRJ,POL_AXIS,POL,CTR(2),X0,V1)
        CALL POINTING(CX,CY,CZ,CAX,KMAX,TRJ_AXIS,TRJ,
     .                    POL_AXIS,POL,CTR(2),X0,WX,WY,WZ)
        CALL VADD(V1,V,U)
        Z1=VDOT(U,WX)
        Z2=VDOT(U,WY)
        Z3=VDOT(U,WZ)
        call getmm(picnm,mmfl,z1,z2,z3,mm)
        IMGPL(1)=CTR(1)+KMAT(1,1)*MM(1)
        IMGPL(2)=X0
        IF((IMGPL(1).GE.1).AND.(IMGPL(1).LE.NPX).AND.
     .     (IMGPL(2).GE.1).AND.(IMGPL(2).LE.NLN)) THEN
          USE=.TRUE.
        ENDIF
        RETURN
      ENDIF

      CALL VADD(V0,V,U)
      CALL VHAT(U,U)
      Z1=VDOT(U,CX)
      Z2=VDOT(U,CY)
      Z3=VDOT(U,CZ)
      call getmm(picnm,mmfl,z1,z2,z3,mm)
      CALL MMPX(KMAT,CTR,D,MM,IMGPL)
      IF((IMGPL(1).GE.1).AND.(IMGPL(1).LE.NPX).AND.
     .   (IMGPL(2).GE.1).AND.(IMGPL(2).LE.NLN)) THEN
        USE=.TRUE.
      ENDIF

      RETURN
      END

