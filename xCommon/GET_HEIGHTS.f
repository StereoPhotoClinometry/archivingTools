C$ Procedure
 
      SUBROUTINE GET_HEIGHTS(NTMP,QSZ,UX,UY,UZ,V0,SCALE, HUSE,HT,AL)

C$ Abstract
C
C     This subroutine populates a maplet with heights and albedos from a
C     shape model.  The maplet coordinate system shown below defines a 
C     piece of surface by heights relative to a grid on a plane.
C
C                                 Uz
C                                  ^
C                 ---> i           |
C                  ________________|__________
C             \    \               |          \
C              \    \              |           \
C               j    \             -------------\---------> Uy
C                     \             \            \
C                      \             \            \
C                       \_____________\____________\
C                                      \
C                                       \
C                                        Ux
C
C     Grid points are labled (i,j) where i and j run from -qsz to qsz.  Each
C     point has an associated height and albedo.  The SCALE is the distance 
C     between adjacent grid points.  It is measured in kilometers.  The height
C     HT relative to the plane is also measured in units of the scale, so if V0 
C     is the body-fixed vector to the maplet center (0,0) the vector to a maplet
C     point i,j is
C
C           V(i,j) = V0 + SCALE X (iUy + jUx + HT(i,j)Uz) 
C     
C     The SHAPE or GLOBAL TOPOGRAPHY model is in the implicitly connected
C     quadrilateral (ICQ) format.  Body-fixed surface vectors are labled as
C     v(I,J,F) where I and J have values from 0 to q, and f from 1 to 6.  The 
C     parameter q is always a power of 2, and is usually taken to be 512, 
C     yielding a 1.57 million vector model.  If one or both of I or J is 0 or q
C     then the point I,J,F corresponds to a point with another F value as 
C     determined by the subroutines CORNERS and MATCHUP.  The labeling scheme
C     can be visualized as 6 grids on the faces of a cube, with common points
C     on the edges and corners.  Four adjacent surface points form a cell of
C     the model as shown below.  
C
C
C                 . I,J (0)
C                                  . I+1,J (1)
C                      x I+w1,J+w2
C                 .               
C               I,J+1 (2)
C                               .  I+1,J+1 (3)
C
C     The vectors to each of these points are projected into the maplet frame
C     Ux, Uy, Uz.  Each point maps into a maplet point X, Y, H(X,Y) where X,Y 
C     are not necessarily integers and so do not corresponding to maplet grid 
C     points.  If the corner points are labeled k=0,1,2,3 as above, the values
C     of some quantity B(k) associated with those points can be interpolated 
C     to the point I+w1,J+w2 through bilinear interpolation.  With the 
C     definitions
C
C                  b0=(0)          b1=B(1)-B(0)
C                  b2=B(2)-B(0)    b3=B(0)-B(1)-B(2)+B(3)
C                  
C     the interpolated value is
C
C                  B = b0 + b1w1 + b2w2 + b3w1w2
C
C     The subroutine determines values w1,w2 so that the X(k) and yYk)
C     interpolate to integer values lying within the maplet (between -qsz
C     and qsz):
C
C                  i = x0 + x1w1 + x2w2 + x3w1w2
C                  j = y0 + y1w1 + y2w2 + y3w1w2
C
C     then the height and albedo at this point is taken to be
C
C                  h(i,j) = h0 + h1w1 + h2w2 + h3w1w2
C                  a(i,j) = a0 + a1w1 + a2w2 + a3w1w2
C
C     There is the possibility that two parts of the shape map into the same
C     i,j position on the maplet.  For example, we might be looking clear 
C     through the shape model to the other side.  In order to avoid this 
C     mistake, we always choose the values corresponding the the largest height
C     at the point i,j.
C
C     The interpolation is allowed to spill outside the cell slightly with a 
C     small parameter EPS.  It is unclear whether this is really necessary.
C
C$ Disclaimer
C
C
C$ Required_Reading
C
C     R.W. Gaskell, et.al, "Characterizing and navigating small bodies
C           with imaging data", Meteoritics & Planetary Science 43,
C           Nr 6, 1049-1061 (2008)
C
C$ Declarations

      use getModel
      INTEGER*4     NTMP, QSZ  

      REAL*8        UX(3), UY(3), UZ(3), V0(3), SCALE, VK(3), EPS,
     .              X(0:3), Y(0:3), Z(0:3), W(0:3), W1, W2, A, B, C
C     .              VEC(3,0:512,0:512,6), A0(0:512,0:512,6)
      REAL*8, allocatable:: VEC(:,:,:,:)
      REAL*8, allocatable:: A0(:,:,:)


      DOUBLE PRECISION      VDOT

      REAL*4        HT(-NTMP:NTMP,-NTMP:NTMP), REAL4
      REAL*4        AL(-NTMP:NTMP,-NTMP:NTMP), AL4

      INTEGER*4     Q, I, J, F, I0, J0, L, 
     .              IMIN, IMAX, JMIN, JMAX 
      
      LOGICAL       HUSE(-NTMP:NTMP,-NTMP:NTMP)
      LOGICAL       EX

      CHARACTER*72  INFILE

      SAVE         IFF, Q, VEC, A0
      DATA         IFF/0/
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NTMP
C     QSZ
C     UX
C     UY
C     UZ
C     V0
C     SCALE
C     HUSE
C     HT
C     AL
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     SHAPE.TXT or SHAPEA.TXT        I   Shape model that contains 
C                                        heights.
C$ Restrictions
C
C
C$ Software_Documentation
C
C     OSIRIS-REx Stereophotoclinometry Software Design Document
C     OSIRIS-REx Stereophotoclinometry Software User's Guide
C
C$ Author_and_Institution
C
C     R.W. Gaskell    (PSI)
C
C$ Version
C
C
C
C$ SPC_functions_called
C     None
C
C$ SPC_subroutines_called
C     GET_MODEL
C
C$ SPICELIB_functions_called
C     VSUB
C
C$ SPICELIB_subroutines_called
C     VDOT
C
C$ Called_by_SPC_Programs
C     AUTOREGISTER
C     LITHOS
C     REGISTER
C     BIGMAP
C     BIGMAPL
C     SUBROUTINE CREATE_LMFILE
C     SUBROUTINE LIMG_HEIGHTS
C     SUBROUTINE VISIBLE

C     .              VEC(3,0:512,0:512,6), A0(0:512,0:512,6)

      IF(IFF.EQ.0) THEN
        IFF=1
        allocate (VEC (3,0:512, 0:512, 6))
        allocate (A0 (0:512, 0:512, 6))
        INFILE='SHAPEFILES/SHAPEA.TXT'
        INQUIRE(FILE=INFILE, EXIST=EX)
        IF(.NOT.EX) THEN
          INFILE='SHAPEFILES/SHAPE.TXT'
        ENDIF
        CALL GET_MODEL(INFILE,Q,VEC,A0)
      ENDIF

      DO I0=-QSZ,QSZ
      DO J0=-QSZ,QSZ
        HT(I0,J0)=-1.D10
        HUSE(I0,J0)=.FALSE.
      ENDDO
      ENDDO
      EPS=0.01
      
      DO F=1,6
      DO I=0,Q-1
      DO J=0,Q-1
        L=-1
        DO J0=0,1
        DO I0=0,1
          L=L+1
          CALL VSUB(VEC(1,I+I0,J+J0,F),V0,VK)
          X(L)=VDOT(UX,VK)/SCALE
          Y(L)=VDOT(UY,VK)/SCALE
          Z(L)=VDOT(UZ,VK)/SCALE
          W(L)=A0(I+I0,J+J0,F)
        ENDDO
        ENDDO

        JMAX=MIN( QSZ,NINT(MAX(X(0),X(1),X(2),X(3)))+1)
        JMIN=MAX(-QSZ,NINT(MIN(X(0),X(1),X(2),X(3)))-1)
        IMAX=MIN( QSZ,NINT(MAX(Y(0),Y(1),Y(2),Y(3)))+1)
        IMIN=MAX(-QSZ,NINT(MIN(Y(0),Y(1),Y(2),Y(3)))-1)

        X(3)=X(0)-X(1)-X(2)+X(3)
        X(1)=X(1)-X(0)
        X(2)=X(2)-X(0)
        Y(3)=Y(0)-Y(1)-Y(2)+Y(3)
        Y(1)=Y(1)-Y(0)
        Y(2)=Y(2)-Y(0)
        Z(3)=Z(0)-Z(1)-Z(2)+Z(3)
        Z(1)=Z(1)-Z(0)
        Z(2)=Z(2)-Z(0)
        W(3)=W(0)-W(1)-W(2)+W(3)
        W(1)=W(1)-W(0)
        W(2)=W(2)-W(0)
              
        DO I0=IMIN,IMAX
        DO J0=JMIN,JMAX
          A=X(1)*Y(3)-X(3)*Y(1)
          B=X(1)*Y(2)-X(2)*Y(1)+(I0-Y(0))*X(3)-(J0-X(0))*Y(3)
          C=(I0-Y(0))*X(2)-(J0-X(0))*Y(2)
          IF(B**2.GT.4*A*C) THEN
            IF(ABS(A).GT.(1.D-8)) THEN
              W1=(SQRT(1-4*A*C/B**2)-1)*B/(2*A)
            ELSE
              W1=-C/B-A*C**2/B**3
            ENDIF
            IF(ABS(X(2)+X(3)*W1).GT.ABS(Y(2)+Y(3)*W1)) THEN
              W2=(J0-X(0)-X(1)*W1)/(X(2)+X(3)*W1)
            ELSE
              W2=(I0-Y(0)-Y(1)*W1)/(Y(2)+Y(3)*W1)
            ENDIF
            IF((W1.GE.-EPS).AND.(W1.LE.1+EPS).AND.
     .         (W2.GE.-EPS).AND.(W2.LE.1+EPS)) THEN
              HUSE(I0,J0)=.TRUE.
              AL4=SNGL(W(0)+W(1)*W1+W(2)*W2+W(3)*W1*W2)
              REAL4=SNGL(Z(0)+Z(1)*W1+Z(2)*W2+Z(3)*W1*W2)
              IF(REAL4.GT.HT(I0,J0)) THEN
                HT(I0,J0)=REAL4
                AL(I0,J0)=AL4
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        ENDDO

      ENDDO
      ENDDO
      ENDDO

      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
      IF(.NOT.HUSE(I,J)) THEN
        HT(I,J)=0
        AL(I,J)=1
      ENDIF
      ENDDO
      ENDDO
                
      RETURN
      END

