
      IMPLICIT NONE

      INTEGER           NTMP
      PARAMETER        (NTMP=2501)

      DOUBLE PRECISION  SCALE
      DOUBLE PRECISION  V(3)
      DOUBLE PRECISION  UX(3)
      DOUBLE PRECISION  UY(3)
      DOUBLE PRECISION  UZ(3)
      DOUBLE PRECISION  Z0, Z1, Z2
      DOUBLE PRECISION  X, Y
      DOUBLE PRECISION  C(0:3)

      REAL*4                HT(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                ALB(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                G(-NTMP:NTMP,-NTMP:NTMP)
      REAL*4                H(-NTMP:NTMP)
      REAL*4                DN(-NTMP:NTMP,NTMP)
      REAL*4                myHeight
      REAL*4                mi, ma, width, ratio
      REAL*8     radius, theta, nrewR, distance, height, newR


      INTEGER            I, I1
      INTEGER            J, J1
      INTEGER            IC, IL
      INTEGER            JC, JL
      INTEGER            K, L
      INTEGER            QSZ
      INTEGER            Q, Q0, Q1, maxIndex
      INTEGER            distX, distY, craterX, craterY, iDist
      REAL*4             leftHeight, rightHeight, m, b, dist, flag
      REAL*4             eqn, delta, maxDelta

      CHARACTER*2        UNITS
      CHARACTER*6        MAPNM
      CHARACTER*72       MAPFILE
      CHARACTER*72       INFILE
      CHARACTER*72       OUTFILE
      CHARACTER*5000     LINE

      LOGICAL            GUSE(-NTMP:NTMP,-NTMP:NTMP)
      write (*,*)       "Version 1.0"

      WRITE(6,*) 'Input mapname'
      READ(5,FMT='(A6)') MAPNM

      MAPFILE='MAPFILES/'//MAPNM//'.MAP'
      CALL READ_MAP(MAPFILE,NTMP,QSZ,SCALE,V,UX,UY,UZ,HT,ALB)
      radius = sqrt (v(1)**2 + v(2)**2 + v(3)**2)
      width = (QSZ*2+1) * SCALE    
      ratio = width / radius
      write (*,*) "Radius: ", radius, " km"
      write (*,*) "Bigmap width: ", width, " km"
      write (*,*) "Width to radius ratio: ", ratio

      if (SCALE .LT. .01) then
         SCALE = SCALE * 1000
         write (*,*) "Units will be in meters"
         UNITS = "m"
      else
         write (*,*) "Units will be in km"
         UNITS = "km"
      endif
      write (*,*) "Scale: ", SCALE, UNITS

C     Adjust to height values -- units km (native)

      if (ratio .GT. .2) then
         write (*,*) "Scaling for curvature"
      
         DO I=-QSZ,QSZ
         DO J=-QSZ,QSZ
             height = HT(I,J) * SCALE
   
C         Get distance
             X = I * SCALE 
             Y = J * SCALE 
             distance = sqrt ( X*X + Y*Y )
   
C         Compute new radius at position i, j
             theta = datan2 (distance, radius + height)
             newR = distance / dsin (theta)
   
C         If the angle is small, use the radius without correction
             if ( theta .LT. .01 )  then
                newR = radius
             endif
   
          HT (I,J) = newR
         ENDDO
         ENDDO

      else
         write (*,*) "Considering it flat"
         DO I=-QSZ,QSZ
         DO J=-QSZ,QSZ
            height = HT(I,J) * SCALE
            HT (I,J) = height
         ENDDO
         ENDDO
      endif
   
      WRITE(6,*) 'Input center'
      READ(5,*) IC, JC

      WRITE(6,*) 'Input left'
      READ(5,*) IL, JL


C     Compute the radius components and rim positions
      distX = NINT ((IC - IL) / 2.0)
      distY = NINT ((JC - JL) / 2.0)

      craterX = IC - distX
      craterY = JC - distY
      leftHeight = HT(craterX - QSZ, craterY - QSZ)
      write (*,*) "                      X           Y      Height"
      write (*,*) "Start Rim:  ", craterX, craterY, leftHeight, UNITS

      craterX = IC + distX
      craterY = JC + distY
      rightHeight = HT(craterX - QSZ, craterY - QSZ)
      write (*,*) "End Rim:    ", craterX, craterY, rightHeight, UNITS

      Z1=0
      Z2=0
      mi = 9999
      ma = -9999

C     Pick a size for the subset of the image
C     Twice the longest 
      Q0=2*MAX(ABS(IC-IL), ABS(JC-JL))
      DO I=-Q0,Q0
      DO J=-Q0,Q0

C       G is image, but it also is a slopemap (fake shaded relief)
        G(I,J)=0.E0
        GUSE(I,J)=.FALSE.
        IF((I+IC-QSZ-1.GE.-QSZ).AND.(I+IC-QSZ.LE.QSZ).AND.
     .      (J+JC-QSZ.GE.-QSZ).AND.(J+JC-QSZ.LE.QSZ)) THEN
          G(I,J)=HT(I+IC-QSZ-1,J+JC-QSZ)-HT(I+IC-QSZ,J+JC-QSZ)
          Z1=MIN(G(I,J),Z1)
          Z2=MAX(G(I,J),Z2)
          mi = min (HT(I+IC-QSZ-1,J+JC-QSZ), mi)
          ma = max (HT(I+IC-QSZ-1,J+JC-QSZ), ma)
          GUSE(I,J)=.TRUE.
        ENDIF
      ENDDO
      ENDDO
  

c      write (*,*) "Height at left ", HT(IL-QSZ, JL-QSZ)
c      write (*,*) "Height at center is ", HT(IC-QSZ, JC-QSZ)
c      write (*,*) "Height right ",HT(IC + ABS(IL-IC)-QSZ, JC 
c     +                           + ABS (JL-JC)-QSZ)
c      write (*,*) "Min/Max is ", mi, ma


C     Scale the image
      DO I=-Q0,Q0
      DO J=-Q0,Q0
      IF(GUSE(I,J)) THEN
        G(I,J)=REAL((G(I,J)-Z1)/(Z2-Z1))
      ENDIF
      ENDDO
      ENDDO

C     Distance of the segment and an integer of distance
      Z0=2*SQRT(1.D0*((IC-IL)**2+(JC-JL)**2))
      Q1=NINT(Z0)

      Z1= 1.D10
      Z2=-1.D10
      DO K=-Q1,Q1
        X=IC+K*(IC-IL)/Z0-0.5                                           Exact position of element K in X,Y
        Y=JC+K*(JC-JL)/Z0
        I=NINT(X)
        J=NINT(Y)
        G(I-IC,J-JC)=1                                                  white strip
        I=INT(X)
        J=INT(Y)
        X=X-I
        Y=Y-J
        L=-1
        DO J1=0,1
        DO I1=0,1
          L=L+1
          C(L)=HT(I+I1-QSZ, J+J1-QSZ)
        ENDDO
        ENDDO

C       Calculate the averaged height 
        C(3)=C(0)-C(1)-C(2)+C(3)
        C(1)=C(1)-C(0)
        C(2)=C(2)-C(0)
        H(K)=REAL(C(0)+C(1)*X+C(2)*Y+C(3)*X*Y) 
        Z1=MIN(Z1,H(K))
        Z2=MAX(Z2,H(K))
      ENDDO
c      write (*,*) "Other min/max", Z1, Z2

C     Calculates the range of verticle with a x5 exageration
      K=INT(5*(Z2-Z1))+1

C     Get which figure is wider
      Q=MAX(Q0,Q1)

C     Blank it out
      DO I=-Q,Q
        DO J=1,2*Q0+20+K
          DN(I,J)=0
        ENDDO
      ENDDO

C     Fill height with G
      DO I=-Q0,Q0
      DO J=1,2*Q0+1
        DN(I,J)=G(I,J-Q0-1)
      ENDDO
      ENDDO

C     Set index as a function of position
C     Set brightness to 1 at that position
      DO I=-Q1,Q1
        J=NINT(5*(Z2-H(I)))+2*Q0+10+1
        DN(I,J)=1
      ENDDO

C     Compute the line, use left side which has 0 for x
C            X position will need an offset for plotting 
      dist = Q1/2.0
      iDist = NINT (dist)
      rightHeight = H(iDist)
      leftHeight = H(-iDist)

C     Adjust to be width (vs radius)
      write (*,*) "Pixels: ", dist*2
      dist = dist * 2 * SCALE
      write (*,*) "Dist, height (Start/Stop): ", 
     +             dist, leftHeight, rightHeight, UNITS

C     Compute the equation
      m = (rightHeight - leftHeight) / dist
      b = leftHeight - m * 0
      write (*,*) "Eqn: y = ", m, "*X + ", b

C     Find the max
      maxDelta = 0
      DO I=-iDist/2,iDist/21
        myHeight=H(I)
        eqn = m * (I + dist) + b
        delta = eqn - myHeight

        if ( maxDelta .LT. delta) then
           maxDelta = delta
           maxIndex = I
        endif
      ENDDO
      write (*,*) "Maximum found : ", maxDelta, UNITS, 
     +           " at index ",maxIndex+Q1
      write (*,*) "D/d:  ", dist / maxDelta

C     Output the profile in ASCII
      OPEN(UNIT=20, FILE="profile.txt")
      DO I=-Q1,Q1
        myHeight=H(I)
        eqn = m * (I + dist) + b

C       Logic for showing crater sides, center and max
        flag = 0
        if ( I .EQ. iDist ) flag = 1
        if ( I .EQ. -iDist ) flag = 1
        if ( I .EQ. 0 ) flag = 1
        if ( I .EQ. maxIndex ) flag = 1

C       Print all three if interesting, otherwise made 3rd field blank
        if ( I .EQ. maxIndex ) then
                 write(20,*)myHeight, eqn, eqn, myHeight
        else
             if ( flag .EQ. 1) then
                   write(20,*)myHeight, eqn, myHeight, " # "
             else 
                   write(20,*)myHeight, eqn, " # ", " # "
             endif
         endif
      ENDDO
      CLOSE(UNIT=20)


C     Output
      INFILE='TEMPFILE.GRAY'
      OPEN(UNIT=10, FILE=INFILE, ACCESS='DIRECT',
     .     RECL=2*Q+1, STATUS='UNKNOWN')
        DO J=1,2*Q0+20+K
          DO I=-Q,Q
            LINE(I+Q+1:I+Q+1)=CHAR(NINT(255*DN(I,J)))
          ENDDO
          WRITE(10,REC=J) LINE(1:2*Q+1)
        ENDDO
      CLOSE(UNIT=10)

 240  format (f14.5)

      OUTFILE='TEMPFILE.pgm'

      CALL RAW2PGM(INFILE,OUTFILE,2*Q+1,2*Q0+20+K)
      OPEN(UNIT=63, FILE=INFILE, STATUS='OLD')
      CLOSE(UNIT=63, STATUS='DELETE')

      STOP
      END


