C$ Procedure
 
      SUBROUTINE HGT2SLP(NTMP,QSZ,HUSE,HT, TUSE,TMPL)     

C$ Abstract
C
C     The first two components of the tmpl array, tmpl(1,i,j) and tmpl(2,i,j)
C     tmpl(2,i,j) represent the negative of the maplet slopes at i,j:
C
C        tmpl(i,j,1) = -dht(i,j)/dx ~ (ht(i,j-1)-ht(i,j+1)/2
C        tmpl(i,j,2) = -dht(i,j)/dy ~ (ht(i-1,j)-ht(i+1,j)/2
C    
C     Note that the discrete map coordinates are arrayed in i,j like
C
C                 ___________________ i (y)
C                 |
C                 |
C                 |
C                 |
C                 |
C                 |
C                j (x)
C
C     whereas the continuous coordinates x,y are arranged to make their normal
C     cross product point out of the paper in the direction of positive height.
C
C     If one of the heights adjacent to ht(i,j) is missing, then the subroutine 
C     steps through a heirarchy of approximations, each involving three points,
C     fits a quadratic to the points and extracts the slope at (i,j).  If none
C     of those work, due to missing data, the subroutine returns the slope from
C     a linear fit between (i,j) and a neighboring point.  Finally, if that 
C     doesn't work, it sets tmpl(i,j,k)=0 and returns a flag tuse(i,j)=.false.
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

      IMPLICIT NONE

      INTEGER*4       NTMP, QSZ, I, J
     
      REAL*4          TMPL(-NTMP:NTMP,-NTMP:NTMP,3), 
     .                HT(-NTMP:NTMP,-NTMP:NTMP) 

      LOGICAL         TUSE(-NTMP:NTMP,-NTMP:NTMP),
     .                HUSE(-NTMP:NTMP,-NTMP:NTMP) 
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NTMP
C     QSZ
C     HUSE
C     HT
C     TUSE
C     TMPL
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     None
C
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
C     None
C
C$ SPICELIB_functions_called
C     None
C
C$ SPICELIB_subroutines_called
C     None
C
C$ Called_by_SPC_Programs
C     AUTOREGISTER
C     LITHOS
C     REGISTER
C     SUBROUTINE COMPARE
C     SUBROUTINE EXTRACT_DATA_PIC
C     SUBROUTINE SHOW_SLOPES
C$ History
C     2014_02_02  Additional missing data interpolations added.
C

      DO I=-QSZ,QSZ
      DO J=-QSZ,QSZ
        TUSE(I,J)=.FALSE.
        TMPL(I,J,1)=0
        TMPL(I,J,2)=0
         IF(((HUSE(I+1,J).AND.HUSE(I-1,J)).OR.
     .       (HUSE(I,J).AND.HUSE(I-1,J)).OR. 
     .       (HUSE(I+1,J).AND.HUSE(I,J))).AND. 
     .      ((HUSE(I,J+1).AND.HUSE(I,J-1)).OR.
     .       (HUSE(I,J).AND.HUSE(I,J-1)).OR. 
     .       (HUSE(I,J+1).AND.HUSE(I,J)))) THEN 
          TUSE(I,J)=.TRUE.
          IF(HUSE(I+1,J).AND.HUSE(I-1,J)) THEN
            TMPL(I,J,2)=(HT(I-1,J)-HT(I+1,J))/2
            GO TO 10
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I-1,J).AND.HUSE(I+2,J)) THEN
            TMPL(I,J,2)=-(3*HT(I,J)-4*HT(I-1,J)+HT(I+2,J))/6
            GO TO 10
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I+1,J).AND.HUSE(I-2,J)) THEN
            TMPL(I,J,2)=+(3*HT(I,J)-4*HT(I+1,J)+HT(I-2,J))/6
            GO TO 10
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I-1,J).AND.HUSE(I-2,J)) THEN
            TMPL(I,J,2)=-(3*HT(I,J)-4*HT(I-1,J)+HT(I-2,J))/2
            GO TO 10
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I+1,J).AND.HUSE(I+2,J)) THEN
            TMPL(I,J,2)=+(3*HT(I,J)-4*HT(I+1,J)+HT(I+2,J))/2
            GO TO 10
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I-1,J)) THEN
            TMPL(I,J,2)=HT(I-1,J)-HT(I,J)
            GO TO 10
          ENDIF
          TMPL(I,J,2)=HT(I,J)-HT(I+1,J)
10        CONTINUE
          IF(HUSE(I,J+1).AND.HUSE(I,J-1)) THEN
            TMPL(I,J,1)=(HT(I,J-1)-HT(I,J+1))/2
            GO TO 20
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I,J-1).AND.HUSE(I,J+2)) THEN
            TMPL(I,J,1)=-(3*HT(I,J)-4*HT(I,J-1)+HT(I,J+2))/6
            GO TO 20
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I,J+1).AND.HUSE(I,J-2)) THEN
            TMPL(I,J,1)=+(3*HT(I,J)-4*HT(I,J+1)+HT(I,J-2))/6
            GO TO 20
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I,J-1).AND.HUSE(I,J-2)) THEN
            TMPL(I,J,1)=-(3*HT(I,J)-4*HT(I,J-1)+HT(I,J-2))/2
            GO TO 20
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I,J+1).AND.HUSE(I,J+2)) THEN
            TMPL(I,J,1)=+(3*HT(I,J)-4*HT(I,J+1)+HT(I,J+2))/2
            GO TO 20
          ENDIF
          IF(HUSE(I,J).AND.HUSE(I,J-1)) THEN
            TMPL(I,J,1)=HT(I,J-1)-HT(I,J)
            GO TO 20
          ENDIF
          TMPL(I,J,1)=HT(I,J)-HT(I,J+1)
20        CONTINUE
        ENDIF

      ENDDO
      ENDDO

      RETURN
      END

