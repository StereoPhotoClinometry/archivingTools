C$Procedure
 
      SUBROUTINE GET_MODEL(INFILE,Q,VEC,A0)

C$ Abstract
C     This routine will open a shape model file in ICQ format and read in
C     in the vector values and albedo data (if available).
C
C$ Disclaimer
C     None
C
C$ Required_Reading
C
C     R.W. Gaskell, et.al, "Characterizing and navigating small bodies
C           with imaging data", Meteoritics & Planetary Science 43,
C           Nr 6, 1049-1061 (2008)
C
C
C$ Declarations
 
      IMPLICIT NONE

C      REAL*8             VEC(3,0:512,0:512,6)
C      REAL*8             A0(0:512,0:512,6)
      REAL*8, allocatable:: VEC(:,:,:,:)
      REAL*8, allocatable:: A0(:,:,:)
      INTEGER*4          I, J, K, F, Q, N
      CHARACTER*72       INFILE
      CHARACTER*100      LINE

      LOGICAL            EX
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INFILE     I   Shape model (ICQ) filename 
C     Q          I   Number of shape model vectors
C     VEC        I   Shape model vectors array
C     A0         I   Albedo
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     INFILE (SHAPE MODEL)           I   ICQ formated shape model to be
C                                        read in
C
C$ Restrictions
C     None
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
C     AUTOREGISTERP
C     LIMBER
C     LITHOSP
C     DENSIFY
C     DENSIFYA
C     SHAPE_INFO
C     SUBROUTINE GET_HEIGHTS
C     SUBROUTINE GET_MODEL
C     SUBROUTINE MAXLEN
C     SUBROUTINE MM2VN
C     SUBROUTINE U2VN
C

      write (*,*) INFILE, Q
      write (*,*) A0
      write (*,*) "Vect: ", VEC (1,1,1,1)
C     Check to see if shape model file exists (3/19/13 - SPOC)
      INQUIRE(FILE=INFILE, EXIST=EX)
      IF(.NOT.EX) THEN
        WRITE(6,*) INFILE, ' does not exist.'
        RETURN
      ENDIF

C     Check to see if file contain albedos, i.e. N=4
      N=0
      OPEN(UNIT=49,FILE=INFILE,STATUS='OLD')
      READ(49,*)
      READ(49,FMT='(A100)') LINE
      DO K=2,100
        IF((LINE(K-1:K-1).NE.' ').AND.(LINE(K:K).EQ.' ')) N=N+1
      ENDDO
      CLOSE(UNIT=49)

C     Re-open shape model file and read vector values
      OPEN(UNIT=49,FILE=INFILE,STATUS='OLD')
      write (*,*) "post open"
      READ(49,*) Q
      DO F=1,6
        DO J = 0,Q
          DO I = 0,Q
            A0(I,J,F)=1.D0
      write (*,*) "pre read"
            READ(49,FMT='(A100)') LINE
      write (*,*) "post read", LINE
            IF(N.EQ.3) THEN
              READ(LINE,*) (VEC(K,I,J,F), K=1,3)
            ENDIF
            IF(N.EQ.4) THEN
              READ(LINE,*) (VEC(K,I,J,F), K=1,3), A0(I,J,F)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      CLOSE(UNIT=49)
      
      RETURN
      END

