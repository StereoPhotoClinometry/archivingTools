C$ Procedure
 
      FUNCTION LMCOUNT(PICNM,K)

C$ Abstract
C
C     This function will count the landmarks and limbmarks in the 
C     given image <PICNM> summary file.
C
C       If K=0 the routine counts landmarks in the image
C       If K=1 the routine counts landmarks + limbmarks in the image
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

      INTEGER               LMCOUNT      
      INTEGER               I
      INTEGER               K
      INTEGER               SLEN
            
      CHARACTER*12          PICNM
      CHARACTER*72          PICTFILE
      CHARACTER*80          LINE

      LOGICAL               EX
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PICNM      I   Image to count the landmarks.
C     K          I   K=0 count landmarks, K=1 count landmarks/limbmarks 
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     SUMFILES/<PICNM>.SUM           I   Image summary file
C
C$ Restrictions
C     PICTNM summary must exits.
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
C     SLEN
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
C     GEOMETRY
C     LITHOS
C     RESIDUALS
C     POLE
C     SUBROUTINE IMP2SCOBJPTG
C     SUBROUTINE LIMB_HEIGHTS
C
C$ Notes:
C     6/11/13:  Added check for summary file prior to opening.
C

      LMCOUNT=0
      I=SLEN(PICNM)
      PICTFILE='./SUMFILES/'//PICNM(1:I)//'.SUM'

C     Verify PICTFILE exists prior to continuing      
      INQUIRE(FILE=PICTFILE, EXIST=EX)
      IF(.NOT. EX) THEN
        WRITE(6,*)
        WRITE(6,*) '****************************************'
        WRITE(6,*) PICTFILE, 'does not exist ...'
        WRITE(6,*) '****************************************'
        WRITE(6,*)
        RETURN
      ENDIF  

      OPEN(UNIT=27,FILE=PICTFILE,STATUS='OLD')

C     Move to the landmark section      
10    CONTINUE       
      READ(27,FMT='(A80)') LINE
      IF(LINE(1:9).NE.'LANDMARKS') GO TO 10

C     Count the number of landmarks      
20    CONTINUE       
      READ(27,FMT='(A80)') LINE
      IF(LINE(1:9).NE.'LIMB FITS') THEN
        LMCOUNT=LMCOUNT+1
        GO TO 20
      ENDIF

C     If K=1 count the limbmarks also      
      IF(K.EQ.1) THEN
30      CONTINUE       
        READ(27,FMT='(A80)') LINE
        IF(LINE(1:8).NE.'END FILE') THEN
          LMCOUNT=LMCOUNT+1
          GO TO 30
        ENDIF
      ENDIF
      CLOSE(UNIT=27)
      
      RETURN
      END

