C$Procedure
 
      SUBROUTINE FIND_PICFILE(PICNM,PICFILE,EX)

C$ Abstract
C     This subroutine will search through directories
C         IMAGEFILES, IMAGEFILES1, IMAGEFILES2, IMAGEFILES3,  
C         IMAGEFILES4, IMAGEFILES5, IMAGEFILES6, IMAGEFILES7,
C         IMAGEFILES8, IMAGEFILES9 for the image file <PICNM> 
C         and returns the full path name to the file <PICFILE> and 
C         the logical variable <EX> indicating if it exists or not.
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
C$ Declarations
 
      IMPLICIT NONE
      
      INTEGER*4         I
      INTEGER*4         SLEN
      CHARACTER*12      PICNM
      CHARACTER*72      PICFILE
      LOGICAL           EX
 
C$ Variable_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PICNM      I   Name of picture image file to search for
C     PICFILE    O   Path and file name link to the picture image file
C                    if files exists
C     EX         O   Logical variable indicating if file exists or not
C
C$ File_I/O
C
C     Filename                      I/O  Description
C     ----------------------------  ---  -------------------------------
C     None  
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
C     SLEN
C
C$SPC_subroutines_called
C     None
C
C$SPICELIB_functions_called
C     None
C
C$SPICELIB_subroutines_called
C     None
C
C$ Called_by_SPC_Programs
C     AUTOREGISTER
C     AUTOREGISTERP
C     BLEMISHES
C     LIMBER
C     LITHOS
C     LITHOSP
C     RESIDUALS
C     SPHEREMAPSA
C     SUBROUTINE EXTRACT_DATA_PIC
C     SUBROUTINE FIND_PICFILE
C     SUBROUTINE LIMB_HEIGHTS
C     SUBROUTINE PICINPT
C

      I=SLEN(PICNM)
      PICFILE='./IMAGEFILES/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES1/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES2/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES3/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES4/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES5/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES6/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES7/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES8/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='./IMAGEFILES9/'//PICNM(1:I)//'.DAT'
      INQUIRE(FILE=PICFILE,EXIST=EX)
      IF(EX) RETURN
      PICFILE='NONE'

      RETURN
      END

