C Eric E Palmer - 15 Aug 2023
C Global Coverage, update to coverage and designed to work like globalStat
C	Reports out how many landmarks over the surface
C  Reports (in matrix) the best GSD for that grid/box
C		Limb check was removed
C		This only displays maplets with images
C  Version 1.1


      implicit none

      integer*4       mx2
      parameter       (mx2=5000)

      INTEGER         KOUNT      
      integer*4       imax, jmax, imap, nmap
      integer*4       coverage(360,180), dcoverage(360,180)
      real*4          bestGSD(360,180), temp
      real*4          gsd

      real*8          vk(3,mx2,mx2), alb(mx2,mx2)

      real*8          v(3), covmin, covmax, ltd, lon, z, 
     .                UX(3), UY(3), UZ(3), KMSCALE, rpd


      integer*4       i, j, k, i0, j0, npx, nln 

      CHARACTER*1     ANS
      CHARACTER*6     NAME
      character*72    map(500000), infile, outfile
      character*90    line
      character*2048  cline

      logical         ex

      real            version 
      version = 1.1
      write (*,*) "Version:  ", version

      J=2

C     Read in list of maplets.  coverage.in superceeds
      INFILE='coverage.in'
      INQUIRE(FILE=INFILE, EXIST=EX)
      IF(.NOT.EX) INFILE='LMRKLIST.TXT'
      open (unit=15, file=INFILE,status='old')
        write (*,*) "Using file:  ", INFILE
        nmap=0
15      continue
        read(15,fmt='(A6)') NAME
        if(NAME(1:1).eq.'#') go to 15
        if(NAME(1:3).ne.'END') then
          if (j.eq.1) then
            k=KOUNT(NAME,1)
            if(k.eq.0) go to 15
          endif
          if (j.eq.2) then
            k=KOUNT(NAME,0)
            if(k.eq.0) go to 15
          endif
          nmap=nmap+1
          MAP(nmap)='./MAPFILES/'//NAME//'.MAP'
          go to 15
        endif
      close (unit=15)
      write(6,*) 
      write(6,*)'Nmap = ', nmap
      write(6,*)
      
c ......................................
c   find coverage
c ......................................

      write(6,*) 'Input scale min/max (km)'
      read(5,*) covmin, covmax
      covmin = covmin * 1000
      covmax = covmax * 1000

C     Set output to zero
      do i=1,360
      do j=1,180
        coverage(i,j)=0
        bestGSD=4286578688
      enddo
      enddo

C     ------------------ Loop over all maplets
      do imap=1,nmap
        write(6,fmt='(a6)') map(imap)(12:17)
        CALL GET_DATA(MAP(IMAP),MX2,IMAX,JMAX,V,UX,UY,UZ,VK,ALB,KMSCALE)
c        IF((KMSCALE.GE.COVMIN).AND.(KMSCALE.LE.COVMAX)) THEN
         gsd = KMSCALE * 1000
          open (unit=10, file=map(imap), status='old')
 
C           set flag to 0
            do i=1,360
            do j=1,180
              dcoverage(i,j)=0
            enddo
            enddo

C           Loop over the full maplet grid
            do j0=1,jmax
            do i0=1,imax
            if(alb(i0,j0).gt.(0.005)) then
            call reclat(vk(1,i0,j0),z,lon,ltd)
            ltd=ltd/rpd()
            lon=lon/rpd()

C           Check boundaries
            if(lon .lt. 0) lon=lon+360
            if(lon .gt. 360) lon=lon-360
            i = 1 + aint(lon)
            j = aint( (90 - ltd ) +.99999999)


            if (i .lt. 1) write (*,*) "Err i: ", map(imap), i,j, lon
            if (j .lt. 1) write (*,*) "Err i: ", map(imap), i,j, ltd
            if (i .gt. 360) write (*,*) "Err j: ", map(imap), i,j, lon
            if (j .gt. 180) write (*,*) "Err j: ", map(imap), i,j, ltd

C           Set flag that this box has coverage
C           Set the best resolution
            if ( (gsd .ge. covmin) .and. (gsd .le. covmax) ) then
              dcoverage(i,j)=1
              if (bestGSD(i,j) .gt. gsd) then
                 bestGSD(i,j) = gsd
              endif
            endif
          endif
            enddo
            enddo
          close(unit=10)

C         for each grid element, add if there is coverage
          do i=1,360
          do j=1,180
            coverage(i,j)=coverage(i,j)+dcoverage(i,j)
          enddo
          enddo

C     end loop over maplet
      enddo

C     Output the data
      outfile='global-map_cov.grid.txt'
      open (unit=11, file=outfile)

        do j=1,180
          do i=1,360
            write (11, 96, advance="no") coverage(i,j)
          enddo
          write(11, *)
        enddo
      close (11)
 96   format (i7)
      write(6,*) 
      write(6,*) 'Global Coverage of maplets done.  Output: ', outfile

C     Get the min value for padding the poles
      gsd = 9999
      do j=1,180
        gsd = min (gsd, bestGSD(j,180))
      enddo

C     Output the data
      outfile='global-map_GSD.grid.txt'
      open (unit=11, file=outfile)

        do j=1,180
          do i=1,360

C           Have no data, so interpolate data to other points
c            if (bestGSD(i,j) .gt. 9990) then
c              bestGSD(i,j) = gsd
c            endif
            write (11, 97, advance="no") bestGSD(i,j)
          enddo
          write(11, *)
        enddo
      close (11)
 97   format (f12.5)
      write(6,*) 'Global Coverage of maplets done.  Output: ', outfile



C     Dump to a temp file
      open (unit=10, file='coverage-cov.gray', access='direct', 
     .      recl=360, status='unknown')
        do j=1,180
          do i=1,360
            cline(i:i)=char(coverage(i,j))
          enddo
          write(10,rec=j) cline(1:360)
        enddo
      close(unit=10)

C     Convert to pgm
      npx=360
      nln=180
      infile='coverage-cov.gray'
      outfile='coverage_g.pgm'
      call raw2pgm(infile,outfile,npx,nln)
      write(6,*) 'gc coverage_g.pgm'
      open (unit=10, file='coverage-cov.gray', status='unknown')
      close(unit=10, status='delete')

      stop
      end

c  ..............................................
      subroutine raw2pgm(infile,outfile,npx,nln)
c  ..............................................

      implicit none
      
      integer*4       npx, nln, k, k1, k2
      character*10    line
      character*50    header
      character*72    infile, outfile
      character*5000  cline

      
      header(1:2)='P5'
      header(3:3)=char(10)
      header(4:5)='#.'
      header(6:6)=char(10)
      k=7
      write(line,fmt='(i10)') npx
      do k1=1,10
      if(line(k1:k1).ne.' ') then
        header(k:k+10-k1) = line(k1:10)
        k=k+11-k1
        go to 10
      endif
      enddo
10    continue
      header(k:k)=' '
      k=k+1
      write(line,fmt='(i10)') nln
      do k1=1,10
      if(line(k1:k1).ne.' ') then
        header(k:k+10-k1) = line(k1:10)
        k=k+11-k1
        go to 20
      endif
      enddo
20    continue
      header(k:k)=char(10)
      k=k+1
      header(k:k+2)='255'
      k=k+3
      header(k:k)=char(10)
      
      open(unit=20, file=outfile, recl=1, access='direct', 
     .     status='unknown')
      open(unit=10, file=infile, recl=npx, access='direct', 
     .     status='old')
        do k1=1,k
          write(20,rec=k1) header(k1:k1)
        enddo
        do k1=1,nln
          read(10,rec=k1) cline(1:npx)
          do k2=1,npx
            write(20,rec=k+npx*(k1-1)+k2) cline(k2:k2)
          enddo
        enddo
      close(unit=10)
      close(unit=20)
      
      return
      end

c  ..............................................
      SUBROUTINE GET_DATA(INFILE,MX,IMAX,JMAX,V,UX,UY,UZ,VK,ALB,KMSCALE)
c  ..............................................

      IMPLICIT NONE
          
      INTEGER                  MX

      DOUBLE PRECISION         VK(3,MX,MX)
      DOUBLE PRECISION         ALB(MX,MX)
      DOUBLE PRECISION         V(3)
      DOUBLE PRECISION         UX(3)
      DOUBLE PRECISION         UY(3)
      DOUBLE PRECISION         UZ(3)
      DOUBLE PRECISION         KMSCALE
      DOUBLE PRECISION         HSCALE
      DOUBLE PRECISION         HT
          
      INTEGER                  QSZ
      INTEGER                  IMAX
      INTEGER                  JMAX
      INTEGER                  I0
      INTEGER                  J0
      INTEGER                  K0
      INTEGER                  I
      INTEGER                  J
      INTEGER                  K
      INTEGER                  IX(24)
      INTEGER                  JX(24)
      INTEGER                  NREC
          
      CHARACTER*72             INFILE
      CHARACTER*72             INLINE

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

      OPEN(UNIT=20, FILE=INFILE, ACCESS='DIRECT', RECL=72, status='OLD')
        READ(20,REC=1) INLINE
        CH4F=INLINE(7:10)
        call flip(4,lflag,ch4f,ch4)
        KMSCALE=RL4
        QSZ=ICHAR(INLINE(11:11))
     .     +ICHAR(INLINE(12:12))*256   !!!*
        IMAX=2*QSZ+1
        JMAX=2*QSZ+1
        I0=QSZ+1
        J0=QSZ+1
        DO K=1,3
          CH4F=INLINE(12+4*K:15+4*K)
          call flip(4,lflag,ch4f,ch4)
          V(K)=RL4
          CH4F=INLINE(24+4*K:27+4*K)
          call flip(4,lflag,ch4f,ch4)
          UX(K)=RL4
          CH4F=INLINE(36+4*K:39+4*K)
          call flip(4,lflag,ch4f,ch4)
          UY(K)=RL4
          CH4F=INLINE(48+4*K:51+4*K)
          call flip(4,lflag,ch4f,ch4)
          UZ(K)=RL4
        ENDDO
        CH4F=INLINE(64:67)
        call flip(4,lflag,ch4f,ch4)
        HSCALE=RL4
        NREC=1        
        K=0
        do j=1,JMAX
        do i=1,IMAX
          K=K+1
          IX(K)=I
          JX(K)=J
          IF(K.EQ.24) THEN
            NREC=NREC+1
            READ(20,REC=NREC) INLINE
            DO K=1,24
              CH1=INLINE(3*K:3*K)
              CH2F=INLINE(3*K-2:3*K-1)
              call flip(2,lflag,ch2f,ch2)
              HT=HSCALE*IX2
              vk(1,IX(K),JX(K))=V(1)
     .       +KMSCALE*((IX(K)-I0)*UY(1)+(JX(K)-J0)*UX(1)+HT*UZ(1))
              vk(2,IX(K),JX(K))=V(2)
     .       +KMSCALE*((IX(K)-I0)*UY(2)+(JX(K)-J0)*UX(2)+HT*UZ(2))
              vk(3,IX(K),JX(K))=V(3)
     .       +KMSCALE*((IX(K)-I0)*UY(3)+(JX(K)-J0)*UX(3)+HT*UZ(3))
              ALB(IX(K),JX(K))=0.01*ICHAR(CH1)
            ENDDO
            K=0
          ENDIF
        ENDDO
        ENDDO
        IF(K.NE.0) THEN
          K0=K
          NREC=NREC+1
          READ(20,REC=NREC) INLINE
          DO K=1,K0
            CH1=INLINE(3*K:3*K)
            CH2F=INLINE(3*K-2:3*K-1)
            call flip(2,lflag,ch2f,ch2)
            HT=HSCALE*IX2
            vk(1,IX(K),JX(K))=V(1)
     .     +KMSCALE*((IX(K)-I0)*UY(1)+(JX(K)-J0)*UX(1)+HT*UZ(1))
            vk(2,IX(K),JX(K))=V(2)
     .     +KMSCALE*((IX(K)-I0)*UY(2)+(JX(K)-J0)*UX(2)+HT*UZ(2))
            vk(3,IX(K),JX(K))=V(3)
     .     +KMSCALE*((IX(K)-I0)*UY(3)+(JX(K)-J0)*UX(3)+HT*UZ(3))
            ALB(IX(K),JX(K))=0.01*ICHAR(CH1)
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
      
c   ..................................................
      FUNCTION KOUNT(LMKNM,K)
c   ..................................................

      IMPLICIT NONE

      INTEGER               KOUNT      
      INTEGER               I
      INTEGER               K
            
      CHARACTER*6           LMKNM
      CHARACTER*72          LMRKFILE
      CHARACTER*80          LINE

      KOUNT=0
      LMRKFILE='./LMKFILES/'//LMKNM//'.LMK'
      OPEN(UNIT=27,FILE=LMRKFILE,STATUS='OLD')
10     CONTINUE       
        READ(27,FMT='(A80)') LINE
        IF(LINE(1:8).NE.'PICTURES') GO TO 10
20      CONTINUE       
        READ(27,FMT='(A80)') LINE
        IF(LINE(1:12).NE.'MAP OVERLAPS') THEN
          KOUNT=KOUNT+1
          GO TO 20
        ENDIF
30      CONTINUE       
        READ(27,FMT='(A80)') LINE
        IF(LINE(1:9).NE.'LIMB FITS') GO TO 30
        IF(K.EQ.1) THEN
40      CONTINUE       
        READ(27,FMT='(A80)') LINE
        IF(LINE(1:8).NE.'END FILE') THEN
          KOUNT=KOUNT+1
          GO TO 40
        ENDIF
        ENDIF
      CLOSE(UNIT=27)
      
      RETURN
      END


