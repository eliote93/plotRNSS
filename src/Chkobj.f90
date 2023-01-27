SUBROUTINE chkobj()

USE param, ONLY : EPS7, MP, io3
USE mdat,  ONLY : FNXY, lerr, ldfrm, l3d, nxa, nya, nxy, plotobj, izp, aoF2F, keff, ndfrm, drho, iedterr

IMPLICIT NONE

INTEGER :: iya, idat
! ------------------------------------------------

! Deformation
ldfrm = ndfrm(plotobj) .GT. 0
IF (l3d .AND. ldfrm) CALL terminate("3D DEFORMATION IS NOT DEVELOPED SO FAR")

! Basics
IF (aoF2F(1) .LT. EPS7) CALL terminate("AOF2F IS NOT INPUTTED")
!IF (keff(1)  .LT. EPS7) CALL terminate("K-EFF")
IF (nxy(plotobj) .GT. FNXY) CALL terminate("TOO MANY ASY.")

! Comparison
IF (.NOT. lerr) RETURN

IF (abs(aoF2F(1) - aoF2F(2)) .GT. 1E-4) CALL terminate("AOF2F IS DIFFERENT")
IF (nya(1) .NE. nya(2)) CALL terminate("DIFFERENT # OF 2-D ASY. (y)")
!IF (keff(2) .LT. EPS7) CALL terminate("K-EFF")

DO iya = 1, nya(1)
  IF (nxa(iya, 1) .NE. nxa(iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. (x)")
  
  DO idat = 1, 10 ! Fixed
    IF (izp(idat, iya, 1) .NE. izp(idat, iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. ZERO POWER (x)")
  END DO
END DO

! EDIT
IF (iedterr .EQ. 2) THEN
  drho = int(keff(plotobj) - keff(MP(plotobj)))
  WRITE (*, '(18X, A17, I5)')       'Reference drho : ',    int(keff(MP(plotobj)))
  WRITE (*, '(18X, A17, I5)')       'Test      drho : ',    int(keff(plotobj))
  WRITE (*, '(12X, A18, I5, X, A5/)') 'drho Difference : ', drho, '(pcm)'
  
  WRITE (io3, '(18X, A17, I5)')       'Reference drho : ',    int(keff(MP(plotobj)))
  WRITE (io3, '(18X, A17, I5)')       'Test      drho : ',    int(keff(plotobj))
  WRITE (io3, '(12X, A18, I5, X, A5/)') 'drho Difference : ', drho, '(pcm)'
ELSE
  drho = int(100000.*(1./keff(MP(plotobj)) - 1./keff(plotobj)))
  WRITE (*, '(18X, A18, F7.5)')       'Reference k-eff : ',       keff(MP(plotobj))
  WRITE (*, '(18X, A18, F7.5)')       'Test      k-eff : ',       keff(plotobj)
  WRITE (*, '(12X, A24, I5, X, A5/)') 'Reactivity Difference : ', drho, '(pcm)'
  
  WRITE (io3, '(18X, A18, F7.5)')       'Reference k-eff : ',       keff(MP(plotobj))
  WRITE (io3, '(18X, A18, F7.5)')       'Test      k-eff : ',       keff(plotobj)
  WRITE (io3, '(12X, A24, I5, X, A5/)') 'Reactivity Difference : ', drho, '(pcm)'
END IF
! ------------------------------------------------

END SUBROUTINE chkobj