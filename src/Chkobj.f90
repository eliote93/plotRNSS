SUBROUTINE chkobj()

USE param, ONLY : EPS7, MP
USE mdat,  ONLY : lerr, nxa, nya, nxy, plotobj, izp, aoF2F, keff

IMPLICIT NONE

INTEGER :: iya, idat
! ------------------------------------------------

IF (aoF2F(1) .LT. EPS7)  CALL terminate("AOF2F IS NOT INPUTTED")

IF (.NOT. lerr) RETURN

IF (abs(aoF2F(1) - aoF2F(2)) .GT. 1E-4) CALL terminate("AOF2F IS DIFFERENT")
IF (nya(1) .NE. nya(2)) CALL terminate("DIFFERENT # OF 2-D ASY. (y)")

DO iya = 1, nya(1)
  IF (nxa(iya, 1) .NE. nxa(iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. (x)")
  
  DO idat = 1, 10 ! Fixed
    IF (izp(idat, iya, 1) .NE. izp(idat, iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. ZERO POWER (x)")
  END DO
END DO

IF (nxy(plotobj) .GT. 10000) CALL terminate("TOO MANY ASY.")

!IF (keff(1) .LT. EPS) CALL terminate("K-EFF")
!IF (keff(2) .LT. EPS) CALL terminate("K-EFF")
WRITE (*, '(18X, A18, F7.5)')       'Reference k-eff : ',       keff(MP(plotobj))
WRITE (*, '(18X, A18, F7.5)')       'Test      k-eff : ',       keff(plotobj)
WRITE (*, '(12X, A24, I5, X, A5/)') 'Reactivity Difference : ', int(100000.*(1./keff(MP(plotobj)) - 1./keff(plotobj))), '(pcm)'
! ------------------------------------------------

END SUBROUTINE chkobj