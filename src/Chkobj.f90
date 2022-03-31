SUBROUTINE chkobj()

USE mdat, ONLY : lerr, nxa, nya, nxy, plotobj, izp

IMPLICIT NONE

INTEGER :: iya, idat
! ------------------------------------------------

IF (.NOT. lerr) RETURN

IF (nya(1) .NE. nya(2)) CALL terminate("DIFFERENT # OF 2-D ASY. (y)")

DO iya = 1, nya(1)
  IF (nxa(iya, 1) .NE. nxa(iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. (x)")
  
  DO idat = 1, 10 ! Fixed
    IF (izp(idat, iya, 1) .NE. izp(idat, iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. ZERO POWER (x)")
  END DO
END DO

IF (nxy(plotobj) .GT. 10000) CALL terminate("TOO MANY ASY.")
! ------------------------------------------------

END SUBROUTINE chkobj