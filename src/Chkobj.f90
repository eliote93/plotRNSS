SUBROUTINE chkobj()

USE mdat, ONLY : lerr, nxa, nya, nxy, plotobj

IMPLICIT NONE

INTEGER :: iya
! ------------------------------------------------

IF (.NOT. lerr) RETURN

IF (nya(1) .NE. nya(2)) CALL terminate("DIFFERENT # OF 2-D ASY. (y)")

DO iya = 1, nya(1)
  IF (nxa(iya, 1) .NE. nxa(iya, 2)) CALL terminate("DIFFERENT # OF 2-D ASY. (x)")
END DO

IF (nxy(plotobj) .GT. 10000) CALL terminate("TOO MANY ASY.")
! ------------------------------------------------

END SUBROUTINE chkobj