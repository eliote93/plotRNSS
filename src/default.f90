SUBROUTINE default()

USE param, ONLY : TRUE, FALSE, ONE, ZERO
USE mdat,  ONLY : nz, l02, xylmin, xylmax, zlmin, zlmax, aoF2F, ndfrm, lptb, iedterr, lbnch, nxy, ndat, nya

IMPLICIT NONE

nz      = 1
l02     = FALSE
xylmin  = ZERO
xylmax  = ONE
zlmin   = ZERO
zlmax   = ONE
aoF2F   = ZERO
ndfrm   = 0
lptb    = FALSE
iedterr = 0
lbnch   = FALSE

END SUBROUTINE default