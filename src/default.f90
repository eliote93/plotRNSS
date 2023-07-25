SUBROUTINE default()

USE param, ONLY : TRUE, FALSE, ONE, ZERO
USE mdat,  ONLY : nz, l02, xylmin, xylmax, zlmin, zlmax, aoF2F, ndfrm, lptb, iedterr, lbnch, nxy, ndat, nya

IMPLICIT NONE

nz      = 1
l02     = FALSE
xylmin  = 0
xylmax  = 1
zlmin   = 0
zlmax   = 1
aoF2F   = ZERO
ndfrm   = 0
lptb    = FALSE
iedterr = 0
lbnch   = FALSE

END SUBROUTINE default