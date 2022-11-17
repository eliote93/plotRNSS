SUBROUTINE default()

USE param, ONLY : TRUE, FALSE, ONE, ZERO
USE mdat,  ONLY : nz, l02, xylim, zlim, aoF2F, ndfrm, lptb, iedterr

IMPLICIT NONE

nz      = 1
l02     = FALSE
xylim   = ONE
zlim    = ONE
aoF2F   = ZERO
ndfrm   = 0
lptb    = FALSE
iedterr = 0

END SUBROUTINE default