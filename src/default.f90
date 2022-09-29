SUBROUTINE default()

USE param, ONLY : FALSE, ONE, ZERO
USE mdat,  ONLY : nz, l02, xylim, zlim, aoF2F, ndfrm, lptb

IMPLICIT NONE

nz    = 1
l02   = FALSE
xylim = ONE
zlim  = ONE
aoF2F = ZERO
ndfrm = 0
lptb  = FALSE

END SUBROUTINE default