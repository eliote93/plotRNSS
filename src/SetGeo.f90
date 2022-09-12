SUBROUTINE setgeo

USE param, ONLY : ZERO, SQ3, HALF, FILE1
USE mdat,  ONLY : aoF2F, asy2Dto1D, nxa, nya, nsfc, cntxy, izp

IMPLICIT NONE

INTEGER :: iax, jax, iay, ixy, icx, icy, mxa, nx, ny, idat, ist, mx
REAL :: aoPch, xx, yy, dx, dy
! ------------------------------------------------

asy2Dto1D = 0
cntxy     = ZERO

! SET : 1D to 2D map
ixy = 0

DO iay = 1, nya(FILE1)
  nx = nya(FILE1) -   abs(nsfc(FILE1) - iay)       ! Full Hex.
  mx = nxa(iay, FILE1) + izp(0, iay, FILE1) ! Inputted
  
  jax = max(1, iay - nsfc(FILE1) + 1) + (nx - mx)/2 - 1
  
  DO iax = 1, nxa(iay, FILE1)
    jax = jax + 1
    
    DO idat = 1, izp(0, iay, FILE1)
      IF (iax .EQ. izp(idat, iay, FILE1)) jax = jax + 1
    END DO
    
    ixy = ixy + 1
    
    asy2Dto1D(jax, iay) = ixy
  END DO
END DO

! SET : Cnt.
aoPch = aoF2F(1) / SQ3

dx = 0.5 * aoF2F(1)
dy = 1.5 * aoPch

DO iay = 1, nya(FILE1)
  DO iax = 1, nya(FILE1)
    ixy = asy2Dto1D(iax, iay)
    
    IF (ixy .LT. 1) CYCLE
    
    cntxy(1, ixy) = aof2f(1) * (iax - nsfc(FILE1)) - dx * (iay - nsfc(FILE1))
    cntxy(2, ixy) =     - dy * (iay - nsfc(FILE1))
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE setgeo