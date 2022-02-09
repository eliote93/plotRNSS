SUBROUTINE setgeo

USE param, ONLY : ZERO, SQ3, HALF, FILE1
USE mdat,  ONLY : aoF2F, asy1Dto2D, asy2Dto1D, nxa, nya, cntxy

IMPLICIT NONE

INTEGER :: ix, iy, ixy, icx, icy, mxa, ny

REAL :: aoPch, xx, yy
! ------------------------------------------------

asy1Dto2D = 0
asy2Dto1D = 0
cntxy     = ZERO

! SET : 1D to 2D map
ixy = 0

DO iy = 1, nya(FILE1)
  DO ix = 1, nxa(iy, FILE1)
    ixy = ixy + 1
    
    asy1Dto2D(1, ixy) = ix
    asy1Dto2D(2, ixy) = iy
    
    asy2Dto1D(ix, iy) = ixy
  END DO
END DO

! SET : Cnt.
aoPch = aoF2F / SQ3

icy = (nya(FILE1) + 1) / 2

DO iy = 1, nya(FILE1)
  yy = (icy - iy) * aoPch * 1.5
  
  mxa = nxa(iy, FILE1)
  
  SELECT CASE (mod(mxa, 2))
  CASE (0)
    icx = mxa / 2
    
    DO ix = 1, mxa
      ixy = asy2Dto1D(ix, iy)
      
      cntxy(1, ixy) = (ix - icx) * aoF2F - aoF2F * HALF
      cntxy(2, ixy) = yy
    END DO
  CASE (1)
    icx = (mxa + 1) / 2
    
    DO ix = 1, mxa
      ixy = asy2Dto1D(ix, iy)
      
      cntxy(1, ixy) = (ix - icx) * aoF2F
      cntxy(2, ixy) = yy
    END DO
  END SELECT
END DO
! ------------------------------------------------

END SUBROUTINE setgeo