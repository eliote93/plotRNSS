SUBROUTINE setgeo

USE param, ONLY : ZERO, SQ3, HALF, FILE1
USE mdat,  ONLY : aoF2F, asy2Dto1D, nxa, nya, cntxy, izp

IMPLICIT NONE

INTEGER :: iax, iay, ixy, icx, icy, mxa, ny
REAL :: aoPch, xx, yy, dx, dy
! ------------------------------------------------

asy2Dto1D = 0
cntxy     = ZERO

! SET : 1D to 2D map
ixy = 0

DO iay = 1, nya(FILE1)
  DO iax = 1, nxa(iay, FILE1)
    ixy = ixy + 1
    
    asy2Dto1D(iax, iay) = ixy
  END DO
END DO

! SET : Cnt.
aoPch = aoF2F / SQ3

icy = (nya(FILE1) + 1) / 2

DO iay = 1, nya(FILE1)
  yy = (icy - iay) * aoPch * 1.5
  
  mxa = nxa(iay, FILE1)
  
  SELECT CASE (mod(mxa, 2))
  CASE (0)
    icx = mxa / 2
    
    DO iax = 1, mxa
      ixy = asy2Dto1D(iax, iay)
      
      cntxy(1, ixy) = (iax - icx) * aoF2F - aoF2F * HALF
      cntxy(2, ixy) = yy
    END DO
  CASE (1)
    icx = (mxa + 1) / 2
    
    DO iax = 1, mxa
      ixy = asy2Dto1D(iax, iay)
      
      cntxy(1, ixy) = (iax - icx) * aoF2F
      cntxy(2, ixy) = yy
    END DO
  END SELECT
END DO

!dx    = 0.5 * aoF2F
!dy    = 1.5 * aoPch
!
!DO iay = 1, ndim
!  DO iax = 1, ndim
!    ixy = asy2Dto1D(iax, iay)
!    
!    cntxy(1, ixy) = af2f * (iax - nrow) - dx * (iay - nrow)
!    cntxy(2, ixy) =  -dy * (iay - nrow)
!  END DO
!END DO
! ------------------------------------------------

END SUBROUTINE setgeo