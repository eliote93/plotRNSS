SUBROUTINE calpowerr_int()
! INTEGRATE : 3-D into 2-D, 1-D

USE param, ONLY : ZERO, MP, ERRABS, ERRREL
USE mdat,  ONLY : lerr, l3d, plotobj, nz, nxy, ndat, nz, xyztotmax, xyztotrms, xyzmax, xyzrms, xymax, xyrms, axmax, axrms, powerr, hgt, avghgt, powxy, powax

IMPLICIT NONE

INTEGER :: iz, ixy, mxy, jobj, ierr
REAL :: rnrm
! ------------------------------------------------

IF (.NOT. lerr) RETURN
IF (.NOT. l3d)  RETURN

jobj = MP (plotobj)
mxy  = nxy(plotobj)
! ------------------------------------------------
!            01. 2-D Err.
! ------------------------------------------------
! CAL : Err.
DO ixy = 1, mxy
  powerr(ixy, 0, ERRABS) = 100. * (powxy(ixy, plotobj) - powxy(ixy, jobj))
  powerr(ixy, 0, ERRREL) = 100. * (powxy(ixy, plotobj) - powxy(ixy, jobj)) / powxy(ixy, jobj)
END DO

! SUMM.
xyrms = ZERO

DO ierr = 1, 2
  xymax(ierr) = max(maxval(powerr(:, 0, ierr)), abs(minval(powerr(:, 0, ierr))))
  
  DO ixy = 1, mxy
    xyrms(ierr) = xyrms(ierr) + powerr(ixy, 0, ierr) * powerr(ixy, 0, ierr)
  END DO
  
  xyrms(ierr) = sqrt(xyrms(ierr) / real(mxy))
  
  xyzmax(0, ierr) = xymax(ierr)
  xyzrms(0, ierr) = xyrms(ierr)
END DO
! ------------------------------------------------
!            03. 1-D Err.
! ------------------------------------------------
! CAL : Err.
DO iz = 1, nz
  powerr(0, iz, ERRABS) = 100 * (powax(iz, plotobj) - powax(iz, jobj))
  powerr(0, iz, ERRREL) = 100 * (powax(iz, plotobj) - powax(iz, jobj)) / powax(iz, jobj)
END DO

! SUMM.
axrms = ZERO

DO ierr = 1, 2
  axmax(ierr) = max(maxval(powerr(0, :, ierr)), abs(minval(powerr(0, :, ierr))))
  
  DO iz = 1, nz
    axrms(ierr) = axrms(ierr) + powerr(0, iz, ierr) * powerr(0, iz, ierr)
  END DO
  
  axrms(ierr) = sqrt(axrms(ierr) / real(nz))
END DO
! ------------------------------------------------

END SUBROUTINE calpowerr_int