SUBROUTINE calpowerr_int()

USE param, ONLY : ZERO, MP
USE mdat,  ONLY : lerr, l3d, lrel, errmax, powerr, nz, nxy, ndat, xymax, xyrms, axmax, axrms, nz, hgt, avghgt, plotobj, powxy, powax

IMPLICIT NONE

INTEGER :: iz, ixy, mxy, jobj
REAL :: totmax, totrms, rnrm
! ------------------------------------------------

IF (.NOT. lerr) RETURN
IF (.NOT. l3d)  RETURN

jobj = MP (plotobj)
mxy  = nxy(plotobj)
! ------------------------------------------------
!            01. 3-D Err.
! ------------------------------------------------
totmax = maxval(errmax)
totrms = ZERO

DO iz = 1, nz
  DO ixy = 1, mxy
    totrms = totrms + powerr(ixy, iz) * powerr(ixy, iz)
  END DO
END DO

totrms = sqrt(totrms / real(ndat(plotobj)))

WRITE (*, '(A31, F5.2, X, A3)') '3-D Power Error Max. : ', totmax, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') '3-D Power Error RMS  : ', totrms, '(%)'
! ------------------------------------------------
!            02. 2-D Err.
! ------------------------------------------------
! CAL : Err.
IF (lrel) THEN
  DO ixy = 1, mxy
    powerr(ixy, 0) = 100. * (powxy(ixy, plotobj) - powxy(ixy, jobj)) / powxy(ixy, jobj)
  END DO
ELSE
  DO ixy = 1, mxy
    powerr(ixy, 0) = 100. * (powxy(ixy, plotobj) - powxy(ixy, jobj))
  END DO
END IF

! SUMM.
xymax = max(maxval(powerr(:, 0)), abs(minval(powerr(:, 0))))
xyrms = ZERO

DO ixy = 1, mxy
  xyrms = xyrms + powerr(ixy, 0) * powerr(ixy, 0)
END DO

xyrms = sqrt(xyrms / real(mxy))
! ------------------------------------------------
!            03. 1-D Err.
! ------------------------------------------------
! CAL : Err.
IF (lrel) THEN
  DO iz = 1, nz
    powerr(0, iz) = 100 * (powax(iz, plotobj) - powax(iz, jobj)) / powax(iz, jobj)
  END DO
ELSE
  DO iz = 1, nz
    powerr(0, iz) = 100 * (powax(iz, plotobj) - powax(iz, jobj))
  END DO
END IF

! SUMM.
axmax = max(maxval(powerr(0, :)), abs(minval(powerr(0, :))))
axrms = ZERO

DO iz = 1, nz
  axrms = axrms + powerr(0, iz) * powerr(0, iz)
END DO

axrms = sqrt(axrms / real(nz))
! ------------------------------------------------

END SUBROUTINE calpowerr_int