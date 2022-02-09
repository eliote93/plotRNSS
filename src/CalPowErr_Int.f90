SUBROUTINE calpowerr_int()
! INTEGRATE : 3-D into 2-D, 1-D

USE param, ONLY : ZERO, MP
USE mdat,  ONLY : lerr, l3d, lrel, plotobj, nz, nxy, ndat, nz, errtotmax, errtotrms, errplnmax, errplnrms, erraxmax, erraxrms, powerr, hgt, avghgt, powxy, powax

IMPLICIT NONE

INTEGER :: iz, ixy, mxy, jobj
REAL :: rnrm
! ------------------------------------------------

IF (.NOT. lerr) RETURN
IF (.NOT. l3d)  RETURN

jobj = MP (plotobj)
mxy  = nxy(plotobj)
! ------------------------------------------------
!            01. 3-D Err.
! ------------------------------------------------
errtotmax = maxval(errplnmax)
errtotrms = ZERO

DO iz = 1, nz
  DO ixy = 1, mxy
    errtotrms = errtotrms + powerr(ixy, iz) * powerr(ixy, iz)
  END DO
END DO

errtotrms = sqrt(errtotrms / real(ndat(plotobj)))
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
errplnmax(0) = max(maxval(powerr(:, 0)), abs(minval(powerr(:, 0))))
errplnrms(0) = ZERO

DO ixy = 1, mxy
  errplnrms(0) = errplnrms(0) + powerr(ixy, 0) * powerr(ixy, 0)
END DO

errplnrms(0) = sqrt(errplnrms(0) / real(mxy))
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
erraxmax = max(maxval(powerr(0, :)), abs(minval(powerr(0, :))))
erraxrms = ZERO

DO iz = 1, nz
  erraxrms = erraxrms + powerr(0, iz) * powerr(0, iz)
END DO

erraxrms = sqrt(erraxrms / real(nz))
! ------------------------------------------------

END SUBROUTINE calpowerr_int