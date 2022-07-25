SUBROUTINE calpowerr_3D()

USE allocs
USE param, ONLY : ZERO, MP
USE mdat,  ONLY : lerr, lrel, l3d, powerr, ndat, xyzmax, xyzrms, xymax, xyrms, xyztotmax, xyztotrms, nz, nxy, avghgt, hgt, plotobj, pow3d

IMPLICIT NONE

INTEGER :: iz, ixy, jobj, mxy
REAL :: tot01, tot02, rnrm
! ------------------------------------------------

CALL dmalloc0(xyzmax, 0, nz)
CALL dmalloc0(xyzrms, 0, nz)

IF (.NOT. lerr) RETURN

mxy = nxy(plotobj)

CALL dmalloc0(powerr, 0, mxy, 0, nz)

jobj = MP(plotobj)
! ------------------------------------------------
!            01. CAL : Err.
! ------------------------------------------------
IF (lrel) THEN
  DO iz = 1, nz
    DO ixy = 1, mxy
      powerr(ixy, iz) = 100. * (pow3d(ixy, iz, plotobj) - pow3d(ixy, iz, jobj)) / pow3d(ixy, iz, jobj)
    END DO
  END DO
ELSE
  DO iz = 1, nz
    DO ixy = 1, mxy
      powerr(ixy, iz) = 100. * (pow3d(ixy, iz, plotobj) - pow3d(ixy, iz, jobj))
    END DO
  END DO
END IF
! ------------------------------------------------
!            02. SUMM.
! ------------------------------------------------
DO iz = 1, nz
  xyzmax(iz) = max(maxval(powerr(:, iz)), abs(minval(powerr(:, iz))))
  
  DO ixy = 1, mxy
    xyzrms(iz) = xyzrms(iz) + powerr(ixy, iz) * powerr(ixy, iz)
  END DO
  
  xyzrms(iz) = sqrt(xyzrms(iz) / real(mxy))
END DO

IF (.NOT. l3d) THEN
  xymax = xyzmax(1)
  xyrms = xyzrms(1)
ELSE
  xyztotmax = maxval(xyzmax)
  xyztotrms = ZERO
  
  DO iz = 1, nz
    DO ixy = 1, mxy
      xyztotrms = xyztotrms + powerr(ixy, iz) * powerr(ixy, iz)
    END DO
  END DO
  
  xyztotrms = sqrt(xyztotrms / real(ndat(plotobj)))
END IF
! ------------------------------------------------

END SUBROUTINE calpowerr_3D