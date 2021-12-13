SUBROUTINE calpowerr_3D()

USE allocs
USE param, ONLY : ZERO, MP
USE mdat,  ONLY : lerr, lrel, l3d, powerr, ndat, errplnmax, errplnrms, nz, nxy, avghgt, hgt, plotobj, pow3d

IMPLICIT NONE

INTEGER :: iz, ixy, jobj, mxy
REAL :: tot01, tot02, rnrm
! ------------------------------------------------

CALL dmalloc0(errplnmax, 0, nz)
CALL dmalloc0(errplnrms, 0, nz)

IF (.NOT. lerr) RETURN

mxy = nxy(plotobj)

CALL dmalloc0(powerr, 0, mxy, 0, nz)

jobj = MP(plotobj)
! ------------------------------------------------
!            01. DEBUG
! ------------------------------------------------
! Total Sum
!tot01 = sum(pow3d(:, :, 1))
!tot02 = sum(pow3d(:, :, 2))
!
! Print 3-D Power
!OPEN (41, FILE = 'tst.out')
!
!DO iz = 1, nz
!  DO ixy = 1, nxy(2)
!    WRITE (41, '(ES13.5)') pow3d(ixy, iz, 1)
!  END DO
!END DO
!
!CLOSE (41)
!STOP
! ------------------------------------------------
!            02. CAL : Err.
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
!            04. SUMMARIZE
! ------------------------------------------------
DO iz = 1, nz
  errplnmax(iz) = max(maxval(powerr(:, iz)), abs(minval(powerr(:, iz))))
  
  DO ixy = 1, mxy
    errplnrms(iz) = errplnrms(iz) + powerr(ixy, iz) * powerr(ixy, iz)
  END DO
  
  errplnrms(iz) = sqrt(errplnrms(iz) / real(mxy))
END DO

IF (.NOT. l3d) THEN
  errplnmax(0) = errplnmax(1)
  errplnrms(0) = errplnrms(1)
END IF
! ------------------------------------------------

END SUBROUTINE calpowerr_3D