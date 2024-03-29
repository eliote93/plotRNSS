SUBROUTINE calpowerr_3D()

USE allocs
USE param, ONLY : ZERO, MP, ERRABS, ERRREL
USE mdat,  ONLY : lerr, l3d, iedterr, powerr, ndat, xyzmax, xyzrms, xymax, xyrms, xyztotmax, xyztotrms, nz, nxy, avghgt, hgt, plotobj, pow3d, lptb

IMPLICIT NONE

INTEGER :: iz, ixy, jobj, mxy, ierr, nintact
REAL :: tot01, tot02, rat
! ------------------------------------------------

CALL dmalloc0(xyzmax, 0, nz, 1, 2)
CALL dmalloc0(xyzrms, 0, nz, 1, 2)

IF (.NOT. lerr) RETURN

jobj = MP (plotobj)
mxy  = nxy(plotobj)
CALL dmalloc0(powerr, 0, mxy, 0, nz, 1, 2)
! ------------------------------------------------
!            01. CAL : Err.
! ------------------------------------------------
SELECT CASE (iedterr)
CASE (2);     rat = 1.
CASE DEFAULT; rat = 100.
END SELECT

DO iz = 1, nz
  DO ixy = 1, mxy
    powerr(ixy, iz, ERRABS) = rat*(pow3d(ixy, iz, plotobj) - pow3d(ixy, iz, jobj))
    powerr(ixy, iz, ERRREL) = rat*(pow3d(ixy, iz, plotobj) - pow3d(ixy, iz, jobj)) / pow3d(ixy, iz, jobj)
  END DO
END DO
! ------------------------------------------------
!            02. SUMM.
! ------------------------------------------------
xyztotrms = ZERO
DO ierr = 1, 2
  DO iz = 1, nz
    xyzmax(iz, ierr) = max(maxval(powerr(:, iz, ierr)), abs(minval(powerr(:, iz, ierr))))
    
    DO ixy = 1, mxy
      xyzrms(iz, ierr) = xyzrms(iz, ierr) + powerr(ixy, iz, ierr)*powerr(ixy, iz, ierr)
    END DO
    
    xyzrms(iz, ierr) = sqrt(xyzrms(iz, ierr) / real(mxy))
    
    ! DEBUG : Exclude Deformed Asy.
    !xyzmax(iz, ierr) = ZERO
    !nintact = 0
    !
    !DO ixy = 1, mxy
    !  IF (lptb(ixy)) CYCLE
    !  nintact = nintact + 1
    !  
    !  xyzmax(iz, ierr) = max(xyzmax(iz, ierr), abs(powerr(ixy, iz, ierr)))
    !  xyzrms(iz, ierr) = xyzrms(iz, ierr) + powerr(ixy, iz, ierr)*powerr(ixy, iz, ierr)
    !END DO
    !
    !xyzrms(iz, ierr) = sqrt(xyzrms(iz, ierr) / real(nintact))
  END DO
  
  IF (.NOT. l3d) THEN
    xymax(ierr) = xyzmax(1, ierr)
    xyrms(ierr) = xyzrms(1, ierr)
  ELSE
    xyztotmax(ierr) = maxval(xyzmax(:, ierr))
    
    DO iz = 1, nz
      DO ixy = 1, mxy
        xyztotrms(ierr) = xyztotrms(ierr) + powerr(ixy, iz, ierr)*powerr(ixy, iz, ierr)
      END DO
    END DO
    
    xyztotrms(ierr) = sqrt(xyztotrms(ierr) / real(ndat(plotobj)))
  END IF
END DO
! ------------------------------------------------

END SUBROUTINE calpowerr_3D