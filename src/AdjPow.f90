SUBROUTINE adjpow()
! Given Asy. Power = J/Vol.
! Avg. Power = sum(Asy. Power) / # of Asy.
! ADJ : Each Asy. Power / Avg. Power
! DO NOT Integrate 3-D Assembly Power into 2-D Assembly Power nor 1-D Plane Power

USE param, ONLY : EPS7, ZERO
USE mdat,  ONLY : FNXY, l02, l3d, iedterr, nz, nxy, ndat, avghgt, hgt, pow3d, powxy, powax, objcn, plotobj

IMPLICIT NONE

INTEGER :: iobj, ixy, iz
REAL :: totpow, avgpow
! ------------------------------------------------

IF (iedterr .EQ. 2) RETURN

DO iobj = 1, 2
  IF (iobj.EQ.2 .AND. .NOT.l02) EXIT
  
  ! 3D
  totpow = sum(pow3d(:, :, iobj))
  IF (totpow .LT. EPS7) THEN
    pow3d(:, 1, iobj) = powxy(:, iobj)
    totpow = sum(pow3d(:, :, iobj))
  END IF
  
  avgpow = totpow / real(ndat(iobj))
  IF (.NOT. l3d) THEN
    pow3d(:, :, iobj) = pow3d(:, :, iobj) / avgpow
  ELSE
    DO iz = 1, nz
      pow3d(:, iz, iobj) = pow3d(:, iz, iobj)*avghgt / avgpow / hgt(iz) ! Volume-wise Power
    END DO
  END IF
  
  ! 2D
  totpow = sum(powxy(:, iobj))
  avgpow = totpow / real(nxy(iobj))
  powxy(:, iobj) = powxy(:, iobj) / avgpow
  
  ! 1D : Already Normalized
  DO iz = 1, nz
    powax(iz, iobj) = powax(iz, iobj)*hgt(iz) ! Point-wise to Volume-wise
  END DO
  
  totpow = sum(powax(:, iobj))
  avgpow = totpow / real(nz)
  DO iz = 1, nz
    powax(iz, iobj) = powax(iz, iobj)*avghgt / avgpow / hgt(iz) ! Volume-wise to Point-wise
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE adjpow