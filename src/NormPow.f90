SUBROUTINE normpow()

USE param, ONLY : EPS7, ZERO
USE mdat,  ONLY : FNXY, l02, l3d, iedterr, nz, nxy, ndat, avghgt, hgt, pow3d, powxy, powax

IMPLICIT NONE

INTEGER :: iobj, ixy, iz
REAL :: totpow, rnrm
! ------------------------------------------------

IF (iedterr .GT. 0) RETURN ! Unstr.

DO iobj = 1, 2
  IF (iobj.EQ.2 .AND. .NOT.l02) EXIT
  
  ! 3D
  totpow = sum(pow3d(:, :, iobj))
  IF (totpow .LT. EPS7) THEN
    pow3d(:, 1, iobj) = powxy(:, iobj)
    totpow = sum(pow3d(:, :, iobj))
  END IF
  
  rnrm = real(ndat(iobj)) / totpow
  IF (.NOT. l3d) THEN
    pow3d(:, :, iobj) = pow3d(:, :, iobj)*rnrm
  ELSE
    DO iz = 1, nz
      pow3d(:, iz, iobj) = pow3d(:, iz, iobj)*rnrm*avghgt / hgt(iz) ! Volume-wise Power
    END DO
  END IF
  
  ! 2D
  totpow = sum(powxy(:, iobj))
  rnrm   = real(nxy(iobj)) / totpow
  powxy(:, iobj) = powxy(:, iobj)*rnrm
  
  ! 1D : Already Normalized
  DO iz = 1, nz
    powax(iz, iobj) = powax(iz, iobj)*hgt(iz) ! Point-wise to Volume-wise
  END DO
  
  totpow = sum(powax(:, iobj))
  rnrm   = real(nz) / totpow
  DO iz = 1, nz
    powax(iz, iobj) = powax(iz, iobj)*rnrm*avghgt / hgt(iz) ! Volume-wise to Point-wise
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE normpow