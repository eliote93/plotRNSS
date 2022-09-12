SUBROUTINE plotpow()
! PLOT : inputted Power instead Power Error

USE allocs
USE mdat, ONLY : nz, nxy, plotobj, powerr, pow3d, xyztotpf, xyzpf, axpf, powxy, powax, axpow, hgt, avghgt

IMPLICIT NONE

INTEGER :: iz, ixy, iobj
! ------------------------------------------------

CALL dmalloc0(xyzpf, 0, nz)
CALL dmalloc0(powerr,   0, nxy(plotobj), 0, nz, 1, 1)

! CnP
DO iz = 1, nz
  DO ixy = 1, nxy(plotobj)
    powerr(ixy, iz, 1) = pow3d(ixy, iz, plotobj)
  END DO
  
  xyzpf(iz) = maxval(powerr(:, iz, 1))
END DO

DO ixy = 1, nxy(plotobj)
  powerr(ixy, 0, 1) = powxy(ixy, plotobj)
END DO

DO iz = 1, nz
  powerr(0, iz, 1) = powax(iz, plotobj)
END DO

! P.F.
xyztotpf    = maxval(powerr)
xyzpf(0) = maxval(powxy(:, plotobj))
axpf     = maxval(powax(:, plotobj))
! ------------------------------------------------

END SUBROUTINE plotpow