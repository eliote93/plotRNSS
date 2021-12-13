SUBROUTINE plotpow()
! PLOT : inputted Power instead Power Error

USE allocs
USE mdat, ONLY : nz, nxy, plotobj, powerr, pow3d, powtotpf, powplnpf, powaxpf, powxy, powax, lrel, axpow, hgt, avghgt

IMPLICIT NONE

INTEGER :: iz, ixy, iobj
! ------------------------------------------------

CALL dmalloc0(powplnpf,  0, nz)
CALL dmalloc0(powerr,    0, nxy(plotobj), 0, nz)

! CnP
DO iz = 1, nz
  DO ixy = 1, nxy(plotobj)
    powerr(ixy, iz) = pow3d(ixy, iz, plotobj)
  END DO
  
  powplnpf(iz) = maxval(powerr(:, iz))
END DO

DO ixy = 1, nxy(plotobj)
  powerr(ixy, 0) = powxy(ixy, plotobj)
END DO

DO iz = 1, nz
  powerr(0, iz) = powax(iz, plotobj)
END DO

! P.F.
powtotpf    = maxval(powerr)
powplnpf(0) = maxval(powxy(:, plotobj))
powaxpf     = maxval(powax(:, plotobj))
! ------------------------------------------------

END SUBROUTINE plotpow