SUBROUTINE plotpow()
! PLOT : inputted Power instead Power Error

USE allocs
USE mdat, ONLY : nz, nxy, plotobj, powerr, pow3d, powpf, totpf, xypf, axpf, powxy, powax, lrel, axpow, hgt, avghgt

IMPLICIT NONE

INTEGER :: iz, ixy, iobj
! ------------------------------------------------

CALL dmalloc(powpf, nz)
CALL dmalloc0(powerr, 0, nxy(plotobj), 0, nz)

! CnP
DO iz = 1, nz
  DO ixy = 1, nxy(plotobj)
    powerr(ixy, iz) = pow3d(ixy, iz, plotobj)
  END DO
  
  powpf(iz) = maxval(powerr(:, iz))
END DO

DO ixy = 1, nxy(plotobj)
  powerr(ixy, 0) = powxy(ixy, plotobj)
END DO

DO iz = 1, nz
  powerr(0, iz) = powax(iz, plotobj)
END DO

! P.F.
totpf = maxval(powerr)

WRITE (*, '(A27, F7.2)') '3-D Power Peaking Factor : ', totpf

xypf = maxval(powxy(:, plotobj))
axpf = maxval(powax(:, plotobj))
! ------------------------------------------------

END SUBROUTINE plotpow