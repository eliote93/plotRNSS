SUBROUTINE adjmcpowlv()
! Given Asy. Power = J/Vol.
! Avg. Power = sum(Asy. Power) / # of Asy.
! ADJ : Each Asy. Power / Avg. Power
! DO NOT Integrate 3-D Assembly Power into 2-D Assembly Power nor 1-D Plane Power

USE param, ONLY : EPS7, ZERO
USE mdat,  ONLY : FNXY, l02, l3d, iedterr, nz, nxy, ndat, avghgt, hgt, pow3d, powxy, powax, objcn, plotobj, vol3d

IMPLICIT NONE

INTEGER :: iobj, ixy, iz
REAL :: totpow, avgpow, totvol, tst1, tst2
! ------------------------------------------------

tst1 = sum(powxy(:, 1))
tst2 = sum(powxy(:, 2))

IF (iedterr .EQ. 2) RETURN

DO iobj = 1, 2
  IF (iobj.EQ.2 .AND. .NOT.l02) CYCLE
  IF (objcn(iobj) .EQ. 'RN')    CYCLE ! RNSS is already Adjusted
  
  totpow = sum(pow3d(:, :, iobj))
  IF (totpow .LT. EPS7) pow3d(:, 1, iobj) = powxy(:, iobj)
  
  ! CAL : Avg. Pow
  totpow = ZERO
  totvol = ZERO
  DO iz = 1, nz
    DO ixy = 1, ndat(iobj)
      totpow = totpow + vol3d(ixy, iz, iobj)*pow3d(ixy, iz, iobj)
      totvol = totvol + vol3d(ixy, iz, iobj)
    END DO
  END DO
  
  avgpow = totpow / totvol
  
  ! Adj.
  pow3d(:, :, iobj) = pow3d(:, :, iobj) / avgpow
  powxy(:, iobj)    = powxy(:, iobj)    / avgpow
  powax(:, iobj)    = powax(:, iobj)    / avgpow
END DO

tst1 = sum(powxy(:, 1))
tst2 = sum(powxy(:, 2))
! ------------------------------------------------

END SUBROUTINE adjmcpowlv