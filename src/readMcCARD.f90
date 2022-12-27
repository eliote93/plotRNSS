! --------------------------------------------------------------------------------------------------
SUBROUTINE readMcCARD(iobj, fn)

USE param, ONLY : ZERO
USE mdat,  ONLY : nMC, keff, nxy, nz, pow3d

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
INTEGER :: iMC, ixy, iz
REAL :: keffloc
! ------------------------------------------------

! READ : 1st
keff = ZERO
CALL readMcCARD1st(iobj, keffloc, fn)
keff = keff + keffloc

! READ : 2nd
DO iMC = 2, nMC(iobj)
  CALL readMcCARD2nd(iobj, keffloc, fn, iMC)
  keff = keff + keffloc
END DO

! Avg.
keff = keff / real(nMC(iobj))

DO iz = 1, nz
  DO ixy = 1, nxy(iobj)
    pow3d(ixy, iz, iobj) = pow3d(ixy, iz, iobj) / real(nMC(iobj))
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE readMcCARD
! --------------------------------------------------------------------------------------------------
SUBROUTINE readMcCARD1st(iobj, keffloc, fn)
! McCARD Outputs in use : 'ASY_F2F', 'KEFF', 'Asy. Power'
! ASSUME : 3-D Tallies are Arranged in the Radial Direction First

USE allocs
USE param, ONLY : TRUE, oneline, io1, EPS7, SQ3
USE mdat,  ONLY : nxy, nz, ndat, pow3d, aoF2F

IMPLICIT NONE

INTEGER :: iobj
REAL :: keffloc
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*100 :: gn

INTEGER :: ixy, iz, indev, idat
REAL :: rtmp
LOGICAL :: ifnumeric
! ------------------------------------------------

indev = io1
gn    = 'MC\' // trim(fn) // " F1.out"

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            READ : Basics
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 1000) oneline
  
  ! aoF2F
  IF (oneline(11:17) .EQ. 'ASY_PCH') THEN
    IF (.NOT. ifnumeric(oneline(18:))) CYCLE
    READ(oneline(18:), *) rtmp
    aoF2F(iobj) = rtmp*SQ3
  END IF
  IF (oneline(11:17) .EQ. 'ASY_F2F') THEN
    IF (.NOT. ifnumeric(oneline(18:))) CYCLE
    READ(oneline(18:), *) aoF2F(iobj)
  END IF
  
  ! k-eff
  IF (oneline(1:13) .EQ. 'Estimate Keff') THEN
    READ(oneline(16:), *) rtmp
    IF (rtmp .GT. EPS7) keffloc = rtmp ! Latest
  END IF
END DO

1000 CONTINUE

REWIND (indev)
! ------------------------------------------------
!            READ : # of Tallies
! ------------------------------------------------
ndat(iobj) = 0
DO
  READ (indev, '(A1000)', END = 2000) oneline
  IF (oneline(1:11) .EQ. '*Cell Name:') ndat(iobj) = ndat(iobj) + 1
END DO

2000 CONTINUE

IF (mod(ndat(iobj), nz) .NE. 0) CALL terminate("McCARD # OF 3-D TALLIES")
nxy(iobj) = ndat(iobj) / nz

REWIND (indev)
! ------------------------------------------------
!            READ : Power
! ------------------------------------------------
idat = 0
DO
  READ (indev, '(A1000)', END = 3000) oneline
  IF (oneline(1:11) .NE. '*Cell Name:') CYCLE
  
  idat = idat + 1
  iz   = (idat-1) / nxy(iobj) + 1
  ixy  = idat - nxy(iobj)*(iz-1)
  READ (oneline(87:98), *) pow3d(ixy, iz, iobj)
END DO

3000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readMcCARD1st
! --------------------------------------------------------------------------------------------------
SUBROUTINE readMcCARD2nd(iobj, keffloc, fn, iMC)
! McCARD Ouptuts in use : 'KEFF', 'Asy. Power'
! ASSUME : Same # of Tallies

USE allocs
USE param, ONLY : TRUE, oneline, io1, EPS7
USE mdat,  ONLY : nxy, nz, ndat, pow3d, aoF2F

IMPLICIT NONE

INTEGER :: iobj, iMC
REAL :: keffloc
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*1 :: dumc
CHARACTER*100 :: gn

INTEGER :: ixy, iz, indev, idat
REAL :: rtmp
! ------------------------------------------------

indev = io1
WRITE (dumc, '(I1)') iMC
gn    = 'MC\' // trim(fn) // " F" // dumc // ".out"

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            READ : keff
! ------------------------------------------------
DO
  READ (indev, '(A1000)', END = 1000) oneline
  IF (oneline(1:13) .NE. 'Estimate Keff') CYCLE
  
  READ(oneline(16:), *) rtmp
  IF (rtmp .GT. EPS7) keffloc = rtmp ! Latest
END DO

1000 CONTINUE

REWIND (indev)
! ------------------------------------------------
!            READ : Power
! ------------------------------------------------
idat = 0
DO
  READ (indev, '(A1000)', END = 3000) oneline
  IF (oneline(1:11) .NE. '*Cell Name:') CYCLE
  
  idat = idat + 1
  iz   = (idat-1) / nxy(iobj) + 1
  ixy  = idat - nxy(iobj)*(iz-1)
  READ (oneline(87:98), *) rtmp
  pow3d(ixy, iz, iobj) = pow3d(ixy, iz, iobj) + rtmp
END DO

3000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------

END SUBROUTINE readMcCARD2nd
! --------------------------------------------------------------------------------------------------
SUBROUTINE adjMC()

USE param, ONLY : ZERO
USE mdat,  ONLY : FNX, FNV, l02, objcn, nxa, nya, nsfc, izp, pow3d, powxy, powax, nxy, nz

IMPLICIT NONE

INTEGER :: ito, ifr, ixy, iz
! ------------------------------------------------

! LOGICAL
IF (l02) THEN
  IF (objcn(1).EQ.'MC' .AND. objcn(2).EQ.'MC') CALL terminate("McCARD ALONE")
  
  IF (objcn(1).EQ.'MC') THEN
    ito = 1
    ifr = 2
  ELSE
    ito = 2
    ifr = 1
  END IF
ELSE
  IF (objcn(1).EQ.'MC') CALL terminate("McCARD ALONE")
  RETURN
END IF

! Geo.
nya (ito) = nya (ifr)
nsfc(ito) = nsfc(ifr)
nxa(1:FNX, ito) = nxa(1:FNX, ifr)
izp(0:FNV, 1:FNX, ito) = izp(0:FNV, 1:FNX, ifr)

! 2-D, 1-D Power : will be Normalized later
powxy(:, ito) = ZERO
powax(:, ito) = ZERO
DO iz = 1, nz
  DO ixy = 1, nxy(ito)
    powxy(ixy, ito) = powxy(ixy, ito) + pow3d(ixy, iz, ito)
    powax(iz,  ito) = powax(iz,  ito) + pow3d(ixy, iz, ito)
  END DO
END DO
! ------------------------------------------------

END SUBROUTINE adjMC
! --------------------------------------------------------------------------------------------------