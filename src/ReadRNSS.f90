SUBROUTINE readRNSS(iobj, fn)

USE allocs
USE param, ONLY : TRUE, DALLR, DOT, BLANK, oneline, probe, io1, EPS7
USE mdat,  ONLY : nxa, nya, nxy, nz, ndat, izp, powxy, powax, pow3d, l3d, avghgt, hgt

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*100 :: gn

INTEGER :: Lgh, fndndata, mz, itmp, mya, mxy, mxy1, mxy2, iz, indev, ist, ied, idat, jdat
LOGICAL :: chknum
REAL :: totpow, rnrm
! ------------------------------------------------

indev = io1
gn    = 'RNSS\' // trim(fn) // '.out'
mya   = 0
mz    = 0

izp(:, :, iobj) = 0

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            01. READ : 1st
! ------------------------------------------------
DO
  READ (indev, '(A512)', END = 1000) oneline
  
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh = len_trim(oneline)
  
  SELECT CASE (oneline(6:Lgh))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    DO
      READ (indev, '(A512)', END = 1000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya = mya + 1
      
      nxa(mya, iobj) = fndndata(oneline) - 1
      
      ! izp
      DO idat = 11, len_trim(oneline)
        IF (oneline(idat:idat) .EQ. BLANK) CYCLE
        
        ist = idat
        
        DO jdat = ist+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .EQ. BLANK) EXIT
        END DO
        
        ied = jdat
        
        EXIT
      END DO
      
      DO idat = 2, nxa(mya, iobj)
        DO jdat = ied+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .NE. BLANK) EXIT
        END DO
        
        IF (jdat-ist .GT. 8) THEN
          izp(0, mya, iobj) = izp(0, mya, iobj) + 1
          izp(izp(0, mya, iobj), mya, iobj) = idat-1
        END IF
        
        ist = jdat
        
        DO jdat = ist+1, len_trim(oneline)
          IF (oneline(jdat:jdat) .EQ. BLANK) EXIT
        END DO
        
        ied = jdat
      END DO
    END DO
    
  CASE ('Axial 1-D Power Distribution')
    READ (indev, *)
    
    DO
      READ (indev, '(A512)', END = 1000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mz = mz + 1
      
      READ (oneline, *) itmp, powax(mz, iobj)
    END DO
    
    IF (mz .NE. nz) CALL terminate("RNSS # of PLANES")
  END SELECT
END DO

1000 CONTINUE

IF (mod(mya, 2) .NE. 1) CALL terminate("EVEN RNSS # of 2-D ASY. (y)")
IF (mod(nxa((mya+1)/2, iobj) + izp(0, mya+1, iobj), 2) .NE. 1) CALL terminate("EVEN MASTER # of 2-D ASY. (x)") ! Asymmetric

nya (iobj) = mya
nxy (iobj) = sum(nxa(1:mya, iobj))
ndat(iobj) = nxy(iobj) * nz

REWIND (indev)
! ------------------------------------------------
!            02. READ : 2nd
! ------------------------------------------------
DO
  READ (indev, '(A512)', END = 2000) oneline
  
  !IF (probe .EQ. DOT)   EXIT ! ECHO : inp
  IF (probe .NE. DALLR) CYCLE ! READ : Only Output Card
  
  Lgh = len_trim(oneline)
  
  SELECT CASE (oneline(6:Lgh))
  CASE ('Assembly Radial 2-D Power Distribution')
    READ (indev, *)
    READ (indev, *)
    READ (indev, *)
    
    mya  = 0
    mxy1 = 1
    
    DO
      READ (indev, '(A512)', END = 2000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      mya  = mya + 1
      mxy2 = mxy1 + nxa(mya, iobj) - 1
      
      READ (oneline, *) itmp, powxy(mxy1:mxy2, iobj)
      
      mxy1 = mxy2 + 1
    END DO
    
  CASE ('Power Distribution in All Region')
    DO
      READ (indev, '(A512)') oneline
      
      IF (.NOT. chknum(oneline)) CYCLE
      
      BACKSPACE (indev)
      EXIT
    END DO
    
    DO
      READ (indev, '(A512)', END = 2000) oneline
      
      IF (fndndata(oneline) .EQ. 0) EXIT
      
      READ (oneline, *) mxy, pow3d(mxy, 1:nz, iobj)
    END DO
  END SELECT
END DO

2000 CONTINUE

CLOSE (indev) ! 1
! ------------------------------------------------
!            03. NORM
! ------------------------------------------------
! 3D
totpow = sum(pow3d(:, :, iobj))

IF (totpow .LT. EPS7) THEN
  pow3d(:, 1, iobj) = powxy(:, iobj)
  totpow = sum(pow3d(:, :, iobj))
END IF

rnrm = real(ndat(iobj)) / totpow

IF (.NOT. l3d) THEN
  pow3d(:, :, iobj) = pow3d(:, :, iobj) * rnrm
ELSE
  DO iz = 1, nz
    pow3d(:, iz, iobj) = pow3d(:, iz, iobj) * rnrm * avghgt / hgt(iz) ! Volume-wise Power
  END DO
END IF

! 2D
totpow = sum(powxy(:, iobj))

rnrm = real(nxy(iobj)) / totpow

powxy(:, iobj) = powxy(:, iobj) * rnrm

! 1D : Already Normalized
DO iz = 1, nz
  powax(iz, iobj) = powax(iz, iobj) * hgt(iz) ! Point-wise to Volume-wise
END DO

totpow = sum(powax(:, iobj))

rnrm = real(nz) / totpow

DO iz = 1, nz
  powax(iz, iobj) = powax(iz, iobj) * rnrm * avghgt / hgt(iz) ! Volume-wise to Point-wise
END DO
! ------------------------------------------------

END SUBROUTINE readRNSS